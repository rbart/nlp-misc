package edu.washington.cs.knowitall.nlp.util

import edu.washington.cs.knowitall.browser.lucene.ParallelExtractionGroupFetcher
import edu.washington.cs.knowitall.browser.extraction._
import edu.washington.cs.knowitall.nlp.ChunkedSentence
import edu.washington.cs.knowitall.commonlib.Range
import edu.washington.cs.knowitall.tool.chunk.ChunkedToken
import scala.collection.JavaConversions._
import scopt.OptionParser

import edu.washington.cs.knowitall.nlp.util.relontology.Candidate
import edu.washington.cs.knowitall.nlp.util.relontology.Nym
import edu.washington.cs.knowitall.nlp.util.relontology.WordNetClass

// Implements Stephen's "Interface A"
class DataExplorerTool(val pegf: ParallelExtractionGroupFetcher) {

  import DataExplorerTool.kMostFrequent
  
  val maxInstances = 10
  
  // used when creating type contexts
  val argSampleSize = 5
  val pairSampleSize = 10000
  
  type REG = ExtractionGroup[ReVerbExtraction]

  class TypedReg(val arg1Type: String, val arg2Type: String, val reg: REG)
  
  object TypedReg {
    
    def chunkedSentence(tokens: Seq[ChunkedToken]): ChunkedSentence = {
      new ChunkedSentence(tokens.map(_.string), tokens.map(_.postag), tokens.map(_.chunk))
    }
    
    def chunkedSentences(reg: REG): Iterable[ChunkedSentence] = {
      reg.instances.take(maxInstances).map(_.extraction.sentenceTokens) map chunkedSentence
    }
    
    def fromReg(reg: REG): TypedReg = {
      
      val instances = reg.instances.take(maxInstances)
      // how do I construct a TypedReg given a regular reg? (sic)
      // just need to find out what the type is for each argument.
      // Since NER might fire for some instances and not others, we 
      // try tagging several instances and looking for types.
      // try using both normalized and surface forms (e.g. hence arg1Types{1/2})
      //val arg1Types1 = instances.toIterable.map(i => StanfordNerHelper.getWnTag(i.extraction.arg1Text, chunkedSentence(i.extraction.sentenceTokens)))
      //val arg1Types2 = chunkedSentences(reg).map(s => StanfordNerHelper.getWnTag(reg.arg1.norm, s))
      //val arg1Types = arg1Types1 ++ arg1Types2;
      val arg1Types = instances.toIterable.map(i => RelTupleProcessor.getType(i.extraction.arg1Tokens)) ++ instances.toIterable.map(i => RelTupleProcessor.getType(i.extraction.normTokens(i.extraction.arg1Interval)))
      val arg1Type = mostFrequent(arg1Types.filterNot(s => s equals "other_noun")).map(_._1).getOrElse("other_noun");
      
      //val arg2Types1 = instances.toIterable.map(i => StanfordNerHelper.getWnTag(i.extraction.arg2Text, chunkedSentence(i.extraction.sentenceTokens)))
      //val arg2Types2 = chunkedSentences(reg).map(s => StanfordNerHelper.getWnTag(reg.arg2.norm, s))
      //val arg2Types = arg2Types1 ++ arg2Types2;
      val arg2Types = instances.toIterable.map(i => RelTupleProcessor.getType(i.extraction.arg2Tokens)) ++ instances.toIterable.map(i => RelTupleProcessor.getType(i.extraction.normTokens(i.extraction.arg2Interval)))
      val arg2Type = mostFrequent(arg2Types.filterNot(s => s equals "other_noun")).map(_._1).getOrElse("other_noun");
            
      new TypedReg(arg1Type, arg2Type, reg) // done
    }
  }
  
  def simpleQuery(arg1Type: String, relString: String, arg2Type: String): Iterable[TypedReg] = {
    
    val tupleString = "(%s)".format(arg1Type, relString, arg2Type)
    
    val typedRegs = findTypedRegs(relString)  
    System.err.println("%s typedRegs results size: %d".format(tupleString, typedRegs.size))
    val finalResults = typedRegs filter filterMatchingTypes(arg1Type, arg2Type)
    System.err.println("%s finalResults results size: %d".format(tupleString, finalResults.size))
    finalResults
  }
  
  def relQueryBackend(relString: String): Iterable[REG] = {
    pegf.getGroups(None, Some(relString), None, false).results
  }
  
  def findTypedRegs(relString: String): Iterable[TypedReg] = {
    val tupleString = "(%s)".format(relString)
    val backendResults = relQueryBackend(relString) 
    System.err.println("%s backend results size: %d".format(tupleString, backendResults.size))
    val exactFilter = backendResults filter filterExactRel(relString) 
    System.err.println("%s exactFilter results size: %d".format(tupleString, exactFilter.size))
    val typedRegs = exactFilter map TypedReg.fromReg 
    typedRegs
  }
  
  def filterExactRel(relString: String)(reg: REG): Boolean = reg.rel.norm.equalsIgnoreCase(relString)
  
  def filterMatchingTypes(arg1Type: String, arg2Type: String)(typedReg: TypedReg): Boolean = {
    typedReg.arg1Type.equals(arg1Type) && typedReg.arg2Type.equals(arg2Type)
  }
 
  def mostFrequent[T](items: Iterable[T]): Option[(T, Int)] = {
    kMostFrequent(items, 1).headOption
  }
  
  def findTypeContexts(relNym: Nym): Iterable[TypeContext] = {
    
    val typedRegs = findTypedRegs(relNym.rel)
    // group by (arg1Type, arg2Type)
    val groups = typedRegs.groupBy { typedReg => (typedReg.arg1Type, typedReg.arg2Type) }
    
    // convert these groups into type contexts...
    val typeContexts = groups.iterator.map { case ((arg1Type, arg2Type), typedRegs) =>
      val arg1Samples = kMostFrequent(typedRegs.map(_.reg.arg1.norm), argSampleSize)
      val arg2Samples = kMostFrequent(typedRegs.map(_.reg.arg2.norm), argSampleSize)
      val argPairSamples = kMostFrequent(typedRegs.map(treg => (treg.reg.arg1.norm, treg.reg.arg2.norm)), pairSampleSize)
      
      TypeContext(arg1Type, relNym, arg2Type, typedRegs.size, arg1Samples, arg2Samples, argPairSamples)
    }
    
    typeContexts.toIterable
  }
  
  def findCandidates(parent: TypeContext, candidateNym: Nym, candidateTypeContexts: Iterable[TypeContext]): Iterable[Candidate] = {
    
    // compute a candidate for each of these typecontexts.
    candidateTypeContexts.map { tc =>
      
      val grouped = (parent.argPairs ++ tc.argPairs).groupBy(_._1).filter(_._2.size >= 2)
      val intersection = grouped.iterator.map { case ((arg1, arg2), counts) => ((arg1, arg2), counts.map(_._2).sum) }
      val overlap = intersection.map(_._2).sum
      
      val pmi = overlap.toDouble / (parent.freq + tc.freq)
      val condProb = overlap.toDouble / tc.freq
      val balanced = math.sqrt(pmi * condProb)
      
      val arg1Class = WordNetClass.fromString(tc.arg1Type)
      val arg2Class = WordNetClass.fromString(tc.arg2Type)
      
      new Candidate(arg1Class, candidateNym , arg2Class, parent, tc.freq, overlap, pmi, condProb, balanced)
    }
  }
}

object DataExplorerTool {
  
  lazy val defaultTool = {
    
    val searchMaxGroups = 20000
    val readMaxInstances = 200000
    val timeoutMillis = 20000
    
    val pegf = new ParallelExtractionGroupFetcher(ParallelExtractionGroupFetcher.defaultIndexes.split(":"), searchMaxGroups, readMaxInstances, timeoutMillis)
    
    val tool = new DataExplorerTool(pegf)
    tool
  }
  
  // for debugging...
  lazy val fastTool = {
    
    val searchMaxGroups = 1000
    val readMaxInstances = 10000
    val timeoutMillis = 500
    
    val pegf = new ParallelExtractionGroupFetcher(ParallelExtractionGroupFetcher.defaultIndexes.split(":"), searchMaxGroups, readMaxInstances, timeoutMillis)
    
    val tool = new DataExplorerTool(pegf)
    tool
  }
  
  def main(args: Array[String]): Unit = {
    
    var arg1Type = ""
    var arg2Type = ""
    var relString1 = ""
    var relString2 = ""
    
    // take a query from the user 
    val parser = new OptionParser("DataExplorerTool") {
      arg("arg1Type", "arg1Type", { str => arg1Type = str})
      arg("arg2Type", "arg2Type", { str => arg2Type = str })
      arg("relString1", "relString1", { str => relString1 = str })
      arg("relString2", "relString2", { str => relString2 = str })
    }
    
    if (!parser.parse(args)) return

    val tregs1 = defaultTool.simpleQuery(arg1Type, relString1, arg2Type)
    val tregs2 = defaultTool.simpleQuery(arg1Type, relString2, arg2Type)
    
    val freq1 = tregs1.size
    val freq2 = tregs2.size
    
    val grouped = (tregs1 ++ tregs2).groupBy(treg => (treg.reg.arg1.norm, treg.reg.arg2.norm))
    val intersection = grouped.filter { case ((arg1Norm, arg2Norm), regs) => regs.size >= 2 }
    
    val overlap = intersection.size;
    
    val pmi = overlap.toDouble / (freq1 + freq2)
    val condProb = overlap.toDouble / freq2
    val balanced = math.sqrt(pmi * condProb)
    
    println("Freq t1: %d".format(freq1))
    println("Freq t2: %d".format(freq2))
    println("Freq overlap: %d".format(overlap))
    println("PMI: %.02f".format(pmi))
    println("P(t1|t2): %.02f".format(condProb))
    println("Balanced: %.02f".format(balanced))
  }
  
  def kMostFrequent[T](items: Iterable[T], k: Int): Seq[(T, Int)] = {
    items.groupBy(identity).iterator.map({ case (item, items) => (item, items.size) }).toSeq.sortBy(-_._2).take(k)
  }
}




























