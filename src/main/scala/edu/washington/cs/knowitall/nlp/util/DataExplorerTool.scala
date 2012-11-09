package edu.washington.cs.knowitall.nlp.util

import edu.washington.cs.knowitall.browser.lucene.ParallelExtractionGroupFetcher
import edu.washington.cs.knowitall.browser.extraction._
import edu.washington.cs.knowitall.nlp.ChunkedSentence
import edu.washington.cs.knowitall.commonlib.Range
import edu.washington.cs.knowitall.tool.chunk.ChunkedToken
import scala.collection.JavaConversions._
import scopt.OptionParser

class DataExplorerTool(val pegf: ParallelExtractionGroupFetcher) {

  val maxInstances = 10
  
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
      val arg1Types = instances.toIterable.map(i => RelTupleProcessor.getType(i.extraction.arg1Tokens))
      val arg1Type = mostFrequent(arg1Types.filterNot(s => s equals "other_noun")).getOrElse("other_noun");
      
      //val arg2Types1 = instances.toIterable.map(i => StanfordNerHelper.getWnTag(i.extraction.arg2Text, chunkedSentence(i.extraction.sentenceTokens)))
      //val arg2Types2 = chunkedSentences(reg).map(s => StanfordNerHelper.getWnTag(reg.arg2.norm, s))
      //val arg2Types = arg2Types1 ++ arg2Types2;
      val arg2Types = instances.toIterable.map(i => RelTupleProcessor.getType(i.extraction.arg2Tokens))
      val arg2Type = mostFrequent(arg2Types.filterNot(s => s equals "other_noun")).getOrElse("other_noun");
            
      new TypedReg(arg1Type, arg2Type, reg) // done
    }
  }
  
  def simpleQuery(arg1Type: String, relString: String, arg2Type: String): Iterable[TypedReg] = {
    val backendResults = relQueryBackend(relString) 
    println("backend results size: %d".format(backendResults.size))
    val exactFilter = backendResults filter filterExactRel(relString) 
    println("exactFilter results size: %d".format(exactFilter.size))
    val typedRegs = exactFilter map TypedReg.fromReg 
    println("typedRegs results size: %d".format(typedRegs.size))
    val finalResults = typedRegs filter filterMatchingTypes(arg1Type, arg2Type)
    println("finalResults results size: %d".format(finalResults.size))
    finalResults
  }
  
  def relQueryBackend(relString: String): Iterable[REG] = {
    pegf.getGroups(None, Some(relString), None, false).results
  }
  
  def filterExactRel(relString: String)(reg: REG): Boolean = reg.rel.norm.equalsIgnoreCase(relString)
  
  def filterMatchingTypes(arg1Type: String, arg2Type: String)(typedReg: TypedReg): Boolean = {
    typedReg.arg1Type.equals(arg1Type) && typedReg.arg2Type.equals(arg2Type)
  }
  
  // utility belt function:
  def mostFrequent[T](items: Iterable[T]): Option[T] = {
    items.groupBy(identity).iterator.map({ case (item, items) => (item, items.size) }).toSeq.sortBy(-_._2).map(_._1).headOption
  }
}

object DataExplorerTool {
  
  
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
    
    // open a pegf!!!
    val searchMaxGroups = 20000
    val readMaxInstances = 40000
    val timeoutMillis = 20000
    
    val pegf = new ParallelExtractionGroupFetcher(ParallelExtractionGroupFetcher.defaultIndexes.split(":"), searchMaxGroups, readMaxInstances, timeoutMillis)
    
    val tool = new DataExplorerTool(pegf)
    
    val tregs1 = tool.simpleQuery(arg1Type, relString1, arg2Type)
    val tregs2 = tool.simpleQuery(arg1Type, relString2, arg2Type)
    
    val freq1 = tregs1.size
    val freq2 = tregs2.size
    
    val grouped = (tregs1 ++ tregs2).groupBy(treg => (treg.reg.arg1.norm, treg.reg.arg2.norm))
    val intersection = grouped.filter { case ((arg1Norm, arg2Norm), regs) => regs.size >= 2 }
    
    val overlap = intersection.size;
    
    val pmi = overlap.toDouble / (freq1 + freq2)
    val condProb = overlap / freq2
    val balanced = math.sqrt(pmi * condProb)
    
    printf("Freq t1: %d".format(freq1))
    printf("Freq t2: %d".format(freq2))
    printf("Freq overlap: %d".format(overlap))
    printf("PMI: %.02f".format(pmi))
    printf("P(t1|t2): %.02f".format(condProb))
    printf("Balanced: %.02f".format(balanced))
  }
}




























