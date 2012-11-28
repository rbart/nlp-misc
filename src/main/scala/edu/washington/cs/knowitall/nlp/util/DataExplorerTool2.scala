package edu.washington.cs.knowitall.nlp.util

import edu.washington.cs.knowitall.browser.util.SortedFileMap
import edu.washington.cs.knowitall.browser.extraction._
import edu.washington.cs.knowitall.nlp.ChunkedSentence
import edu.washington.cs.knowitall.commonlib.Range
import edu.washington.cs.knowitall.tool.chunk.ChunkedToken
import scala.collection.JavaConversions._
import scopt.OptionParser

import edu.washington.cs.knowitall.nlp.util.relontology.Candidate
import edu.washington.cs.knowitall.nlp.util.relontology.Nym
import edu.washington.cs.knowitall.nlp.util.relontology.WordNetClass

import edu.washington.cs.knowitall.browser.hadoop.scoobi.TypeContextAggregator.postagger


// Implements Stephen's "Interface A"
class DataExplorerTool2(val relArgPairMap: SortedFileMap) {

  import DataExplorerTool.kMostFrequent
  
  // used when creating type contexts
  val argSampleSize = 5
  val pairSampleSize = 10000
  
  type REG = ExtractionGroup[ReVerbExtraction]

  case class ArgPair(val arg1: String, val arg2: String)
  object ArgPair {
    def fromString(line: String): Option[ArgPair] = {
      line.split("\\+") match {
        case Array(arg1, arg2, _*) => Some(ArgPair(arg1, arg2))
        case _ => { System.err.println("ArgPair parse failure: %s".format(line)); None }
      }
    }
  }
 
  case class TypedArgPair(val arg1Type: String, val arg2Type: String, val argPair: ArgPair)
  object TypedArgPair {
    def fromArgPair(argPair: ArgPair): TypedArgPair = {
      val arg1Tokens = postagger.postag(argPair.arg1)
      val arg2Tokens = postagger.postag(argPair.arg2)
      val arg1Type = RelTupleProcessor.getType(arg1Tokens)
      val arg2Type = RelTupleProcessor.getType(arg2Tokens)
      TypedArgPair(arg1Type, arg2Type, argPair)
    }
  }
  
  def mostFrequent[T](items: Iterable[T]): Option[(T, Int)] = {
    kMostFrequent(items, 1).headOption
  }
  
  def findTypeContexts(relNym: Nym): Iterable[TypeContext] = {
    
    val argPairs = relArgPairMap.get(relNym.rel).split("\t").flatMap(ArgPair.fromString _);
    val typedArgPairs = argPairs map TypedArgPair.fromArgPair
    // group by (arg1Type, arg2Type)
    val groups = typedArgPairs.groupBy(tap => (tap.arg1Type, tap.arg2Type))
    
    // convert these groups into type contexts...
    val typeContexts = groups.iterator.map { case ((arg1Type, arg2Type), typedArgPairs) =>
      val arg1Samples = kMostFrequent(typedArgPairs.map(_.argPair.arg1), argSampleSize)
      val arg2Samples = kMostFrequent(typedArgPairs.map(_.argPair.arg2), argSampleSize)
      val argPairSamples = kMostFrequent(typedArgPairs.map(tap => (tap.argPair.arg1, tap.argPair.arg1)), pairSampleSize)
      
      TypeContext(arg1Type, relNym, arg2Type, typedArgPairs.size, arg1Samples, arg2Samples, argPairSamples)
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




























