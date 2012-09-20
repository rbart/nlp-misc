package edu.washington.cs.knowitall.browser.hadoop.scoobi

import scopt.OptionParser
import util.ExtractionSentenceRecord
import com.nicta.scoobi.Scoobi._
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer
import edu.washington.cs.knowitall.tool.postag.PostaggedToken
import RelationCounter.stemmer
import RelationCounter.filterTokens
import scala.Option.option2Iterable

import scala.collection.mutable

import edu.washington.cs.knowitall.nlp.util.ArgContext
import edu.washington.cs.knowitall.nlp.util.ArgContext.joinTokensAndPostags

import edu.washington.cs.knowitall.nlp.util.RelTupleProcessor._
import RelationCounter._

object RelTupleTabulator extends ScoobiApp {
  
  case class TypeContext(
      val arg1Type: String,
      val rel: String,
      val arg2Type: String,
      val freq: Int,
      val arg1s: Seq[(String, Int)],
      val arg2s: Seq[(String, Int)]) {
    
    // need a toString
    override def toString = {
      Seq(
        freq,
        rel,
        arg1Type,
        arg2Type,
        arg1s.mkString(" | "),
        arg2s.mkString(" | ")
      ).mkString("\t")
    }
  }
  
  def buildTypeContext(rel: String, relContexts: Iterable[ArgContext]): Iterable[TypeContext] = {
    
    // we need to count the frequency of:
    // -- (arg1Type, arg2Type) pairs,
    val pairCounts = new mutable.HashMap[(String, String), MutInt]
    val argCounts = new mutable.HashMap[(String, String), (mutable.Map[String, MutInt], mutable.Map[String, MutInt])]
    
    def toTypePair(context: ArgContext): (String, String) = (getType(context.arg1), getType(context.arg2))
    
    def toArgString(tokens: Seq[PostaggedToken]) = tokens.map(_.string).mkString(" ")
    
    relContexts.iterator.take(1000000).foreach { context =>
      val typePair = toTypePair(context)
      pairCounts.getOrElseUpdate(typePair, MutInt.zero).inc
      val argCount = argCounts.getOrElseUpdate(typePair, {
        (new mutable.HashMap[String, MutInt], new mutable.HashMap[String, MutInt])
      })
      argCount._1.getOrElseUpdate(toArgString(context.arg1), MutInt.zero).inc
      argCount._2.getOrElseUpdate(toArgString(context.arg2), MutInt.zero).inc
    }
    
    // now, for each typed pair, we need to construct a TypeContext based on the data in the mutable maps.
    // here is a helper method to extract the top args from an arg count map:
    def topArgs(countMap: mutable.Map[String, MutInt]): Seq[(String, Int)] = {
      countMap.iterator.map({ case (arg, count) => (arg, count.count.toInt) }).filter(_._2 > 1).toSeq.sortBy(-_._2)
    }
    
    pairCounts.iterator.filter(_._2.count > 1).map({ case ((arg1Type, arg2Type), freq) => 
      val (arg1Counts, arg2Counts) = argCounts(arg1Type, arg2Type) 
      val (topArg1s, topArg2s) = (topArgs(arg1Counts), topArgs(arg2Counts))
      TypeContext(arg1Type, rel, arg2Type, freq.count.toInt, topArg1s.take(6), topArg2s.take(6))
    }).toIterable
  }
  
  def run(): Unit = {

    var inputPath = ""
    var outputPath = ""
    var minFrequency = 0
    var maxFrequency = Integer.MAX_VALUE

    val parser = new OptionParser() {
      arg("inputPath", "file input path, records delimited by newlines", { str => inputPath = str })
      arg("outputPath", "file output path, newlines again", { str => outputPath = str })
      intOpt("minFreq", "don't keep tuples below this frequency", { num => minFrequency = num })
      intOpt("maxFreq", "don't keep tuples above this frequency", { num => maxFrequency = num })
    }

    if (!parser.parse(args)) return
 
    println("Parsed args: %s".format(args.mkString(" ")))
    
    val input: DList[String] = fromTextFile(inputPath)
    
    val tuples = input flatMap toTuple
    
    val grouped = tuples.groupByKey
    
//    val freqFilteredRels = grouped.flatMap { case (rel, argContexts) =>
//      val size = argContexts.size
//      if (size >= minFrequency && size <= maxFrequency) Some((rel, "")) else None
//    }
    
    //val filteredRelContexts = freqFilteredRels.join(grouped)
    
    
    // take rel, Iterable[ArgContext] and traverse the iterable to find most common (arg1Type, arg2Type) pairs,
    // as well as the most common arg1/arg2 strings for each (arg1Type, arg2Type) pair.
    val typeContexts = grouped.flatMap { case (rel, contexts) =>
      buildTypeContext(rel, contexts flatMap ArgContext.fromString).take(15)
    }
    
    persist(toTextFile(typeContexts.map(_.toString), outputPath + "/"))
  }
  
  def stemToken(token: PostaggedToken): PostaggedToken = {
    val lemma = stemmer.stemToken(token)
    new PostaggedToken(lemma.token.postag, lemma.lemma, 0)
  }
  
  // (rel tokens, (rel.toString, arg1String, arg2String))
  def toTuple(inputRecord: String): Option[(String, String)] = {
    try { 
      val esr = new ExtractionSentenceRecord(inputRecord)
      val relTokens = joinTokensAndPostags(esr.norm1Rel, esr.norm1RelPosTags) filter filterTokens map stemToken
      val relString = relTokens.map(_.string).mkString(" ")
      if (relTokens.isEmpty || relString.startsWith("'")) None 
      else {
        val arg1Tokens = joinTokensAndPostags(esr.norm1Arg1, esr.norm1Arg1PosTags) filter filterTokens map stemToken
        val arg2Tokens = joinTokensAndPostags(esr.norm1Arg2, esr.norm1Arg2PosTags) filter filterTokens map stemToken
        Some(relString, ArgContext(arg1Tokens, arg2Tokens).toString)
      }
    } 
    catch { case e: Exception => { e.printStackTrace; None } }
  }
}