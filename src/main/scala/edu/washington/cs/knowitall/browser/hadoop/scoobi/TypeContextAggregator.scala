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
import edu.washington.cs.knowitall.nlp.util.RelTupleProcessor.getType
import edu.washington.cs.knowitall.nlp.util.WordNetHelper._
import edu.washington.cs.knowitall.nlp.util.StanfordNerHelper
import RelationCounter._
import edu.washington.cs.knowitall.tool.postag.OpenNlpPostagger
import edu.washington.cs.knowitall.nlp.util.ArgContext
import edu.washington.cs.knowitall.nlp.util.TypeContext



object TypeContextAggregator extends ScoobiApp {

  def postagger = postaggerLocal.get
  private val postaggerLocal = new ThreadLocal[OpenNlpPostagger] { override def initialValue = new OpenNlpPostagger }
  
  def buildTypeContext(rel: String, relContexts: Iterator[ArgContext]): Iterable[TypeContext] = {

    // we need to count the frequency of:
    // -- (arg1Type, arg2Type) pairs,
    val pairCounts = new mutable.HashMap[(String, String), MutInt]
    val argCounts = new mutable.HashMap[(String, String), (mutable.Map[String, MutInt], mutable.Map[String, MutInt])] 

    def toTypePair(context: ArgContext): (String, String) = { 
      val typePair = (getType(context.arg1), getType(context.arg2))
      // if wn returned "other_noun", try to do an NER lookup
      // This is commented for testing purposes, remove it!
      val left = if (typePair._1.equals("other_noun")) StanfordNerHelper.getWnTag(context.esr.arg1, context.esr) else typePair._1
      val right = if (typePair._2.equals("other_noun")) StanfordNerHelper.getWnTag(context.esr.arg2, context.esr) else typePair._2
      (left, right)
    }
    
    def toArgString(tokens: Seq[PostaggedToken]) = tokens.map(_.string).mkString(" ")

    relContexts.foreach { context =>
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
      countMap.iterator.map({ case (arg, count) => (arg, count.count.toInt) }).toSeq.sortBy(-_._2)
    }

    pairCounts.iterator.map({
      case ((arg1Type, arg2Type), freq) =>
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
    var sampleFrac = 1.0

    val parser = new OptionParser() {
      arg("inputPath", "file input path, records delimited by newlines", { str => inputPath = str })
      arg("outputPath", "file output path, newlines again", { str => outputPath = str })
      intOpt("minFreq", "don't keep tuples below this frequency", { num => minFrequency = num })
      intOpt("maxFreq", "don't keep tuples above this frequency", { num => maxFrequency = num })
      doubleOpt("sample", "sample the input at this freq", { num => sampleFrac = num })
    }

    if (!parser.parse(args)) return

    println("Parsed args: %s".format(args.mkString(" ")))

    val input: DList[String] = fromTextFile(inputPath)
    
    val rawEsrs = input.flatMap { line =>
      if (scala.util.Random.nextDouble <= sampleFrac) Some(line) else None  
    }

    val tuples = rawEsrs flatMap toTuple

    val grouped = tuples.groupByKey

    val groupSizes = grouped.flatMap { case (rel, argContexts) => 
      val limitedContexts = new mutable.MutableList[String]
      var size = 0
      for (context <- argContexts) {
        size += 1
        if (size <= 100000) limitedContexts += context
      }
        
      if (size >= minFrequency && size <= maxFrequency) Some((rel, (size.toString, limitedContexts.toIterable))) else None
    }

    // take rel, Iterable[ArgContext] and traverse the iterable to find most common (arg1Type, arg2Type) pairs,
    // as well as the most common arg1/arg2 strings for each (arg1Type, arg2Type) pair.
    val freqTypeContextsRaw = groupSizes.flatMap {
      case (rel, (size, contexts)) =>
        val typeContexts = buildTypeContext(rel, contexts.iterator.take(250000).flatMap(ArgContext.fromString _)).toSeq.sortBy(-_.freq)
        typeContexts.take(15).map { context => "%s\t%s".format(size, context.toString) }
    }
    
    val postagged = RelPostagFinder.go(freqTypeContextsRaw, rawEsrs)
    

    persist(toTextFile(postagged, outputPath + "/"))
  }

  def stemToken(token: PostaggedToken): PostaggedToken = {
    val lemma = stemmer.stemToken(token)
    new PostaggedToken(lemma.token.postag, lemma.lemma, 0)
  }

  def toEsr(inputRecord: String) = {
    try {
      Some(new ExtractionSentenceRecord(inputRecord))
    } catch {
      case e => {
        e.printStackTrace
        None
      }
    }
  }
   
  // (rel tokens, (rel.toString, arg1String, arg2String))
  def toTuple(inputRecord: String): Option[(String, String)] = {
  
    val esr = toEsr(inputRecord).getOrElse { return None }
    
    val relTokens = joinTokensAndPostags(esr.norm1Rel.toLowerCase, esr.norm1RelPosTags) filter filterTokens map stemToken
    val relString = relTokens.map(_.string).mkString(" ")
    if (relTokens.isEmpty || relString.startsWith("'")) None
    else {
      val arg1Tokens = joinTokensAndPostags(esr.norm1Arg1, esr.norm1Arg1PosTags) filter filterTokens
      val arg2Tokens = joinTokensAndPostags(esr.norm1Arg2, esr.norm1Arg2PosTags) filter filterTokens
      val sent = StanfordNerHelper.getChunkedSentence(esr).getOrElse { return None }
      Some(relString, ArgContext(arg1Tokens, arg2Tokens, esr).toString)
    }
   
  }
}