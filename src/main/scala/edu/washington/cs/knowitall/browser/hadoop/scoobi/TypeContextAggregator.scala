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
import edu.washington.cs.knowitall.nlp.ChunkedSentence
import RelationCounter._
import edu.washington.cs.knowitall.tool.postag.OpenNlpPostagger
import edu.washington.cs.knowitall.nlp.util.ArgContext
import edu.washington.cs.knowitall.nlp.util.TypeContext

import edu.washington.cs.knowitall.nlp.util.DataExplorerTool.kMostFrequent
import edu.washington.cs.knowitall.nlp.util.relontology.Nym

object TypeContextAggregator extends ScoobiApp {

  private val maxTcPerRel = 20;
  
  def postagger = postaggerLocal.get
  private val postaggerLocal = new ThreadLocal[OpenNlpPostagger] { override def initialValue = new OpenNlpPostagger }
  
  def buildTypeContext(rel: String, relContexts: Iterator[ArgContext]): Iterable[TypeContext] = {

    // we need to count the frequency of:
    // -- (arg1Type, arg2Type) pairs,
    // given a type pair, the frequency of arg1s, arg2s, and (arg1, arg2)'s 
    // while only using the iterator once, in case this is being run in hadoop.
    // hence all of the mutation.
    val typePairCounts = new mutable.ArrayBuffer[(String, String)]
    val arg1Counts = new mutable.HashMap[(String, String), mutable.ArrayBuffer[String]]
    val arg2Counts = new mutable.HashMap[(String, String), mutable.ArrayBuffer[String]]
    val argPairCounts = new mutable.HashMap[(String, String), mutable.ArrayBuffer[(String, String)]]

    def toTypePair(arg1: Seq[PostaggedToken], arg2: Seq[PostaggedToken], sentence: ChunkedSentence): (String, String) = { 
      val typePair = (getType(arg1), getType(arg2))

      // commented out due to problems with NER
      //val left = if (typePair._1.equals("other_noun")) StanfordNerHelper.getWnTag(arg1, sentence) else typePair._1
      //val right = if (typePair._2.equals("other_noun")) StanfordNerHelper.getWnTag(arg2, sentence) else typePair._2
      //(left, right)
      typePair
    }
    
    def toArgString(tokens: Seq[PostaggedToken]) = tokens.map(_.string).mkString(" ")

    relContexts.foreach { context =>
      val typePair = toTypePair(context.arg1, context.arg2, context.sent)
      typePairCounts.append(typePair)
      val arg1String = toArgString(context.arg1)
      val arg2String = toArgString(context.arg2)
      val argPair = (arg1String, arg2String)
      arg1Counts.getOrElseUpdate(typePair, new mutable.ArrayBuffer[String]).append(arg1String)
      arg2Counts.getOrElseUpdate(typePair, new mutable.ArrayBuffer[String]).append(arg2String)
      argPairCounts.getOrElseUpdate(typePair, new mutable.ArrayBuffer[(String, String)]).append(argPair)
    }

    kMostFrequent(typePairCounts, maxTcPerRel).map({
      case ((arg1Type, arg2Type), freq) =>
        val topArg1s = kMostFrequent(arg1Counts(arg1Type, arg2Type), 6)
        val topArg2s = kMostFrequent(arg2Counts(arg1Type, arg2Type), 6)
        val topArgPairs = kMostFrequent(argPairCounts(arg1Type, arg2Type), 10000)
        TypeContext(arg1Type, new Nym("tca:") { def rel = rel }, arg2Type, freq, topArg1s, topArg2s, topArgPairs)
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
        typeContexts.take(15).map { context => "%s\t%s".format(context.freq, context.toString) }
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
      Some(relString, ArgContext(arg1Tokens, arg2Tokens, sent).toString)
    }
   
  }
}