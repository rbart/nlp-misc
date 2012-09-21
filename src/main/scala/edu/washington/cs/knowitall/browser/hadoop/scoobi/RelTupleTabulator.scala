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
import edu.washington.cs.knowitall.nlp.util.RelationTabulator._
import RelationCounter._

import edu.washington.cs.knowitall.tool.postag.OpenNlpPostagger

case class TypeContext(
  val arg1Type: String,
  val rel: String,
  val arg2Type: String,
  val freq: Int,
  val arg1s: Seq[(String, Int)],
  val arg2s: Seq[(String, Int)]) {

  import RelTupleTabulator.postagger
  
  private def formatArgs(args: Seq[(String, Int)]): String = args.map({ case (arg, freq) => "%s(%d)".format(arg, freq) }).mkString(" | ")
  
  def arg1sFormatted = formatArgs(arg1s)
  def arg2sFormatted = formatArgs(arg2s)
  
  // need a toString
  override def toString = {
    Seq(
      freq.toString,
      rel,
      arg1Type,
      arg2Type,
      arg1sFormatted,
      arg2sFormatted).mkString("\t")
  }
}

object TypeContext {

  private def parseArgFreq(string: String): Option[(String, Int)] = try {
    val trimmed = string.dropRight(1)
    val breakIndex = trimmed.lastIndexWhere(_ == '(')
    val arg = trimmed.take(breakIndex)
    val freq = trimmed.drop(breakIndex+1).toInt
    Some((arg, freq))
  } catch {
    case e => { e.printStackTrace(); System.err.println("ParseArgFreq error: %s".format(string)); None }
  }

  def fromString(string: String): Option[TypeContext] = string.split("\t") match {
    case Array(freq, rel, arg1Type, arg2Type, arg1Strings, arg2Strings, _*) => {
      try {
        val arg1s = arg1Strings.split(" \\| ").flatMap(parseArgFreq _)
        val arg2s = arg2Strings.split(" \\| ").flatMap(parseArgFreq _)
        Some(TypeContext(arg1Type, rel, arg2Type, freq.toInt, arg1s, arg2s))
      } catch {
        case e => { e.printStackTrace(); System.err.println("TypeContext couldn't parse: %s".format(string)); None }
      }
    }
    case _ => { System.err.println("TypeContext couldn't parse: %s".format(string)); None }
  }
}

object RelTupleTabulator extends ScoobiApp {

  def postagger = postaggerLocal.get
  private val postaggerLocal = new ThreadLocal[OpenNlpPostagger] { override def initialValue = new OpenNlpPostagger }
  
  def buildTypeContext(rel: String, relContexts: Iterator[ArgContext]): Iterable[TypeContext] = {

    // we need to count the frequency of:
    // -- (arg1Type, arg2Type) pairs,
    val pairCounts = new mutable.HashMap[(String, String), MutInt]
    val argCounts = new mutable.HashMap[(String, String), (mutable.Map[String, MutInt], mutable.Map[String, MutInt])]

    def toTypePair(context: ArgContext): (String, String) = (getType(context.arg1), getType(context.arg2))

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

    val groupSizes = grouped.flatMap {
      case (rel, argContexts) =>
        val size = argContexts.size
        if (size >= minFrequency && size <= maxFrequency) Some((rel, size.toString)) else None
    }

    val groupsWithSizes = grouped.join(groupSizes)

    // take rel, Iterable[ArgContext] and traverse the iterable to find most common (arg1Type, arg2Type) pairs,
    // as well as the most common arg1/arg2 strings for each (arg1Type, arg2Type) pair.
    val typeContexts = groupsWithSizes.flatMap {
      case (rel, (contexts, size)) =>
        val typeContexts = buildTypeContext(rel, contexts.iterator.flatMap(ArgContext.fromString _).take(50000)).toSeq.sortBy(-_.freq)
        typeContexts.take(15).map { context => "%s\t%s".format(size, context.toString) }
    }

    persist(toTextFile(typeContexts, outputPath + "/"))
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
    } catch { case e: Exception => { e.printStackTrace; None } }
  }
}