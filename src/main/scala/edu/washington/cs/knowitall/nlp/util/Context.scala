package edu.washington.cs.knowitall.nlp.util

import edu.washington.cs.knowitall.tool.postag.PostaggedToken
import scala.Array.canBuildFrom
import scala.Array.fallbackCanBuildFrom

import edu.washington.cs.knowitall.nlp.ChunkedSentence

import edu.washington.cs.knowitall.browser.hadoop.scoobi.util.ExtractionSentenceRecord

case class FreqRelTypeContext(val freq: Int, val rel: Seq[PostaggedToken], val typeContext: TypeContext) {
  override def toString = {
    val relPostags = rel.map(_.postag).mkString(" ")
    Seq(freq, relPostags, typeContext.toString).mkString("\t")
  }
}
object FreqRelTypeContext {
  def fromString(string: String): Option[FreqRelTypeContext] = {
    def fail = { System.err.println("FreqRelTypeContext couldn't parse: %s".format(string)); None }
    val split = string.split("\t")
    if (split.length <= 4) return fail
    val typeContext = TypeContext.fromString(split.drop(2).mkString("\t")).getOrElse(return fail)
    val relTokens = ArgContext.joinTokensAndPostags(typeContext.rel, split(1))
    Some(FreqRelTypeContext(split(0).toInt, relTokens, typeContext))
  }
}

case class ArgContext(val arg1: Seq[PostaggedToken], val arg2: Seq[PostaggedToken], val sent: ChunkedSentence) {
  
  override def toString = {
    val arg1Tokens = arg1.map(_.string).mkString(" ")
    val arg1Postags = arg1.map(_.postag).mkString(" ")
    val arg2Tokens = arg2.map(_.string).mkString(" ")
    val arg2Postags = arg2.map(_.postag).mkString(" ")
    Seq(arg1Tokens, arg1Postags, arg2Tokens, arg2Postags, sent.getTokensAsString(), sent.getPosTagsAsString(), sent.getChunkTagsAsString()).mkString("\t")
  }
}

object ArgContext {

  def joinTokensAndPostags(tokens: String, postags: String): Seq[PostaggedToken] = {
    tokens.split(" ").zip(postags.split(" ")).map {
      case (tok, pos) =>
        new PostaggedToken(pos, tok, 0)
    }
  }

  def fromString(str: String): Option[ArgContext] = {
    val split = str.split("\t")
    if (split.length > 6) {
      val arg1 = joinTokensAndPostags(split(0), split(1))
      val arg2 = joinTokensAndPostags(split(2), split(3))
      val tokens = split(4).split(" ")
      val postags = split(5).split(" ")
      val chunktags= split(6).split(" ")
      Some(ArgContext(arg1, arg2, new ChunkedSentence(tokens, postags, chunktags)))
    } else {
      System.err.println("Error parsing ArgContext: %s".format(str))
      None
    }
  }
}

case class TypeContext(
  val arg1Type: String,
  val rel: String,
  val arg2Type: String,
  val freq: Int,
  val arg1s: Seq[(String, Int)],
  val arg2s: Seq[(String, Int)]) {

  import edu.washington.cs.knowitall.browser.hadoop.scoobi.TypeContextAggregator.postagger
  
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
