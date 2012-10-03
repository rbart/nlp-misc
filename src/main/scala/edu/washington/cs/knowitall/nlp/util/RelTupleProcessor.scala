package edu.washington.cs.knowitall.nlp.util

import scopt.OptionParser
import scala.io.Source
import scala.collection.JavaConversions._
import java.io.PrintStream
import java.io.FileOutputStream

import net.sf.extjwnl.data.Word
import net.sf.extjwnl.data.Synset
import net.sf.extjwnl.data.PointerType
import net.sf.extjwnl.data.Pointer

import edu.washington.cs.knowitall.browser.hadoop.scoobi.util.ExtractionSentenceRecord

import edu.washington.cs.knowitall.tool.postag.PostaggedToken

import edu.washington.cs.knowitall.nlp.ChunkedSentence

case class ArgContext(val arg1: Seq[PostaggedToken], val arg2: Seq[PostaggedToken], val esr: ExtractionSentenceRecord) {
  
  override def toString = {
    val arg1Tokens = arg1.map(_.string).mkString(" ")
    val arg1Postags = arg1.map(_.postag).mkString(" ")
    val arg2Tokens = arg2.map(_.string).mkString(" ")
    val arg2Postags = arg2.map(_.postag).mkString(" ")
    Seq(arg1Tokens, arg1Postags, arg2Tokens, arg2Postags, esr.toString).mkString("\t")
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
    if (split.length > 4) {
      val arg1 = joinTokensAndPostags(split(0), split(1))
      val arg2 = joinTokensAndPostags(split(2), split(3))
      val esr = new ExtractionSentenceRecord(split.drop(4))
      Some(ArgContext(arg1, arg2, esr))
    } else {
      System.err.println("Error parsing ArgContext: %s".format(str))
      None
    }
  }
}

object RelTupleProcessor {

  val relTabulator = RelationTabulator.getInstance

  def getChains(word: Word) = {
    val chains = Seq(relTabulator.entailmentChain(word.getSynset), relTabulator.hypernymChain(word.getSynset))
    chains.foreach { chain =>
      val line = chain.map { synset =>
        synset.synset.getWords().map(_.getLemma()).mkString(",")
      }.mkString(" --> ")
      System.err.println(line)
    }
    chains
  }

  def findTopClass(word: Word): Option[String] = {

    getChains(word) flatMap findTopClassChain headOption
  }

  def findTopClassChain(chain: Iterable[SynsetWrapper]): Option[String] = {
    for (wrapper <- chain) {
      val synset = wrapper.synset
      val classStrings = synset.getWords.map(relTabulator.wordBracketString(_)).toSet
      topClasses.find(topClass => classStrings.contains(topClass)) match {
        case Some(str) => return Some(str)
        case None =>
      }
    }
    None
  }

  def getPrintableChains(word: Word) = {
    getChains(word).map { chain =>
      chain.take(7).map(wrapper => wrapper.synset.getWords.head).map(relTabulator.wordBracketString _).mkString(" > ")
    }.mkString(", ")
  }

  def getType(tokens: Seq[PostaggedToken]): String = {

    // is it a pronoun?
    tokens.find(_.postag.startsWith("PRP")) match {
      case Some(tok) => {
        if (tokens.length <= 2) {
          if (tok.string.equals("it")) return "other_noun"
          else return "person[n1]"
        }
      }
      case None => {}
    }

    // is it a proper noun?
    if (tokens.exists(_.isProperNoun)) return "other_noun"

    // combine all "NN" tags into a string and look this up:   
    val lookupString = tokens.dropWhile(!_.isNoun).takeWhile(_.isNoun).map(_.string).mkString(" ")

    // fake a Postagged Token with this string, as a noun:
    val lookupToken = new PostaggedToken("NN", lookupString, 0)

    val word = relTabulator.wnWords(lookupToken).headOption

    word flatMap findTopClass match {
      case Some(clazz) => clazz
      case None => "other_noun"
    }
  }

  def main(args: Array[String]): Unit = {

    var input: Source = Source.stdin
    var output: PrintStream = System.out

    val parser = new OptionParser("RelTupleProcessor") {
      opt("inputFile", "file for input, default stdin", { str => input = Source.fromFile(str) })
      opt("outputFile", "file for output, default stdout", { str => output = new PrintStream(new FileOutputStream(str)) })
    }

    if (!parser.parse(args)) return

    def parseRelArgContext(str: String): Option[(String, ArgContext)] = {
      val split = str.split("\t")
      if (split.length >= 5) ArgContext.fromString(split.drop(1).mkString("\t")).map { context => (split(0), context) }
      else None
    }

    // end helper methods here

    val grouped = input.getLines.flatMap(parseRelArgContext _).toSeq.groupBy({ case (rel, context) => rel })
    grouped.foreach {
      case (rel, relContexts) =>
        // convert arg1 string context to arg type context
        val typeContexts = relContexts.map {
          case (rel, context) =>
            (getType(context.arg1), getType(context.arg2))
        }
        val contextGroups = typeContexts.groupBy(identity)
        val contextFreqs = contextGroups.map({ case (context, contexts) => (context, contexts.size) }).iterator.toSeq.sortBy(-_._2).take(20)

        contextFreqs.foreach {
          case ((arg1Type, arg2Type), freq) =>
            val outString = Seq(rel, arg1Type, arg2Type, freq)
            println(outString)
        }
    }

    // map input lines to (rel, argContext)
    // then, need to map argContext.arg1 /.arg2 to classes - have 
    // something encapsulate this, so that we ignore proper nouns (for now), 
    // and treat pronouns accordingly.
  }
  


  val topClasses = Set(
    "abstraction[n6]",
    "action[n1]",
    "animal[n1]",
    "artifact[n1]",
    "communication[n2]",
    "event[n1]",
    "group[n1]",
    "knowledge[n1]",
    "location[n1]",
    "number[n1]",
    "organization[n1]",
    "person[n1]",
    "physical_entity[n1]",
    "quantity[n1]",
    "time_period[n1]")

}