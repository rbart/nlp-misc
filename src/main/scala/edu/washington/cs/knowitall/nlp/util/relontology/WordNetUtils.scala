package edu.washington.cs.knowitall.nlp.util.relontology

import edu.washington.cs.knowitall.tool.postag.PostaggedToken

import edu.washington.cs.knowitall.nlp.util.RelTupleProcessor
import edu.washington.cs.knowitall.nlp.util.WordNetHelper

import scala.collection.JavaConversions._

/** A helper class for performing wordnet lookups */
class WordNetUtils(
  val wnHelper: WordNetHelper) {

  private val max_senses = 6
  
  def getWnClass(tokens: Seq[PostaggedToken]): WordNetClass = {
    val rawClass = RelTupleProcessor.getType(tokens)
    WordNetClass.fromString(rawClass)
  }
  
  def getWnEntailments(token: PostaggedToken): Seq[Entailment] = {
    val wnWords = wnHelper.wnWords(token).take(max_senses)
    val senses = wnWords.map(_.getSynset()).distinct
    val hypernymSenses = senses.flatMap(sense => wnHelper.entailments(sense)++wnHelper.hypernyms(sense)).map(_.synset)
    val hypernyms = hypernymSenses.flatMap(_.getWords().map(_.getLemma())).sorted.distinct
    hypernyms map WnEntailment.apply
  }
  
  def getWnTroponyms(token: PostaggedToken): Seq[Troponym] = {
    val wnWords = wnHelper.wnWords(token).take(max_senses)
    val senses = wnWords.map(_.getSynset()).distinct
    val troponymSenses = senses.flatMap(sense => wnHelper.troponyms(sense))
    val troponyms = troponymSenses.flatMap(_.getWords().map(_.getLemma())).sorted.distinct
    troponyms map WnTroponym.apply
  }
  
  def getWnSynonyms(token: PostaggedToken): Seq[Synonym] = {
    val wnWords = wnHelper.wnWords(token).take(max_senses)
    val senses = wnWords.map(_.getSynset()).distinct
    val synonyms = senses.flatMap(sense => sense.getWords().map(_.getLemma())).sorted.distinct
    synonyms.filter(!_.equals(token.string)) map WnSynonym.apply// don't include the original in the list of synonyms
  }
}

object WordNetUtils {
  lazy val defaultInstance = new WordNetUtils(WordNetHelper.getInstance)
}

object WordNetUtilsTest {
  
  import scopt.OptionParser
  
  def main(args: Array[String]): Unit = {
    
    var noun = ""
    var rel = ""
      
    val parser = new OptionParser("WordNetUtilsTest") {
      opt("arg", "a noun", { str => noun = str })
      opt("rel", "a verb", { str => rel = str })
    }
    
    if (!parser.parse(args)) return
    
    lazy val wnUtils = new WordNetUtils(WordNetHelper.getInstance)
    
    if (!noun.isEmpty()) {
      val nounToken = new PostaggedToken("NN", noun, 0)
      val wnClass = wnUtils.getWnClass(Seq(nounToken))
      println("Top-level WordNet class for %s is: %s".format(noun, wnClass.string))
    }
    
    if (!rel.isEmpty()) {
      val verbToken = new PostaggedToken("VB", rel, 0)
      val hypernyms = wnUtils.getWnEntailments(verbToken)
      val troponyms = wnUtils.getWnTroponyms(verbToken)
      val synonyms = wnUtils.getWnSynonyms(verbToken)
      println("WN Hypernyms for %s are: %s".format(rel, hypernyms.mkString(", ")))
      println("WN Troponyms for %s are: %s".format(rel, troponyms.mkString(", ")))
      println("WN Synonyms for %s are: %s".format(rel, synonyms.mkString(", ")))
    }
    println("Program terminated.")
  }
}
