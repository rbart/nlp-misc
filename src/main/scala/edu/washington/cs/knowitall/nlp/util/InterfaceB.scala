package edu.washington.cs.knowitall.nlp.util

import scopt.OptionParser
import edu.washington.cs.knowitall.nlp.ChunkedSentence

import edu.washington.cs.knowitall.browser.hadoop.scoobi.TypeContextAggregator

import edu.washington.cs.knowitall.tool.postag.PostaggedToken

import scala.collection.JavaConversions._

import relontology.WordNetUtils
import relontology.CleanUtils

// performs the task of stephen's command line interface "B" -
// which is to take as input a relation phrase and return
// wordnet info such as synonyms, entailments, 

object InterfaceB {

  def main(args: Array[String]) {
    
    var relPhrase = "";
    
    val parser = new OptionParser("Interface B") {
      arg("relPhrase", "relation phrase", { str => relPhrase = str })
    }
    
    if (!parser.parse(args)) return
    
    lazy val wnUtils = WordNetUtils.defaultInstance
    lazy val cleanUtilsHtl = CleanUtils.defaultHtl
    lazy val cleanUtilsTncf = CleanUtils.defaultTncf
    
    val relToken = new PostaggedToken("VB", relPhrase, 0)
    
    println("WN Synonyms:")
    println(wnUtils.getWnSynonyms(relToken).mkString(", "))
    println("WN Hypernyms:")
    println(wnUtils.getWnHypernyms(relToken).mkString(", "))
    println("WN Troponyms:")
    println(wnUtils.getWnTroponyms(relToken).mkString(", "))
    println("\nCLEAN HTL Entailments:")
    cleanUtilsHtl.getCleanEntailments(relPhrase).map({ ent => println("%s => %s".format(ent.left, ent.right)) }).sorted.distinct;
    println("\nCLEAN TNCF Entailments:")
    cleanUtilsTncf.getCleanEntailments(relPhrase).map({ ent => println("%s => %s".format(ent.left, ent.right)) }).sorted.distinct;
  }
}




