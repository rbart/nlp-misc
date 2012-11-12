package edu.washington.cs.knowitall.nlp.util.relontology

import edu.washington.cs.knowitall.tool.postag.PostaggedToken

class WordNetUtils {

  def getWnClass(tokens: Seq[PostaggedToken]): WordNetClass = {
    OtherNoun
  }
  
  def getWnHypernyms(tokens: Seq[PostaggedToken]): Seq[String] = {
    Seq("Not implemented")
  }
  
  def getWnTroponyms(tokens: Seq[PostaggedToken]): Seq[String] = {
    Seq("Not implemented")
  }
  
  def getWnSynonyms(tokens: Seq[PostaggedToken]): Seq[String] = {
    Seq("Not implemented")
  }
}