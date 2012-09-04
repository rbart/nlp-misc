package edu.washington.cs.knowitall.nlp.util

import edu.washington.cs.knowitall.common.Resource.using
import scopt.OptionParser
import scala.io.Source
import edu.mit.jwi.morph.WordnetStemmer
import edu.mit.jwi.IDictionary
import edu.mit.jwi.Dictionary
import edu.mit.jwi.item.POS
import edu.washington.cs.knowitall.tool.postag.PostaggedToken
import scala.collection.JavaConversions._
import edu.mit.jwi.item.IIndexWord
import edu.mit.jwi.item.IndexWord
import edu.mit.jwi.item.IWord
import java.io.FileNotFoundException
import java.net.URL
import edu.mit.jwi.item.Pointer

case class CountedRelation(val rel: String, val postags: String, val freq: Int) {
  def tokens: Seq[PostaggedToken] = rel.split(" ").zip(postags.split(" ")).map { case (string, postag) => new PostaggedToken(postag, string, 0) }
}
object CountedRelation {
  def fromString(string: String): Option[CountedRelation] = {
    val split = string.split("\t")
    if (split.length != 3) None
    else Some(CountedRelation(split(1), split(2), split(0).toInt))
  }
}

class RelationTabulator(
  val wnDict: IDictionary,
  val wnStemmer: WordnetStemmer) {
  
  def outputRow(id: Int, rel: CountedRelation): String = {
    val words = rel.tokens map wnWords flatMap { _.headOption }
    outputRow(id, rel, words)
  }
  
  def outputRow(id: Int, rel: CountedRelation, words: Seq[IWord]): String = {
    val heCols = words map { word => 
      val string = word.getLemma()
      val synsetID = word.getSynset().getID().toString
      val gloss = word.getSynset().getGloss()
      val ents = entailments(word) map hypEntColumns("e") mkString("\t")
      val hyps = hypernyms(word) map hypEntColumns("h") mkString("\t")
      Seq(string, synsetID, gloss, ents, hyps).mkString("\t")
    }
    Seq(id.toString, rel.rel, rel.freq.toString, heCols.mkString("\t")).mkString("\t")
  }
  
  def hypEntColumns(hOrE: String)(word: IWord): String = {
    val string = word.getLemma
    val wnId = word.getID.toString
    Seq(string, wnId, hOrE).mkString("\t")
  }
  
  def entailments(word: IWord): Iterable[IWord] = {
    val ents = word.getRelatedWords(Pointer.ENTAILMENT).toSeq
    println("ents: " + ents)
    ents map wnDict.getWord
  }
  
  def hypernyms(word: IWord): Iterable[IWord] = {
    val hyps = word.getRelatedWords(Pointer.HYPERNYM_INSTANCE).toSeq
    println("hyps: "+ hyps)
    hyps map wnDict.getWord
  }
  
  def wnWords(token: PostaggedToken): Seq[IWord] = {
    wnLemmas(token).flatMap { idxWord =>
      val wordIds = idxWord.getWordIDs()
      val words = wordIds map wnDict.getWord
      words
    }
  }
  
  def wnLemmas(token: PostaggedToken): Seq[IIndexWord] = wnPOS(token) match {
    case Some(pos) => wnStemmer.findStems(token.string, pos).map { str => wnDict.getIndexWord(str, pos) }
    case None => Seq.empty
  }
  
  def wnPOS(token: PostaggedToken): Option[POS] = {
    if (token.postag.startsWith("NN")) Some(POS.NOUN)
    else if (token.postag.startsWith("VB")) Some(POS.VERB)
    else if (token.postag.startsWith("JJ")) Some(POS.ADJECTIVE)
    else if (token.postag.startsWith("RB")) Some(POS.ADVERB)
    else None
  }
}

/**
 * Takes a list of relation strings as input (for example, in descending order of prominence) and 
 * annotates them with wordnet hypernym/entailment info in a tab-delimited format, suitable
 * for hand-editing in a spreadsheet.
 */
object RelationTabulator {

  def getInstance(wnHome: String): RelationTabulator = {
    val dict = fetchDictionary(wnHome)
    new RelationTabulator(dict, new WordnetStemmer(dict))
  }
  
  def fetchDictionary(wnHome: String): Dictionary = {

    val path = wnHome + "/dict"
    val url = new URL("file", null, path)

    val dict = new Dictionary(url)
    val success = dict.open()
    if (success) dict else throw new FileNotFoundException()
    return dict;
  }
  
  def main(args: Array[String]): Unit = {
    
//    var inputFile: String = ""
//    
//    val parser = new OptionParser("Relation Tabulator") {
//      arg("inputFile", "file with freq, relation string separated by tabs", { str => inputFile = str })
//    }
//    
//    if (!parser.parse(args)) return
    
    val wnHome = "D:/scratch/WordNet-3.0"
    
    val inst = getInstance(wnHome)
      
    val rels = Source.fromFile("D:/scratch/ollie-relations-sorted.txt").getLines.take(100).flatMap(CountedRelation.fromString _).toSeq
    
    rels.zipWithIndex map { case (rel, index) => 
      inst.outputRow(index, rel)
    } foreach println
    
  }
  

}