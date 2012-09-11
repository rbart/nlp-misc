package edu.washington.cs.knowitall.nlp.util

import edu.washington.cs.knowitall.common.Resource.using
import scopt.OptionParser
import scala.io.Source
//import edu.mit.jwi.morph.WordnetStemmer
//import edu.mit.jwi.IDictionary
import net.sf.extjwnl.dictionary.Dictionary
import net.sf.extjwnl.data.POS
import edu.washington.cs.knowitall.tool.postag.PostaggedToken
import scala.collection.JavaConversions._
import net.sf.extjwnl.data.IndexWord
import edu.mit.jwi.item.IndexWordID
import net.sf.extjwnl.data.Word
import net.sf.extjwnl.data.Synset
import java.io.FileNotFoundException
import java.net.URL
import edu.mit.jwi.item.Pointer

case class CountedRelation(val rel: String, val postags: String, val freq: Int, val arg1s: Seq[String], val arg2s: Seq[String]) {
  lazy val tokens: Seq[PostaggedToken] = rel.split(" ").zip(postags.split(" ")).map { case (string, postag) => new PostaggedToken(postag, string, 0) }
}
object CountedRelation {
  def fromString(string: String): Option[CountedRelation] = {
    val split = string.split("\t")
    def commaSplit(str: String) = str.split(",") map { _.trim }
    if (split.length != 5) { System.err.println("Error parsing %s".format(string)); None }
    else Some(CountedRelation(
        rel=split(1), 
        postags=split(2), 
        freq=split(0).toInt, 
        arg1s=commaSplit(split(3)), 
        arg2s=commaSplit(split(4))))
  }
}

sealed abstract class SynsetWrapper(val synset: Synset) {
  def cursor: String
}
case class HypSynset(override val synset: Synset) extends SynsetWrapper(synset) {
  override val cursor: String = ">h: "
}
case class EntSynset(override val synset: Synset) extends SynsetWrapper(synset) {
  override val cursor: String = ">e: "
}

class RelationTabulator(
  val wnDict: Dictionary) {
  
  import RelationTabulator.cartesianProduct
  import RelationTabulator.chainWordStopList
  
  private val maxSensesPerWord = 6
  private val maxChainLength = 6
  private val maxSenseCombos = 100
  
  def adjIndexes(tokens: Seq[PostaggedToken]): Set[Int] = {
    tokens.zip(tokens.drop(1)).zipWithIndex.flatMap { case ((token, nextToken), tokenIndex) =>
      if (token.isAdjective && !nextToken.isNoun && !nextToken.isVerb && !nextToken.isAdjective) Some(tokenIndex)
      else None
    } toSet
  }
  
  def outputRows(rel: CountedRelation): Seq[String] = {
    val verbIndex = rel.tokens.lastIndexWhere(_.isVerb)
    val adjIdxs = adjIndexes(rel.tokens)
    val tokenLists = rel.tokens.zipWithIndex.toList.map { case (token, index) =>
      val words = wnWords(token)
      if (!words.isEmpty && (index == verbIndex || token.isNoun || adjIdxs.contains(index))) words.map(word => WordnetToken(token, Some(word)))
      else List(WordnetToken(token, None))
    }
    val wordnetRelations = cartesianProduct(tokenLists) map WordnetRelation.apply
    wordnetRelations map outputRow(rel) take(maxSenseCombos)
  }
  
  def outputRow(rel: CountedRelation)(wnRel: WordnetRelation): String = {
    Seq(
      wnRel.toString,
      rel.freq,
      "", // "keep?"
      "", // "syn_of"
      rel.arg1s.mkString(" | "),
      rel.arg2s.mkString(" | "), 
      " ",  // empty column
      "",   // arg1 sense (by hand)
      "",   // arg2 sense (by hand)
      wnRel.glossStrings.mkString(" | "),
      " ", // empty column for excel
      wnRel.synStrings,
      (wnRel.senseChainStrings).mkString("\t")
    ).mkString("\t")
  }
  
  case class WordnetToken(val token: PostaggedToken, val word: Option[Word]) 

  case class WordnetRelation(val tokens: Seq[WordnetToken]) {
    
    val verbIndex = tokens.lastIndexWhere(_.token.isVerb)
    
    require(verbIndex >= 0)
    
    val adjIdxs = adjIndexes(tokens map { _.token })
    
    def isContentWord(token: WordnetToken, index: Int) = (token.token.isNoun || index == verbIndex || adjIdxs.contains(index)) && token.word.isDefined
    def contentWords = tokens.zipWithIndex.filter { case (token, index) => isContentWord(token, index) }
    
    override val toString: String = tokens(verbIndex).word match {
      case Some(word) => toStringWithReplacement(verbIndex, word)
      case None => toStringWithReplacement(-1, null)
    }
    
    def toStringWithReplacement(replIndex: Int, word: IWord): String = tokens.zipWithIndex.map { case (token, index) =>
      if (index == replIndex) wordBracketString(word)
      else if (isContentWord(token, index)) wordBracketString(token.word.get)
      else token.token.string
    }.mkString(" ")
    
    def senseChainString(chainIndex: Int)(senseChain: Seq[SynsetWrapper]): String = {
      var stopIndex = senseChain.indexWhere(sense => chainWordStopList.contains(wordBracketString(sense.synset.getWord(1))))
      if (stopIndex < 0) stopIndex = 100 // a chain should never be this long.. (hack)
      val limitedChain = senseChain.take(stopIndex + 1).take(maxChainLength) // really just need something like "takeWhileInclusive"
      val chainParts = limitedChain.map { sense => sense.cursor + toStringWithReplacement(chainIndex, sense.synset.getWord(1)) }
      if (!chainParts.isEmpty) (Seq(toString) ++ chainParts).mkString(" ") else ""
    }

    def troponymChainStrings: Seq[String] = {
      val tropoSynsets = contentWords.map { case (token, tokIndex) => (token, tokIndex, troponyms(token.word.get.getSynset)) }
      val candidates = tropoSynsets.flatMap { case (token, tokIndex, troponyms) => 
        troponyms.zipWithIndex.flatMap { case (trop, tropIndex) =>
          trop.getWords.map { word =>
            // penalize high sense numbers as well as high troponym index:
            (toStringWithReplacement(tokIndex, word), getSenseNum(word) * tropIndex+1)  
          }
        }
      }
      candidates.sortBy(_._2).map(_._1).take(6).map(str => "%s >h: %s".format(str, toString))
    }
    
    def senseChainStrings: Seq[String] = {

      val strings = troponymChainStrings ++ contentWords.flatMap { case (token, index) => 
        val hypChain = hypernymChain(token.word.get.getSynset())
        val entChain = entailmentChain(token.word.get.getSynset())
        Seq(hypChain, entChain) map senseChainString(index)
      }.filter(!_.isEmpty()).distinct
      if (strings.isEmpty) Seq("none") else strings
    }
    
    def glossStrings: Seq[String] = {
      val glossWords = contentWords.flatMap { case (token, index) => token.word }
      glossWords.map { word => glossString(word.getSynset) }
    }
    
    def synStrings = contentWords.flatMap { case (token, index) => token.word }.map(word=>synString(word.getSynset)).mkString(" | ")
  }
  
  def glossString(synset: ISynset): String = synset.getGloss()
  
  def synString(synset: ISynset): String = synset.getWords().map(wordBracketString _).mkString(",")
    
  def wordBracketString(word: IWord): String = {
    val posChar = word.getPOS().toString().charAt(0).toString
    "%s[%s%d]".format(word.getLemma, posChar, getSenseNum(word))
  }
  
  def getSenseNum(word: IWord): Int = {
    val lemma = word.getLemma
    val senses = wnDict.getIndexWord(word.getPOS, lemma).getWordIDs().map(_.getSynsetID())
    senses.indexWhere(_.equals(word.getSynset().getID())) + 1
  }
  
  def troponyms(synset: ISynset): Iterable[ISynset] = {
    val trops = synset.getRelatedSynsets(Pointer.HYPONYM).toSeq
    trops map wnDict.getSynset toIterable
  }
  
  def entailments(synset: ISynset): Iterable[EntSynset] = {
    val ents = synset.getRelatedSynsets(Pointer.ENTAILMENT).toSeq
    ents map wnDict.getSynset map EntSynset.apply toIterable;
  }
  
  def entailmentChain(synset: ISynset): Seq[SynsetWrapper] = entailmentChain(0)(synset)
  def entailmentChain(depth: Int)(synset: ISynset): Seq[SynsetWrapper] = {
    if (depth > 10) return Seq.empty
    val hypEnts: Iterable[SynsetWrapper] = (entailments(synset) ++ hypernyms(synset))
    hypEnts.headOption match {
      case Some(nextSynset) => Seq(nextSynset) ++ entailmentChain(depth + 1)(nextSynset.synset)
      case None => Seq.empty
    } 
  }
  
  def hypernymChain(synset: ISynset): Seq[SynsetWrapper] = hypernymChain(0)(synset)
  def hypernymChain(depth: Int)(synset: ISynset): Seq[SynsetWrapper] = {
    if (depth > 10) return Seq.empty
    val hypEnts: Iterable[SynsetWrapper] = (hypernyms(synset) ++ entailments(synset))
    hypEnts.headOption match {
      case Some(nextSynset) => Seq(nextSynset) ++ hypernymChain(depth + 1)(nextSynset.synset)
      case None => Seq.empty
    } 
  }
  
  def hypernyms(synset: Synset): Iterable[HypSynset] = {
    val hyps = synset.getRelatedSynsets(Pointer.HYPERNYM).toSeq
    hyps map wnDict.getSynset map HypSynset.apply
  }
  
  def wnSynsets(token: PostaggedToken): List[Synset] = wnLemma(token) match {
    case Some(idxWord) => idxWord.getSenses().toList.take(maxSensesPerWord)
    case None => List.empty
  }

  def wnLemma(token: PostaggedToken): Option[IndexWord] = wnPOS(token) map { pos =>
    wnDict.lookupIndexWord(pos, token.string)
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
    new RelationTabulator(dict)
  }
  
  def fetchDictionary(wnHome: String): Dictionary = {

    val path = wnHome + "/dict"
    val url = new URL("file", null, path)

    val dict = Dictionary.getDatabaseBackedInstance(url.toString)
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
      
    val rels = Source.fromFile("D:/scratch/ollie-relations-sorted.txt").getLines.take(200).flatMap(CountedRelation.fromString _).toSeq
    
    //val rels = Seq("251821\tbe part of\tVBZ NN PP").flatMap(CountedRelation.fromString _).toSeq
    
    val columnHeaders = Seq(
      "rel_ID",
      "relation",
      "freq",
      "keep?",
      "syn_of",
      "arg1",
      "arg2",
      "<empty>",
      "argSense1",
      "argSense2",
      "gloss",
      "<empty>",
      "synonyms",
      "h/e chains"
    )
    
    println(columnHeaders.mkString("\t"))
    
    rels.filter(doHaveRelationFilter _).flatMap { rel => 
      try { inst.outputRows(rel) } catch { case e: Exception => Seq.empty }
    }.zipWithIndex foreach { case (string, index) =>
      println("%s\t%s".format(index, string))
    }
    
//    val idxwordid = new IndexWordID("have", POS.VERB)
//    val idxword = inst.wnDict.getIndexWord(idxwordid)
//    val words = idxword.getWordIDs map inst.wnDict.getWord
//    val synsets = words.map { _.getSynset }
//    synsets map inst.troponyms foreach println
  }

  def cartesianProduct[T](xss: List[List[T]]): List[List[T]] = xss match {
    case Nil => List(Nil)
    case h :: t => for (xh <- h; xt <- cartesianProduct(t)) yield xh :: xt
  }
  
  def doHaveRelationFilter(rel: CountedRelation): Boolean = {
    
    val tokens = rel.tokens
    val tokenPairs = tokens.zip(tokens.drop(1))
    !tokenPairs.exists { case (first, second) => 
      val doHave = Set("do", "have").contains(first.string.toLowerCase)
      doHave && second.isVerb
    }
  }
  
  private val chainWordStopList: Set[String] = Set(
      
    // VERBS
    "think[v3]",
    "communicate[v1]",
    "communicate[v2]",
    "guess[v4]",
    "complete[v1]",
    "proceed[v4]",
    "increase[v1]",
    "change_posture[v1]",
    "change_state[v1]",
    "experience[v1]",
    "imagine[v1]",
    "manipulate[v2]",
    "liquefy[v3]",
    "spend[v2]",
    "give[v3]",
    "offer[v4]",
    "trade[v1]",
    "ask[v3]",
    "conceive[v3]",
    "convey[v1]",
    
    // NOUNS
    "part[n1]",
    "physical_entity[n1]",
    "activity[n1]",
    "interest[n1]",
    "person[n1]",
    "thing[n12]",
    "object[n1]",
    "body[n2]",
    "whole[n2]",
    "idea[n1]",
    "form[n3]",
    "measure[n2]"
  )
}