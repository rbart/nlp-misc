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
import net.sf.extjwnl.data.PointerType
import net.sf.extjwnl.data.Pointer
import java.io.FileNotFoundException
import java.net.URL
import edu.mit.jwi.item.Pointer

import edu.washington.cs.knowitall.browser.hadoop.scoobi.TypeContext

case class CountedRelation(val rel: String, val postags: String, val freq: Int, val arg1s: Seq[String], val arg2s: Seq[String]) {
  val tokens: Seq[PostaggedToken] = rel.split(" ").zip(postags.split(" ")).map { case (string, postag) => new PostaggedToken(postag, string, 0) }
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
  
  def outputRows(relFreq: Int, context: TypeContext): Seq[String] = {
    
    val verbIndex = context.relTokens.lastIndexWhere(_.isVerb)
    val adjIdxs = adjIndexes(context.relTokens)
    val tokenLists = context.relTokens.zipWithIndex.map { case (token, index) =>      
      val words = wnWords(token).take(maxSensesPerWord).toList
      if (!words.isEmpty && (index == verbIndex || token.isNoun || adjIdxs.contains(index))) { words.map(word => WordnetToken(token, Some(word))) }
      else { List(WordnetToken(token, None)) }
    }
    
    val wordnetRelations = cartesianProduct(tokenLists.toList).map(WordnetRelation.apply _)
    
    val result = wordnetRelations.map(outputRow(relFreq, context)  _).take(maxSenseCombos)
    
    
    result
  }
  
  def outputRow(relFreq: Int, context: TypeContext)(wnRel: WordnetRelation): String = {
    Seq(
      wnRel.toString,
      relFreq,
      "", // "keep?"
      "", // "syn_of"
      context.arg1sFormatted,
      context.arg2sFormatted, 
      " ",  // empty column
      "",   // arg1 sense (by hand)
      "",   // arg2 sense (by hand)
      wnRel.glossStrings.mkString(" | "),
      " ", // empty column for excel
      wnRel.synStrings,
      (wnRel.senseChainStrings).mkString("\t")
    ).mkString("\t")
  }
  
  case class WordnetToken(val token: PostaggedToken, val wordOpt: Option[Word]) {
    def word = wordOpt.get
    def synset = word.getSynset()
    def synOpt = wordOpt.map(_.getSynset)
  }

  case class WordnetRelation(val tokens: Seq[WordnetToken]) {
    
    val verbIndex = tokens.lastIndexWhere(_.token.isVerb)
    
    require(verbIndex >= 0)
    
    val adjIdxs = adjIndexes(tokens map { _.token })
    
    def isContentWord(token: WordnetToken, index: Int) = (token.token.isNoun || index == verbIndex || adjIdxs.contains(index)) && token.synOpt.isDefined
    def contentWords = tokens.zipWithIndex.filter { case (token, index) => isContentWord(token, index) }
    
    override val toString: String = tokens(verbIndex).wordOpt match {
      case Some(word) => toStringWithReplacement(verbIndex, word)
      case None => toStringWithReplacement(-1, null)
    }
    
    def toStringWithReplacement(replIndex: Int, word: Word): String = tokens.zipWithIndex.map { case (token, index) =>
      if (index == replIndex) wordBracketString(word)
      else if (isContentWord(token, index)) wordBracketString(token.word)
      else token.token.string
    }.mkString(" ")
    
    def senseChainString(chainIndex: Int)(senseChain: Seq[SynsetWrapper]): String = {
      var stopIndex = senseChain.indexWhere { sense =>
        chainWordStopList.contains(wordBracketString(sense.synset.getWords.head))
      }
      if (stopIndex < 0) stopIndex = 100 // a chain should never be this long.. (hack)
      val limitedChain = senseChain.take(stopIndex + 1).take(maxChainLength) // really just need something like "takeWhileInclusive"
      val chainParts = limitedChain.map { sense => 
        sense.cursor + toStringWithReplacement(chainIndex, sense.synset.getWords.head) 
      }
      if (!chainParts.isEmpty) (Seq(toString) ++ chainParts).mkString(" ") else ""
    }

    def troponymChainStrings: Seq[String] = {
      val tropoSynsets = contentWords.map { case (token, tokIndex) => (token, tokIndex, troponyms(token.synOpt.get)) }
      val candidates = tropoSynsets.flatMap { case (token, tokIndex, troponyms) => 
        troponyms.zipWithIndex.flatMap { case (trop, tropIndex) =>
          trop.getWords.map { word =>
            // penalize high sense numbers as well as high troponym index:
            (toStringWithReplacement(tokIndex, word), word.getUseCount())  
          }
        }
      }
      candidates.sortBy(-_._2).map(_._1).take(6).map(str => "%s >h: %s".format(str, toString))
    }
    
    def senseChainStrings: Seq[String] = {

      val strings = troponymChainStrings ++ contentWords.flatMap { case (token, index) => 
        val hypChain = hypernymChain(token.word.getSynset())
        val entChain = entailmentChain(token.synset)
        Seq(hypChain, entChain) map senseChainString(index)
      }.filter(!_.isEmpty()).distinct
      if (strings.isEmpty) Seq("none") else strings
    }
    
    def glossStrings: Seq[String] = {
      val glossWords = contentWords.flatMap { case (token, index) => token.wordOpt }
      glossWords.map { word => glossString(word) }
    }
    
    def synStrings = contentWords.flatMap { case (token, index) => token.wordOpt }.map(word=>synString(word.getSynset)).mkString(" | ")
  }
  
  def glossString(word: Word): String = "(%d) %s".format(word.getUseCount, word.getSynset.getGloss)
  
  def synString(synset: Synset): String = synset.getWords().map(wordBracketString _).mkString(",")
  
  def wordBracketString(word: Word): String = {
    val posChar = word.getPOS().getLabel().charAt(0).toString
    "%s[%s%s]".format(word.getLemma, posChar, getSenseNum(word))
  }
  
  def getSenseNum(word: Word): Int = {
    val synset = word.getSynset
    val senses = wnDict.getIndexWord(word.getPOS, word.getLemma).getSenses.toSeq
    senses.indexWhere(sense => sense.getWords.toSet.contains(word)) + 1
  }
  
  def troponyms(synset: Synset): Iterable[Synset] = {
    val trops = synset.getPointers().filter(_.getType().equals(PointerType.HYPONYM)).map(_.getTargetSynset())
    trops
  }
  
  def entailments(synset: Synset): Iterable[EntSynset] = {
    val ents = synset.getPointers().filter(_.getType().equals(PointerType.ENTAILMENT)).map(_.getTargetSynset())
    ents map EntSynset.apply
  }
  
  def entailmentChain(synset: Synset): Seq[SynsetWrapper] = entailmentChain(0)(synset)
  def entailmentChain(depth: Int)(synset: Synset): Seq[SynsetWrapper] = {
    if (depth > 10) return Seq.empty
    val hypEnts: Iterable[SynsetWrapper] = (entailments(synset) ++ hypernyms(synset))
    hypEnts.headOption match {
      case Some(nextSynset) => Seq(nextSynset) ++ entailmentChain(depth + 1)(nextSynset.synset)
      case None => Seq.empty
    } 
  }
  
  def hypernymChain(synset: Synset): Seq[SynsetWrapper] = hypernymChain(0)(synset)
  def hypernymChain(depth: Int)(synset: Synset): Seq[SynsetWrapper] = {
    if (depth > 10) return Seq.empty
    val hypEnts: Iterable[SynsetWrapper] = (hypernyms(synset) ++ entailments(synset))
    hypEnts.headOption match {
      case Some(nextSynset) => Seq(nextSynset) ++ hypernymChain(depth + 1)(nextSynset.synset)
      case None => Seq.empty
    } 
  }
  
  def hypernyms(synset: Synset): Iterable[HypSynset] = {
    val hyps = synset.getPointers().filter(_.getType().equals(PointerType.HYPERNYM)).map(_.getTargetSynset())
    hyps map HypSynset.apply
  }

  def wnWords(token: PostaggedToken): Seq[Word] = {
    val idxWordOpt = wnLemma(token) 
    idxWordOpt match {
      case Some(idxWord) => {
        val synsets = idxWord.getSenses()
        synsets.flatMap { sense => sense.getWords.find(_.getLemma().toLowerCase.equals(idxWord.getLemma().toLowerCase)) }
      }
      case None => {
        Seq.empty
      }
    }
  }

  def wnLemma(token: PostaggedToken): Option[IndexWord] = wnPOS(token) flatMap { pos =>
    val indexWord = wnDict.lookupIndexWord(pos, token.string)
    Option(indexWord)
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

  val wnHome = "/scratch/WordNet-3.0/file_properties.xml"
  
  def getInstance: RelationTabulator = getInstance(wnHome)
  
  def getInstance(wnHome: String): RelationTabulator = {
    val dict = fetchDictionary(wnHome)
    new RelationTabulator(dict)
  }
  
  def fetchDictionary(wnHome: String): Dictionary = {

    val dict = Dictionary.getInstance(new java.io.FileInputStream(wnHome))
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
    
    val inst = getInstance
      
    def loadFreqAndContext(string: String): Option[(Int, TypeContext)] = {
      val split = string.split("\t")
      TypeContext.fromString(split.drop(1).mkString("\t")).map { context => (split(0).toInt, context) }
    }
    
    val rels = Source.fromFile("/scratch/relations-tabulated.txt").getLines.take(5000).flatMap(loadFreqAndContext _).toSeq
    
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
    
    val filtered = rels.filter({ case (freq, context) => doHaveRelationFilter(context) })
    val outputRows = filtered.flatMap { case (freq, context) => 
      try { 
        inst.outputRows(freq, context)
      } catch { case e: Exception => { e.printStackTrace; Seq.empty } }
    }
    outputRows.zipWithIndex foreach { case (string, index) =>
      println("%s\t%s".format(index, string))
    }
    
//    val idxword = inst.wnDict.lookupIndexWord(POS.NOUN, "president")
//    val senses = idxword.getSenses()
//    println("IndexWord: %s\nSenses:%s".format(idxword, senses.map(s => inst.synString(s)).mkString("\n")))
  }

  def cartesianProduct[T](xss: List[List[T]]): List[List[T]] = xss match {
    case Nil => List(Nil)
    case h :: t => for (xh <- h; xt <- cartesianProduct(t)) yield xh :: xt
  }
  
  def tokensForRel(rel: String): Seq[PostaggedToken] = {
    Seq.empty
  }
  
  def doHaveRelationFilter(context: TypeContext): Boolean = {
    
    val tokens = tokensForRel(context.rel)
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