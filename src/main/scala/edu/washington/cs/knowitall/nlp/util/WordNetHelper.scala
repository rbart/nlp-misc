package edu.washington.cs.knowitall.nlp.util

import edu.washington.cs.knowitall.common.Resource.using
import scopt.OptionParser
import scala.io.Source
//import edu.mit.jwi.morph.WordnetStemmer
//import edu.mit.jwi.IDictionary
import net.sf.extjwnl.dictionary.Dictionary
import net.sf.extjwnl.data.POS

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

import java.util.LinkedList

import edu.washington.cs.knowitall.nlp.util.ArgContext.joinTokensAndPostags
import scala.collection.mutable

import edu.washington.cs.knowitall.browser.hadoop.scoobi.util.ExtractionSentenceRecord
import edu.washington.cs.knowitall.tool.postag.PostaggedToken

sealed abstract class SynsetWrapper(val synset: Synset) {
  def cursor: String
}
case class HypSynset(override val synset: Synset) extends SynsetWrapper(synset) {
  override val cursor: String = ">h: "
}
case class EntSynset(override val synset: Synset) extends SynsetWrapper(synset) {
  override val cursor: String = ">e: "
}

class WordNetHelper(
  val wnDict: Dictionary) {
  
  import WordNetHelper.chainWordStopList
  
  private val maxSensesPerWord = 6
  private val maxChainLength = 6
  private val maxSenseCombos = 100
  
  def adjIndexes(tokens: Seq[PostaggedToken]): Set[Int] = {
    tokens.zip(tokens.drop(1)).zipWithIndex.flatMap { case ((token, nextToken), tokenIndex) =>
      if (token.isAdjective && !nextToken.isNoun && !nextToken.isVerb && !nextToken.isAdjective) Some(tokenIndex)
      else None
    } toSet
  }
  
  def outputAllRows(contexts: Seq[FreqRelTypeContext]): Seq[String] = {
    if (contexts.isEmpty) return Seq.empty
    outputSenseRows(contexts)
  }

  def outputContextRows(relContentWords: Seq[String], contexts: Seq[FreqRelTypeContext]): Seq[String] = {
    
    val spacers = relContentWords.map(_ => " ")
    val rel = contexts.head.typeContext.rel
    
    Seq("Contexts",
      (Seq("C", "relation") ++ relContentWords ++ Seq("arg1_type", "arg1_terms", "arg2_type", "arg2_terms")).mkString("\t")
      ) ++ contexts.sortBy(-_.freq).map { context =>
        val tc = context.typeContext
      	(Seq("C", rel) ++ spacers ++ Seq(tc.arg1Type, tc.arg1sFormatted, tc.arg2Type, tc.arg2sFormatted)).mkString("\t")
      }
  }
  
  def outputSenseRows(contexts: Seq[FreqRelTypeContext]): Seq[String] = {
    
    val context = contexts.head
   
    
    val verbIndex = context.rel.lastIndexWhere(_.isVerb)
    val adjIdxs = adjIndexes(context.rel)
    
    // this contains the various wordnet word senses for each content word in the relation.
    // It is a list of lists, where the elements are wrappers for an optional wordnet word.
    // in the case that the token is not a content word, the wrapper contains an empty word field.
    val tokenLists = context.rel.zipWithIndex.map { case (token, index) =>      
      val words = wnWords(token).take(maxSensesPerWord).toList
      if (!words.isEmpty && (index == verbIndex || token.isNoun || adjIdxs.contains(index))) { words.map(word => WordnetToken(token, Some(word))) }
      else { List(WordnetToken(token, None)) }
    }

    
    // we need to construct WordnetRelations that cover all senses of the content words, while containing only
    // a single content word at a time. e.g. for relation "a b c" where a,b,c are content words with senses a0, a1, etc,
    // and "a" means word a as a non-content word,
    // we want:
    // a0, b, c
    // a1, b, c
    // ...
    // a, b0, c
    // ...
    // a, b, c0
    
    val relationCombos = (0 until tokenLists.length).map({ i =>
      val tokensBefore = tokenLists.take(i).flatMap(_.headOption).map(_.copy(wordOpt = None))
      val tokensAfter = tokenLists.drop(i+1).flatMap(_.headOption).map(_.copy(wordOpt = None))
      val combos = tokenLists(i).map(wordnetToken => tokensBefore ++ Seq(wordnetToken) ++ tokensAfter)
      combos map WordnetRelation.apply
    }).flatten

    
    val outputLines = new LinkedList[String]()
    outputLines.add(Seq("Senses", "term", "gloss", "synset", "h/e chains").mkString("\t"))
    
    relationCombos.foreach { rel =>	
      val wnTokenOpt = rel.tokens.find(_.wordOpt.isDefined)
      wnTokenOpt.foreach { wnToken => outputLines.add(senseRow(wnToken.word, rel)) }
    }
    
    val relContentWords = tokenLists.flatMap(_.find(_.wordOpt.isDefined)).map(_.token.string)
    
    val relInfo = Seq(context.freq, context.rel.map(_.string).mkString(" "), context.rel.map(_.postag).mkString(" ")).mkString("\t")
    
    Seq(relInfo) ++ outputLines ++ outputContextRows(relContentWords, contexts)
  }
  
  def senseRow(word: Word, rel: WordnetRelation): String = {
    // wordbracketstring,
    // gloss string,
    // synset,
    // h/e chains
    Seq(
      "S",
      wordBracketString(word),
      glossString(word),
      rel.synStrings,
      rel.senseChainStrings.mkString("\t")
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
object WordNetHelper {

  val wnHome = "/scratch/WordNet-3.0/file_properties.xml"
  
  def getInstance: WordNetHelper = getInstance(wnHome)
  
  def getInstance(wnHome: String): WordNetHelper = {
    val dict = fetchDictionary(wnHome)
    new WordNetHelper(dict)
  }
  
  def fetchDictionary(wnHome: String): Dictionary = {

    val dict = Dictionary.getInstance(new java.io.FileInputStream(wnHome))
    return dict;
  }

  def loadFreqAndContext(string: String): Option[(Int, TypeContext)] = {
    val split = string.split("\t")
    TypeContext.fromString(split.drop(1).mkString("\t")).map { context => (split(0).toInt, context) }
  }
  
  def main(args: Array[String]): Unit = {
    
    var input: Source = Source.stdin
    
    val parser = new OptionParser("Relation Tabulator") {
      opt("inputFile", "TypeContexts, tab serialized (read from stdin by default", { str => input = Source.fromFile(str) })
    }
    
    if (!parser.parse(args)) return
    
    val inst = getInstance
      

    
    val rels = input.getLines.flatMap(FreqRelTypeContext.fromString _).toSeq
    
    //val rels = Seq("251821\tbe part of\tVBZ NN PP").flatMap(CountedRelation.fromString _).toSeq
    
    
    val filtered = rels.filter({ case context => doHaveRelationFilter(context) }).filter({ case context => context.rel.exists(_.isVerb) })
    
    // this is horrible, ugly code, written while desperately behind and trying to just get something that works.
    
    var lastRel: String = ""
    var numRels = 0
    val accumulator = new mutable.MutableList[FreqRelTypeContext]()
    
    scala.util.control.Breaks.breakable { 
    
    for (context <- filtered) {
      if (numRels > 5000) scala.util.control.Breaks.break()
      val currentRel = context.typeContext.rel
      if (currentRel.equals(lastRel)) {
        accumulator += context
      } else {
        lastRel = currentRel
        inst.outputAllRows(accumulator.toSeq) foreach println
        numRels += 1
        accumulator.clear()
        accumulator += context
      }
    }
    inst.outputAllRows(accumulator.toSeq) foreach println
    
    }
    
    
//    val idxword = inst.wnDict.lookupIndexWord(POS.NOUN, "president")
//    val senses = idxword.getSenses()
//    println("IndexWord: %s\nSenses:%s".format(idxword, senses.map(s => inst.synString(s)).mkString("\n")))
  }

  def cartesianProduct[T](xss: List[List[T]]): List[List[T]] = xss match {
    case Nil => List(Nil)
    case h :: t => for (xh <- h; xt <- cartesianProduct(t)) yield xh :: xt
  }
  
  def doHaveRelationFilter(context: FreqRelTypeContext): Boolean = {
    
    val tokens = context.rel
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