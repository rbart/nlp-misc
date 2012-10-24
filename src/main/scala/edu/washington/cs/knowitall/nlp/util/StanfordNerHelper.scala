package edu.washington.cs.knowitall.nlp.util

import scala.collection.JavaConversions._
import edu.washington.cs.knowitall.nlp.ChunkedSentence
import collection.mutable.ArrayBuffer
import opennlp.tools.util.Span

import edu.washington.cs.knowitall.browser.hadoop.scoobi.util.ExtractionSentenceRecord

import edu.washington.cs.knowitall.commonlib.Range
import edu.washington.cs.knowitall.util.DefaultObjects
import edu.washington.cs.knowitall.Sentence
import opennlp.tools.tokenize.{TokenizerME, TokenizerModel}
import edu.washington.cs.knowitall.tool.postag.PostaggedToken

import edu.washington.cs.knowitall.Sentence
import edu.washington.cs.knowitall.`type`.tag.StanfordNamedEntityTagger
import edu.washington.cs.knowitall.`type`.Type

import scala.io.Source

object StanfordNerHelper {
  
  val tokenizerModel = new TokenizerModel(
    DefaultObjects.getResourceAsStream(DefaultObjects.tokenizerModelFile))
  val tokenizerLocal = new ThreadLocal[TokenizerME] {
    override def initialValue() = {
      new TokenizerME(tokenizerModel);
    }
  };

  def tokenizer = tokenizerLocal.get 
  
  val netaggerLocal = new ThreadLocal[StanfordNamedEntityTagger] { override def initialValue() = new StanfordNamedEntityTagger("") }
  def netagger = netaggerLocal.get
  
  def parseChunkedSentence(text:String, postags:Seq[String], chunktags:Seq[String]): Option[ChunkedSentence] = {

    try {
      val offsets  = tokenizer.tokenizePos(text)
      val ranges = new ArrayBuffer[Range](offsets.length)
      val tokenList = new ArrayBuffer[String](offsets.length)
      for (span:Span <- offsets) {
        ranges.add(Range.fromInterval(span.getStart(), span.getEnd()))
        tokenList.add(text.substring(span.getStart(), span.getEnd()))
      }
      Some(new ChunkedSentence(ranges.toArray, tokenList.toArray, postags.toArray, chunktags.toArray))
    }catch {
      case e:Exception => {
        println("Caught exception building chunked sentence: " + text + " postags: " + postags)
        e.printStackTrace()
        Some(new ChunkedSentence(new ArrayBuffer[String].toArray, new ArrayBuffer[String].toArray, new ArrayBuffer[String].toArray))
      }

    }
  }
  
  def getChunkedSentence(esr: ExtractionSentenceRecord): Option[ChunkedSentence] = parseChunkedSentence(esr.sentence, esr.posTags, esr.chunkTags)
  
  def locateArgInSentence(arg: String, sentence: ChunkedSentence): Set[Range] = {
    // subgoal this: 
    // - generate the set of ranges to be examined. If there is a match, then we can jsut keep that range.
   
    val argLength = arg.split(" ").length
    
    val ranges = (0 to (sentence.getLength - argLength)).map { start =>
      Range.fromInterval(start, start + argLength)
    } 
    
    val candidates = for (range <- ranges) yield { sentence.getTokens(range).mkString(" ") }
    
    val matchRanges = ranges.zip(candidates).filter { case (range, string) => string.equals(arg) } map { _._1 }
    
    matchRanges toSet;
  }

  def getTags(arg: String, esr: ExtractionSentenceRecord): Seq[Type] = {

    val chunkedSentence = getChunkedSentence(esr).getOrElse { return Seq.empty }

    getTags(arg, chunkedSentence)
  }

  var exceptions = 0
  var calls = 0
  
  def getTags(arg: String, chunkedSentence: ChunkedSentence): Seq[Type] = {
    
    calls += 1
    
    val argLocation = locateArgInSentence(arg, chunkedSentence)

    val sentenceText = chunkedSentence.getTokens.mkString(" ")
    
    val sentence = new Sentence(chunkedSentence, sentenceText)
    
    val tagsForSentence = try {
      netagger.findTags(sentence).toList
    } catch {
      case e => {
        exceptions += 1
        e.printStackTrace
        System.err.println("%s\n%s".format(sentenceText, sentence.tokens.map(t => "%s_%s".format(t.string(), t.offset()))))
        val errorRate = (exceptions.toDouble / calls.toDouble) * 100.0
        System.err.println("NER exception rate: %.02f%%".format(errorRate))
        List.empty
      }
    }

    val tagsForArg = tagsForSentence.filter { tag =>
      argLocation.exists(loc => loc.contains(tag.range) || loc.equals(tag.range))
    }
    tagsForArg
  }

  def getWnTag(arg: Seq[PostaggedToken], sentence: ChunkedSentence): String = {
    getWnTag(arg.map(_.string).mkString(" "), sentence)
  }
  
  def getWnTag(arg: String, esr: ExtractionSentenceRecord): String = {
    val sentence = getChunkedSentence(esr).getOrElse { return "other_noun" };
    getWnTag(arg, esr)
  }
  
  def getWnTag(arg: String, sentence: ChunkedSentence): String = {
    getTags(arg, sentence).headOption match {
      case Some(nerClass) => {
        if (nerClass.descriptor.equals("Organization")) "organization[n1]"
        else if (nerClass.descriptor.equals("Location")) "location[n1]"
        else if (nerClass.descriptor.equals("Person")) "person[n1]"
        else "other_noun"
      }
      case None => "other_noun"
    }

  }
  // For testing purposes only. This method could be deleted in favor of a better unit test.
  def main(args: Array[String]): Unit = {
    
    val rawEsrs = Source.fromFile(args(0)).getLines
    
    // convert esrs from strings
    val esrs = rawEsrs map { str => new ExtractionSentenceRecord(str) } take (1000000) toSeq;

    val sentences = esrs flatMap { esr => 
      getChunkedSentence(esr).map(f => (esr.arg1, f))
    }
    
    val results = sentences flatMap { case (arg, sentence)  =>
      
      val tags = getTags(arg, sentence).map(_.descriptor())
      if (tags.isEmpty) Seq.empty
      else tags
    } toSet
    
    results foreach println
  }
}