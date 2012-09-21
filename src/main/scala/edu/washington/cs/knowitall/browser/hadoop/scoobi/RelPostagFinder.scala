package edu.washington.cs.knowitall.browser.hadoop.scoobi

import scopt.OptionParser
import com.nicta.scoobi.Scoobi._
import util.ExtractionSentenceRecord
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer
import edu.washington.cs.knowitall.tool.postag.PostaggedToken
import scala.collection.mutable
import scala.Array.canBuildFrom
import scala.Option.option2Iterable

import edu.washington.cs.knowitall.nlp.util.RelationTabulator._
import edu.washington.cs.knowitall.nlp.util.ArgContext.joinTokensAndPostags

import edu.washington.cs.knowitall.nlp.util.RelTupleProcessor._
import RelationCounter._
import RelTupleTabulator.stemToken

// a hack to annotate relation strings with their most common part-of-speech signature
object RelPostagFinder extends ScoobiApp {

  
    // (rel tokens, (rel.toString, arg1String, arg2String))
  def toRelPostags(inputRecord: String): Option[(String, String)] = {
    try {
      val esr = new ExtractionSentenceRecord(inputRecord)
      val relTokens = joinTokensAndPostags(esr.norm1Rel, esr.norm1RelPosTags) filter filterTokens map stemToken
      val relString = relTokens.map(_.string).mkString(" ")
      def relPostags = relTokens.map(_.postag).mkString(" ")
      if (relTokens.isEmpty || relString.startsWith("'")) None
      else {
        Some(relString, relPostags)
      }
    } catch { case e: Exception => { e.printStackTrace; None } }
  }
  
  def freqTypeToRelFreqType(pair: (Int, TypeContext)): (String, String) = {
    val freq = pair._1
    val typeContext = pair._2
    (typeContext.rel, Seq(freq, typeContext.toString).mkString("\t"))
  }
  
  def run(): Unit = {
    
    var freqTypeContexts: String = ""
    var rawRecords: String = ""
    var outputPath: String = ""
      
    val parser = new OptionParser() {
      arg("freqTypeContexts", "frequency, typecontext tab-delimited", { str => freqTypeContexts = str })
      arg("rawRecords", "raw niranjan ExtractionSentenceRecords objects", { str => rawRecords = str })
      arg("outputPath", "where to write the output", { str => outputPath = str })
    }
    
    if (!parser.parse(args)) return
    System.err.println("freqTypeContexts: %s".format(freqTypeContexts))
    System.err.println("rawRecords: %s".format(rawRecords))
    System.err.println("outputPath: %s".format(outputPath))
    
    val freqTypeContextsRaw = fromTextFile(freqTypeContexts)
    val freqTypeContextsParsed = freqTypeContextsRaw flatMap loadFreqAndContext
    val relFreqTypePairs = freqTypeContextsParsed map freqTypeToRelFreqType
    
    val rawEsrs = fromTextFile(rawRecords)
    val relPostags = rawEsrs flatMap toRelPostags

    // (rel, (relPostags, freq\tTypeContext)
    val coGrouped = relPostags.coGroup(relFreqTypePairs)

    val output = coGrouped.flatMap {
      case (relString, (relPostags, freqTypeStrings)) =>

        relPostags.headOption match {
          case Some(relPostag) => freqTypeStrings.map { freqTypeString =>
            val (freq, typeContext) = loadFreqAndContext(freqTypeString).get
            Seq(freq, relPostag, typeContext.toString).mkString("\t")
          }
          case None => Seq.empty
        }
    }
    
    persist(toTextFile(output, outputPath + "/"))
  }
}