package edu.washington.cs.knowitall.nlp.util

import scopt.OptionParser
import edu.washington.cs.knowitall.nlp.ChunkedSentence

import edu.washington.cs.knowitall.browser.hadoop.scoobi.TypeContextAggregator

import scala.collection.JavaConversions._

// performs the task of stephen's command line interface "B" -
// which is to take as input a relation phrase and return
// wordnet info such as synonyms, entailments, 

object RelExplorer {

  def main(args: Array[String]) {
    
    var relPhrase = "";
    
    val parser = new OptionParser("Rel Explorer (Interface B)") {
      arg("relPhrase", "relation phrase", { str => relPhrase = str })
    }
    
    if (!parser.parse(args)) return
    
    val tool = DataExplorerTool.defaultTool
    
    // retrieve REGs from the backend
    val regs = tool.relQueryBackend(relPhrase) filter tool.filterExactRel(relPhrase)
    // convert these to ArgContext objects.
    val argContexts = regs.map { reg =>
        // get the chunkedsentence
      val chunkedSent = tool.TypedReg.chunkedSentences(reg).head
      val extr = reg.instances.head.extraction
      ArgContext(extr.arg1Tokens, extr.arg2Tokens, chunkedSent)
    }
    
    // convert these to typecontexts.
    val typeContexts = TypeContextAggregator.buildTypeContext(relPhrase, argContexts.iterator)
    
    // determine the actual relation tokens
    val relTokens = regs.head.instances.head.extraction.relTokens
    
    // now convert to freqreltypecontexts
    val freqRelContexts = typeContexts.map { tc =>
      FreqRelTypeContext(-1, relTokens, tc)
    } toSeq
    
    // now use existing code to produce output
    val wordNetHelper = WordNetHelper.getInstance
    
    wordNetHelper.outputAllRows(freqRelContexts) foreach println
  }
}




