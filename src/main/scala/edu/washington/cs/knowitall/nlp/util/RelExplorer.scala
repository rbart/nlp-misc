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
    val extraction = regs.head.instances.head.extraction
    val relTokens = extraction.normTokens(extraction.relInterval)
        
    // now convert to freqreltypecontexts
    val freqRelContexts = typeContexts.map({ tc =>
      // put "other noun" at the bottom of the list.
      val freq = if (tc.arg1Type.equals("other_noun") || tc.arg2Type.equals("other_noun")) -1 else tc.freq
      FreqRelTypeContext(freq, relTokens, tc)
    }).toSeq.sortBy(-_.freq).take(15)
    
    // now use existing code to produce output
    val wordNetHelper = WordNetHelper.getInstance
    
    wordNetHelper.outputAllRows(freqRelContexts) foreach println
  }
}




