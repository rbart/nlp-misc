package edu.washington.cs.knowitall.nlp.util.relontology

import edu.washington.cs.knowitall.nlp.util.DataExplorerTool
import edu.washington.cs.knowitall.tool.postag.PostaggedToken

class RelOntology(val interfaceA: DataExplorerTool, val interfaceB: WordNetUtils, val cleanUtils: CleanUtils) {

   private val maxNyms = 2 // increase if not debug
  
   // Implements the "Core" module described in Relation Ontology Spec
  // returns candidates in descending order of score
   def findCandidates(relPhrase: String): Seq[ScoredCandidate] = {
     
     val relToken = new PostaggedToken("VB", relPhrase, 0)
     
     // "First sub-module looks up arguments and partitions them into arg-typed contexts"
	 val typeContexts = interfaceA.findTypeContexts(new Nym("parent") { def rel = relPhrase })
	 
	 // "Next sub-module finds all candidate relation phrases t2 that are either
	 // synonyms, entailments, or troponyms of t1."
	 val wnSynonyms = interfaceB.getWnSynonyms(relToken).take(maxNyms)
	 val wnEntailments = interfaceB.getWnEntailments(relToken).take(maxNyms)
	 val cleanEntailments = cleanUtils.getCleanEntailments(relPhrase).take(maxNyms)
	 val wnTroponyms = interfaceB.getWnTroponyms(relToken).take(maxNyms)
	 
	 // "Next sub-module looks up each t2 in the tuple base, finds arguments 
	 // that match each context, computes statistics."
	 val allT2 = wnSynonyms ++ wnEntailments ++ cleanEntailments ++ wnTroponyms
	
	 val nymContexts = allT2.map(t2 => (t2, interfaceA.findTypeContexts(t2)))
	 
	 val candidates = typeContexts.flatMap({ relTc =>
	   nymContexts.flatMap { case (nym, tcs) =>
	     interfaceA.findCandidates(relTc, nym, tcs)
	   }
	 })
	 
	 // "Next sub-module applies a classifier (stub for now) to each arg-typed t2 entailing t1."
	 val scoredCandidates = candidates map StubClassifier.score
	 
	 // 6.	Output is a list of arg-typed t2 ranked by probability.
	 scoredCandidates.toSeq.sortBy(-_.score)
   }
}

object RelOntology {
  
  import edu.washington.cs.knowitall.nlp.util.DataExplorerTool
  import scopt.OptionParser
  
  lazy val relOntology = new RelOntology(DataExplorerTool.fastTool, WordNetUtils.defaultInstance, CleanUtils.defaultBoth)
  
  def main(args: Array[String]): Unit = {
    
    var relPhrase = ""
    val parser = new OptionParser("RelOntology") {
      arg("relPhrase", "relPhrase", { str => relPhrase = str })
    }
    if (!parser.parse(args)) return;
    
    relOntology.findCandidates(relPhrase) foreach println;
  }
  
}