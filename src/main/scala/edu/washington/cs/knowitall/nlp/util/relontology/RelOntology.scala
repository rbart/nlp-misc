package edu.washington.cs.knowitall.nlp.util.relontology

import edu.washington.cs.knowitall.nlp.util.DataExplorerTool
import edu.washington.cs.knowitall.tool.postag.PostaggedToken

class RelOntology(val interfaceA: DataExplorerTool, val interfaceB: WordNetUtils, val cleanUtils: CleanUtils) {

   // Implements the "Core" module described in Relation Ontology Spec
  // returns candidates in descending order of score
   def findCandidates(relPhrase: String): Seq[ScoredCandidate] = {
     
     val relToken = new PostaggedToken("VB", relPhrase, 0)
     
     // "First sub-module looks up arguments and partitions them into arg-typed contexts"
	 val typeContexts = interfaceA.findTypeContexts(new Nym("parent") { def rel = relPhrase })
	 
	 // "Next sub-module finds all candidate relation phrases t2 that are either
	 // synonyms, entailments, or troponyms of t1."
	 val wnSynonyms = interfaceB.getWnSynonyms(relToken)
	 val wnEntailments = interfaceB.getWnEntailments(relToken)
	 val cleanEntailments = cleanUtils.getCleanEntailments(relPhrase)
	 val wnTroponyms = interfaceB.getWnTroponyms(relToken)
	 
	 // "Next sub-module looks up each t2 in the tuple base, finds arguments 
	 // that match each context, computes statistics."
	 val allT2 = wnSynonyms ++ wnEntailments ++ cleanEntailments ++ wnTroponyms
	
	 val candidates = typeContexts.flatMap({ relTc =>
	   allT2.flatMap { nym =>
	     interfaceA.findCandidates(relTc, nym)
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
  
  lazy val relOntology = new RelOntology(DataExplorerTool.defaultTool, WordNetUtils.defaultInstance, CleanUtils.defaultBoth)
  
  def main(args: Array[String]): Unit = {
    
    var relPhrase = ""
    val parser = new OptionParser("RelOntology") {
      arg("relPhrase", "relPhrase", { str => relPhrase = str })
    }
    if (!parser.parse(args)) return;
    
    relOntology.findCandidates(relPhrase) foreach println;
  }
  
}