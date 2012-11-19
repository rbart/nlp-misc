package edu.washington.cs.knowitall.nlp.util.relontology

import edu.washington.cs.knowitall.nlp.util.DataExplorerTool
import edu.washington.cs.knowitall.tool.postag.PostaggedToken

class RelOntology(val interfaceA: DataExplorerTool, val interfaceB: WordNetUtils, val cleanUtils: CleanUtils) {

   // Implements the "Core" module described in Relation Ontology Spec
   def findCandidates(relPhrase: String): Iterable[Candidate] = {
     
     val relToken = new PostaggedToken("VB", relPhrase, 0)
     
     // "First sub-module looks up arguments and partitions them into arg-typed contexts"
	 val typeContexts = interfaceA.findTypeContexts(relPhrase)
	 
	 // "Next sub-module finds all candidate relation phrases t2 that are either
	 // synonyms, entailments, or troponyms of t1."
	 val wnSynonyms = interfaceB.getWnSynonyms(relToken)
	 val wnEntailments = interfaceB.getWnHypernyms(relToken)
	 val cleanEntailments = cleanUtils.getCleanEntailments(relPhrase)
	 val wnTroponyms = interfaceB.getWnTroponyms(relToken)
	 
	 // "Next sub-module looks up each t2 in the tuple base, finds arguments 
	 // that match each context, computes statistics."
	 val allT2 = wnSynonyms ++ wnEntailments ++ cleanEntailments ++ wnTroponyms
	
	 Iterable.empty
   }
}