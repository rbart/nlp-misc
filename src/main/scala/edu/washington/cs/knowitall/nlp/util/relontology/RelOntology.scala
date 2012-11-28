package edu.washington.cs.knowitall.nlp.util.relontology

import edu.washington.cs.knowitall.nlp.util.DataExplorerTool2
import edu.washington.cs.knowitall.tool.postag.PostaggedToken

class RelOntology(val interfaceA: DataExplorerTool2, val interfaceB: WordNetUtils, val cleanUtils: CleanUtils, val verbose: Boolean = false) {

  import RelOntology._

  private val maxNyms = 2 // increase if not debug

  // Implements the "Core" module described in Relation Ontology Spec
  // returns candidates in descending order of score
  def findCandidates(relPhrase: String): Seq[ScoredCandidate] = {

    val relToken = new PostaggedToken("VB", relPhrase, 0)

    // "First sub-module looks up arguments and partitions them into arg-typed contexts"
    val typeContexts = interfaceA.findTypeContexts(new Nym("parent") { def rel = relPhrase })
    if (verbose) mod1Verbose(typeContexts)
    
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
      nymContexts.flatMap {
        case (nym, tcs) =>
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
  import edu.washington.cs.knowitall.nlp.util.TypeContext

  def main(args: Array[String]): Unit = {

    var relPhrase = ""
    var verbose = false;
    val parser = new OptionParser("RelOntology") {
      arg("relPhrase", "relPhrase", { str => relPhrase = str })
      opt("v", "verbose output", { verbose = true })
    }
    if (!parser.parse(args)) return ;

    val sortedMap = new edu.washington.cs.knowitall.browser.util.SortedFileMap("/scratch/usr/rbart/argpairs.sorted", 10000)
    val det = new DataExplorerTool2(sortedMap)
    
    val relOntology = new RelOntology(det, WordNetUtils.defaultInstance, CleanUtils.defaultBoth, verbose)

    relOntology.findCandidates(relPhrase) foreach println;
  }

  // Outputs verbose debugging information for the first "sub-module" - e.g.
  // the part that takes the rel phrase and looks up type contexts. 
  def mod1Verbose(typeContexts: Iterable[TypeContext]): Unit = {
    
    val hdr = Seq("num", "freq", "arg1Type", "arg2Type", "arg1 sample", "arg2 sample")
    println(hdr.mkString("\t"))
    
    var num = 0
    var totalFreq = 0
    for (tc <- typeContexts) {
      num += 1
      totalFreq += tc.freq
      val cols = Seq(num.toString, tc.freq.toString, tc.arg1Type, tc.arg2Type, tc.arg1sFormatted, tc.arg2sFormatted)
      println(cols.mkString("\t"))
    }
    
    println("Total Freq %d\n".format(totalFreq))
  }
  
  def mod2Verbose(nyms: Seq[Nym]): Unit = {
    
    val ents = nyms.filter(_.isInstanceOf[Entailment])
    val trops = nyms.filter(_.isInstanceOf[Troponym])
    val syns = nyms.filter(_.isInstanceOf[Synonym])
    
    def printHelper(prefix: String, ns: Seq[Nym]) = {
      println("%s (%d): %s".format(prefix, ns.size, ns.mkString(", ")))
    }
    
    printHelper("Entailments", ents)
    printHelper("Troponyms", trops)
    printHelper("Synonyms", syns)
    println()
  }
}