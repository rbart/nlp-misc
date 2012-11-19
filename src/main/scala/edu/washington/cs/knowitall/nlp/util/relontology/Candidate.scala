package edu.washington.cs.knowitall.nlp.util.relontology

import edu.washington.cs.knowitall.nlp.util.TypeContext

/**
 * A "Candidate" represents a possible node in the graph,
 * and contains information about distributional similarity
 * with it's parent (a typecontext)
 */
class Candidate(
    val arg1Type: WordNetClass, 
    val relNym: Nym, 
    val arg2Type: WordNetClass,
    val parent: TypeContext,
    val freq: Int,
    val freqOverlap: Int,
    val PMI: Double,
    val condProb: Double, // p(parent|this)
    val balanced: Double 
    ) 



