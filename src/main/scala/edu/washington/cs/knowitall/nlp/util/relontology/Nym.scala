package edu.washington.cs.knowitall.nlp.util.relontology

/**
 * a "Nym" is a supertype for Hypernyms (entailments), Synonyms, or Troponyms...
 * They wrap a string (e.g. rel phrase) and contain info about their source.
 */
abstract class Nym(val prefix: String/*for toString*/) {
  def rel: String
  override def toString: String = "%s%s".format(prefix, rel)
}

abstract class Synonym(prefix: String) extends Nym(prefix)
case class WnSynonym(val rel:String) extends Synonym("ws:")

abstract class Troponym(prefix: String) extends Nym(prefix)
case class WnTroponym(val rel: String) extends Troponym("wt:")
case class ClTroponym(val rel: String) extends Troponym("ct:")

abstract class Entailment(prefix: String) extends Nym(prefix)
case class WnEntailment(val rel: String) extends Entailment("we:") // wordnet entailment
case class ClEntailment(val rel: String) extends Entailment("ce:") // clean entailment

