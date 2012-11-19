package edu.washington.cs.knowitall.nlp.util.relontology

/**
 * a "Nym" is a supertype for Hypernyms (entailments), Synonyms, or Troponyms...
 * They wrap a string (e.g. rel phrase) and contain info about their source.
 */
sealed abstract class Nym {
  def rel: String
}