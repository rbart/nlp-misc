package edu.washington.cs.knowitall.nlp.util.relontology

abstract class CandidateClassifier {
  def score(candidate: Candidate): ScoredCandidate
}

object StubClassifier extends CandidateClassifier {
  def score(candidate: Candidate): ScoredCandidate = new ScoredCandidate(0.0, candidate)
}