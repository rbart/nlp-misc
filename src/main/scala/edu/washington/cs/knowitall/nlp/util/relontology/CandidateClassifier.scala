package edu.washington.cs.knowitall.nlp.util.relontology

abstract class CandidateClassifier {
  def score(candidate: Candidate): ScoredCandidate
}

object StubClassifier extends CandidateClassifier {
  // uses "candidate.balanced" directlty as the score.
  def score(candidate: Candidate): ScoredCandidate = new ScoredCandidate(candidate.balanced, candidate)
}