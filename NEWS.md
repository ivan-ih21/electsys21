# electsys21 0.1.0

* Initial release.
* Voting methods for ranked, rated and approval ballots: the D21 (with and
  without minus votes), first-past-the-post, two-round runoff, instant
  runoff, the Borda count, approval voting, majority judgement and the
  Condorcet method.
* Automatic detection of ballot input type (ranking, cardinal utilities,
  approvals and scores), configurable tie-breaking and tidy summaries of
  the results.
* Ballot generators for simulation: `gen_ranks()`, `gen_utilities()` and
  `gen_approvals()`.
