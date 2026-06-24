################################################################################
# Resolve a winner (or winners) from a set of tied candidates
resolve_ties <- function(candidates, ties) {
  switch(
    ties,
    random        = sample(candidates, 1L),
    lexicographic = sort(candidates)[1L],
    all           = candidates
  )
}
################################################################################
