################################################################################
gen_ranks <- function(
    n_voters,
    n_candidates,
    n_ranked = NULL,
    p_abstain = 0,
    candidate_names = NULL,
    voter_names = NULL,
    seed = NULL
) {
  
  #-----------------------------------------------------------------------------
  # validate n_voters and n_candidates
  check_count <- function(value, name) {
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        value < 1 || abs(value - round(value)) > .Machine$double.eps^0.5) {
      stop(sprintf("`%s` must be a single integer >= 1.", name), call. = FALSE)
    }
    as.integer(round(value))
  }
  
  n_voters <- check_count(n_voters, "n_voters")
  n_candidates <- check_count(n_candidates, "n_candidates")
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # Validate n_ranked
  if (!is.null(n_ranked)) {
    if (!is.numeric(n_ranked) || length(n_ranked) != 1L || is.na(n_ranked) ||
        n_ranked < 1 || abs(n_ranked - round(n_ranked)) > .Machine$double.eps^0.5) {
      stop("`n_ranked`")
    }
  }
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # Validate p_abstain
  if (!is.numeric(p_abstain) || length(p_abstain) != 1L || is.na(p_abstain) ||
      p_abstain < 0 || p_abstain > 1) {
    stop("`p_abstain` must be a single number in [0, 1].", call. = FALSE)
  }
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # Validate names
  if (!is.null(candidate_names) &&
      (!is.character(candidate_names) || length(candidate_names) != n_candidates)) {
    stop("`candidate_names` must be NULL or a character vector of length n_candidates.",
         call. = FALSE)
  }
  if (!is.null(voter_names) &&
      (!is.character(voter_names) || length(voter_names) != n_voters)) {
    stop("`voter_names` must be NULL or a character vector of length n_voters.",
         call. = FALSE)
  }
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # Validate seed
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
      stop("`seed` must be NULL or a single numeric value.", call. = FALSE)
    }
    set.seed(seed)
  }
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # Latent utilities
  u <- matrix(
    runif(n_voters * n_candidates),
    nrow = n_voters,
    ncol = n_candidates
  )
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # Choose voters who abstain entirely (whole row NA)
  abstain <- if (p_abstain > 0) {
    runif(n_voters) < p_abstain
  } else {
    rep(FALSE, n_voters)
  }
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # Convert latent utilities to ranks
  r <- matrix(NA_real_, nrow = n_voters, ncol = n_candidates)
  for (i in seq_len(n_voters)) {
    if (abstain[i]) next
    r[i, ] <- rank(-u[i, ], ties.method = "first")
  }
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # Truncate to top-k if requested
  if (!is.null(n_ranked)) {
    r[r > n_ranked] <- NA_real_
  }
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # Warning if any voter ends up with no ranked candidate
  empty_rows <- which(rowSums(!is.na(r)) == 0L)
  if (length(empty_rows)) {
    warning(sprintf(
      "%d voter(s) ranked no candidate (all-NA rows).",
      length(empty_rows)), call. = FALSE)
  }
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # Dimension names
  colnames(r) <- if (is.null(candidate_names)) {
    paste0("Candidate_", seq_len(n_candidates))
  } else {
    candidate_names
  }
  rownames(r) <- if (is.null(voter_names)) {
    paste0("Voter_", seq_len(n_voters))
  } else {
    voter_names
  }
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  return(r)
  #-----------------------------------------------------------------------------
}
################################################################################