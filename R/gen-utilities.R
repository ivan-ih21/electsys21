################################################################################
gen_utilities <- function(
    n_voters,
    n_candidates,
    scale = NULL,
    p_abstain = 0,
    candidate_names = NULL,
    voter_names = NULL,
    seed = NULL
) {
  
  #-----------------------------------------------------------------------------
  # Validate n_voters and n_candidates
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
  # Validate scale
  if (!is.null(scale)) {
    if (!is.numeric(scale) || length(scale) != 2L || any(is.na(scale)) ||
        scale[1L]  >= scale[2L]) {
      stop("`scale` nust be NULL or a numeric vector c(lo, hi) with lo < hi.",
           call. = FALSE)
    }
  }
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # Validate p_abstain
  if (!is.numeric(p_abstain) || length(p_abstain) != 1L || is.na(p_abstain) ||
      p_abstain < 0 || p_abstain >1) {
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
  # Draw utilities
  lo <- if (is.null(scale)) 0 else scale[1L]
  hi <- if (is.null(scale)) 1 else scale[2L]
  u <- matrix(
    runif(n_voters * n_candidates, min = lo, max = hi),
    nrow = n_voters,
    ncol = n_candidates
  )
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # Abstentions
  if (p_abstain > 0) {
    mask <- matrix(
      runif(n_voters * n_candidates) < p_abstain,
      nrow = n_voters,
      ncol = n_candidates
    )
    u[mask] <- NA_real_
    
    # Warn if any voter ends up with no expressed utility at all
    empty_rows <- which(rowSums(!is.na(u)) == 0L)
    if (length(empty_rows)) {
      warning(sprintf(
        "%d voter(s) abstained on every candidate (all-NA rows).",
        length(empty_rows)), call. = FALSE)
    }
  }
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # Dimension names
  colnames(u) <- if (is.null(candidate_names)) {
    paste0("Candidate_", seq_len(n_candidates))
  } else {
    candidate_names
  }
  rownames(u) <- if (is.null(voter_names)) {
    paste0("Voter_", seq_len(n_voters))
  } else {
    voter_names
  }
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  return(u)
  #-----------------------------------------------------------------------------
}
################################################################################