################################################################################
gen_approvals <- function(
    n_voters,
    n_candidates,
    p_approve = 0.5,
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
      stop(sprintf("`%s` must be a single integer >=1.", name), call. = FALSE)
    }
    as.integer(round(value))
  }
  n_voters <- check_count(n_voters, "n_voters")
  n_candidates <- check_count(n_candidates, "n_candidates")
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Validate p_approve
  if (!is.numeric(p_approve) || length(p_approve) != 1L || is.na(p_approve) ||
      p_approve < 0 || p_approve > 1) {
    stop("`p_approve` must be a single number in [0, 1].", call. = FALSE)
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
      stop("`seed` must be NULL or a single numeric value.",
           call. = FALSE)
    }
    set.seed(seed)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Draw approvals
  a <- matrix(
    runif(n_voters * n_candidates) < p_approve,
    nrow = n_voters,
    ncol = n_candidates
  )
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Abstentions
  if (p_abstain > 0) {
    abstain <- runif(n_voters) < p_abstain
    a[abstain, ] <- NA
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Warn if ant voter approves no candidate
  empty_rows <- which(rowSums(a, na.rm = TRUE) == 0L)
  if (length(empty_rows)) {
    warning(sprintf(
      "%d voter(s) approved no candidate (empty ballots); voting functions will treat them as invalid.",
      length(empty_rows)), call. = FALSE)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Dimension names
  colnames(a) <- if (is.null(candidate_names)) {
    paste0("Candidate_", seq_len(n_candidates))
  } else {
    candidate_names
  }
  rownames(a) <- if (is.null(voter_names)) {
    paste0("Voter_", seq_len(n_voters))
  } else {
    voter_names
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  return(a)
  #-----------------------------------------------------------------------------
}
################################################################################
