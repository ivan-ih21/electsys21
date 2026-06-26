#' Generate random approval ballots
#'
#' Simulates an approval-ballot matrix for testing and demonstration. Each
#' candidate is independently approved by each voter with probability
#' `p_approve`, and voters may abstain entirely.
#'
#' @param n_voters Single positive integer. Number of voters (matrix rows).
#' @param n_candidates Single positive integer. Number of candidates (matrix
#'   columns).
#' @param p_approve Single number in `[0, 1]`. Probability that a voter
#'   approves any given candidate. Defaults to `0.5`.
#' @param p_abstain Single number in `[0, 1]`. Probability that a voter
#'   abstains entirely, leaving that whole row `NA`. Defaults to `0`.
#' @param candidate_names `NULL` or a character vector of length
#'   `n_candidates` giving the column names. Defaults to `NULL`
#'   (`Candidate_1`, `Candidate_2`, ...).
#' @param voter_names `NULL` or a character vector of length `n_voters` giving
#'   the row names. Defaults to `NULL` (`Voter_1`, `Voter_2`, ...).
#' @param seed `NULL` or a single numeric value passed to [set.seed()] for
#'   reproducible output. Defaults to `NULL`.
#'
#' @return A logical matrix with `n_voters` rows and `n_candidates` columns:
#'   `TRUE` marks an approved candidate, `FALSE` a non-approved one, and an
#'   abstaining voter's row is all `NA`. Suitable as the `x` argument of the
#'   voting functions.
#'
#' @seealso [gen_ranks()], [gen_utilities()]
#'
#' @examples
#' gen_approvals(n_voters = 5, n_candidates = 3, seed = 1)
#'
#' # Stricter voters approve fewer candidates
#' gen_approvals(n_voters = 5, n_candidates = 3, p_approve = 0.25, seed = 1)
#'
#' @export
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
