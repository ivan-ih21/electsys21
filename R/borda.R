################################################################################
#' Borda Count
#'
#' Runs an election using the Borda Count. Each voter's ballot is turned into
#' a ranking and each candidate earns points according to their position: with
#' `n` candidates, a candidate ranked first receives `n - 1` points, the second
#' `n - 2`, down to `0` for the last. Points are summed across all voters and
#' the candidate with the highest total wins. Utility and approval ballots are
#' converted to rankings internally before scoring.
#'
#' @inheritParams voting-params
#' @param return_scores Logical. If `TRUE`, the per-voter Borda points matrix
#'   (`n_voters` by `n_candidates`) used for scoring is attached to the result
#'   as `$scores`. Defaults to `FALSE`.
#'
#' @return An object of class `"borda_result"`, a list with elements:
#'   `summary` (a data frame of candidates ordered by total score, with columns
#'   `candidate`, `score`, `percentage` and `rank`), `winners` (a character
#'   vector of the winning candidate name(s), or `character(0)` if no voter
#'   provided a valid preference), `n_voters`, `n_valid`, `n_candidates`, and
#'   `method` (a list recording the inferred `type` and the `ties` rule).
#'   Optionally includes `$ranks` (when `return_ranks = TRUE`) and `$scores`
#'   (when `return_scores = TRUE`). Print the object or call [summary()] on it
#'   for a formatted results table.
#'
#' @seealso [fptp()], [condorcet()], [approval()]
#'
#' @examples
#' ballots <- gen_ranks(n_voters = 50, n_candidates = 4, seed = 1)
#' borda(ballots)
#'
#' # Resolve ties alphabetically and keep the derived ranking matrix
#' borda(ballots, ties = "lexicographic", return_ranks = TRUE)
#'
#' @export
borda <- function(
    x,
    type = c("auto", "rank", "utility", "approval"),
    ties = c("random", "lexicographic", "all"),
    return_ranks = FALSE,
    return_scores = FALSE
) {

  #-----------------------------------------------------------------------------
  # Argument matching
  type <- match.arg(type)
  ties <- match.arg(ties)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Input preparation and validation
  x <- prepare_ballots(x)
  n_voters     <- nrow(x)
  n_candidates <- ncol(x)
  cand_names   <- colnames(x)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Input type inference
  inferred_type <- switch(
    type,
    auto = {
      if (is.logical(x)) "approval"
      else if (is_binary_matrix(x)) "approval"
      else if (is_ranking_matrix(x)) "rank"
      else "utility"
    },
    rank = "rank",
    utility = "utility",
    approval = "approval"
  )
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Build ranking matrix
  ranks <- build_ranks(x, inferred_type, context = "borda")
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Borda points vector: rank k -> n_candidates - k
  # (rank 1 = n-1 points, rank n = 0 points)
  borda_points <- as.numeric((n_candidates - 1L):0L)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Per-voter score matrix
  # For each voter and candidate: score = points[rank], or 0 if not ranked
  # Ranks are clamped to [1, n_candidates] for safety in case rank() input
  # passes loose type='rank' validation with out-of-range values
  score_matrix <- matrix(0, nrow = n_voters, ncol = n_candidates)
  colnames(score_matrix) <- cand_names

  for (i in seq_len(n_voters)) {
    row <- ranks[i, ]
    pos <- which(!is.na(row) & row > 0)
    if (!length(pos)) next
    idx <- pmin(pmax(as.integer(round(row[pos])), 1L), n_candidates)
    score_matrix[i, pos] <- borda_points[idx]
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Aggregate scores and count valid voters
  total_scores <- colSums(score_matrix, na.rm = TRUE)
  voter_has_rank <- rowSums(!is.na(ranks) & ranks > 0) > 0L
  n_valid <- sum(voter_has_rank)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Early return when no voter provided a valid preference.
  # Borda-specific: when n_valid > 0 but sum(total_scores) == 0 (e.g., a
  # single-candidate election where rank 1 yields n-1 = 0 points), the main
  # flow below handles the all-tied-at-zero case via the `ties` policy.
  if (n_valid == 0L) {
    summary_df <- data.frame(
      candidate = factor(cand_names, levels = cand_names, ordered = TRUE),
      score = numeric(n_candidates),
      percentage = rep(0, n_candidates),
      rank = rep(1L, n_candidates),
      stringsAsFactors = FALSE
    )
    out <- list(
      summary = summary_df,
      winners = character(0),
      n_voters = n_voters,
      n_valid = n_valid,
      n_candidates = n_candidates,
      method = list(type = inferred_type,
                    ties = ties)
    )
    if (isTRUE(return_ranks)) out$ranks <- ranks
    if (isTRUE(return_scores)) out$scores <- score_matrix
    class(out) <- "borda_result"
    return(out)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Summary table
  total_pool <- sum(total_scores)
  percentage <- if (total_pool > 0) {
    as.numeric(total_scores) / total_pool * 100
  } else {
    rep(0, n_candidates)
  }

  ord <- order(total_scores, decreasing = TRUE, method = "radix")
  ranks_by_score <- rank(-total_scores, ties.method = "min")[ord]

  df <- data.frame(
    candidate = factor(cand_names[ord], levels = cand_names[ord], ordered = TRUE),
    score = as.numeric(total_scores)[ord],
    percentage = as.numeric(percentage)[ord],
    rank = as.integer(ranks_by_score),
    stringsAsFactors = FALSE
  )
  rownames(df) <- NULL
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Winner(s)
  max_score <- max(total_scores)
  top_idx <- which(total_scores == max_score)
  top_names <- cand_names[top_idx]

  winners <- resolve_ties(top_names, ties)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Output assembly
  out <- list(
    summary = df,
    winners = winners,
    n_voters = n_voters,
    n_valid = n_valid,
    n_candidates = n_candidates,
    method = list(type = inferred_type,
                  ties = ties)
  )

  if (isTRUE(return_ranks)) out$ranks <- ranks
  if (isTRUE(return_scores)) out$scores <- score_matrix

  class(out) <- "borda_result"
  out
  #-----------------------------------------------------------------------------
}
################################################################################

################################################################################
# Printer for borda_result
#' @export
print.borda_result <- function(x, digits = 1, ...) {
  stopifnot(inherits(x, "borda_result"))
  .borda_pretty_print(x, digits = digits)
  invisible(x)
}
################################################################################

################################################################################
# Summary for borda_result
#' @export
summary.borda_result <- function(object, digits = 1, ...) {
  stopifnot(inherits(object, "borda_result"))
  .borda_pretty_print(object, digits = digits, title = "Borda Count")
  invisible(object)
}
################################################################################

################################################################################
# Internal formatter
#' @noRd
.borda_pretty_print <- function(x, digits = 1, title = "Borda Count") {

  #-----------------------------------------------------------------------------
  # Summary table
  df <- x$summary
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Column access
  candidate <- df$candidate
  score <- df$score
  percentage <- df$percentage
  rank_v <- df$rank
  is_winner <- candidate %in% x$winners
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Total numbers
  n_valid <- if (!is.null(x$n_valid)) x$n_valid else 0L
  n_total <- if (!is.null(x$n_voters)) x$n_voters else n_valid
  n_cand  <- if (!is.null(x$n_candidates)) x$n_candidates else length(candidate)
  total_score <- if (length(score)) sum(as.numeric(score)) else 0
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Info line
  info_line <- sprintf(" Voters: %d/%d valid  │  Candidates: %d  │  Type: %s  │  Ties: %s",
                       n_valid, n_total, n_cand, x$method$type, x$method$ties)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Render
  print_results_table(
    candidate   = candidate,
    value       = score,
    percentage  = percentage,
    rank        = rank_v,
    winners     = x$winners,
    title       = title,
    info_line   = info_line,
    value_label = "Score",
    digits      = digits,
    extras      = function() {
      if (n_valid > 0L) {
        cat(sprintf(" Score per voter (avg): %.2f\n", total_score / n_valid))
      }
    }
  )
  #-----------------------------------------------------------------------------
}
################################################################################
