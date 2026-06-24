################################################################################
fptp <- function(
    x,
    type = c("auto", "rank", "utility", "approval"),
    ties = c("random", "lexicographic", "all"),
    return_ranks = FALSE
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
  inferred_type <- switch(type,
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
  ranks <- build_ranks(x, inferred_type)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # First preference extraction
  first_choice_idx <- apply(ranks, 1L, function(row) {
    valid <- which(!is.na(row) & row > 0)
    if (!length(valid)) return(NA_integer_)
    valid[which.min(row[valid])]
  })
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Valid-voter filtering
  valid_voters <- which(!is.na(first_choice_idx))
  n_valid <- length(valid_voters)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Early return when no valid first choices
  if (n_valid == 0L) {
    summary <- data.frame(
      candidate = factor(colnames(x), levels = colnames(x), ordered = TRUE),
      votes = integer(n_candidates),
      percentage = rep(0, n_candidates),
      stringsAsFactors = FALSE
    )
    res <- list(
      summary = summary,
      winners = character(0),
      n_voters = n_voters,
      n_candidates = n_candidates,
      method = list(type = inferred_type,
                    ties = ties)
    )
    class(res) <- "fptp_result"
    return(res)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Vote aggregation
  vote_names <- if (n_valid) {
    cand_names[first_choice_idx[valid_voters]]
  } else {
    character(0)
  }
  vote_counts <- table(factor(vote_names, levels = cand_names))
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Tie detection
  max_votes <- max(as.integer(vote_counts))
  top_idx <- which(vote_counts == max_votes)
  top_names <- names(vote_counts)[top_idx]
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Tie resolution
  winners <- resolve_ties(top_names, ties)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Summary table
  percentage <- if (n_valid) {
    as.numeric(vote_counts) / n_valid * 100
  } else {
    rep(0, n_candidates)
  }
  ord <- order(as.integer(vote_counts), decreasing = TRUE, method = "radix")

  ranks_by_votes <- rank(-as.integer(vote_counts), ties.method = "min")
  ranks_by_votes <- ranks_by_votes[ord]

  df <- data.frame(
    candidate = factor(names(vote_counts)[ord], levels = names(vote_counts[ord]), ordered = TRUE),
    vote = as.integer(vote_counts)[ord],
    percentage = percentage[ord],
    rank = as.integer(ranks_by_votes),
    stringsAsFactors = FALSE
  )
  rownames(df) <- NULL
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Output assembly
  out <- list(
    summary = df,
    winners = winners,
    n_voters = n_voters,
    n_candidates = n_candidates,
    method = list(type = inferred_type,
                  ties = ties)
  )
  if (isTRUE(return_ranks)) {
    out$ranks <- ranks
  }

  class(out) <- "fptp_result"
  out
  #-----------------------------------------------------------------------------

}
################################################################################

################################################################################
# Printer for fptp_result
print.fptp_result <- function(x, digits = 1, ...) {
  stopifnot(inherits(x, "fptp_result"))
  .fptp_pretty_print(x, digits = digits)
  invisible(x)
}
################################################################################

################################################################################
# Summary for fptp_result
summary.fptp_result <- function(x, digits = 1, ...) {
  stopifnot(inherits(x, "fptp_result"))
  .fptp_pretty_print(x, digits = digits, title = "First-Past-The-Post voting")
  invisible(x)
}
################################################################################

################################################################################
# Internal formatter
.fptp_pretty_print <- function(x, digits = 1, title = "First-Past-The-Post voting") {
  #-----------------------------------------------------------------------------
  # Summary table
  df <- x$summary
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Column access
  candidate <- df$candidate
  votes <- df$vote
  percentage <- df$percentage
  rank <- df$rank
  is_winner <- candidate %in% x$winners
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Total numbers
  n_valid <- if (length(votes)) sum(as.integer(votes)) else 0L
  n_total <- if (!is.null(x$n_voters)) x$n_voters else n_valid
  n_cand <- if (!is.null(x$n_candidates)) x$n_candidates else length(candidate)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Calculate dynamic width based on info line
  info_line <- sprintf(" Votes: %d/%d valid  \u2502  Candidates: %d  \u2502  Type: %s  \u2502  Ties: %s",
                       n_valid, n_total, n_cand, x$method$type, x$method$ties)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Render
  print_results_table(
    candidate   = candidate,
    value       = votes,
    percentage  = percentage,
    rank        = rank,
    winners     = x$winners,
    title       = title,
    info_line   = info_line,
    value_label = "Votes",
    digits      = digits
  )
  #-----------------------------------------------------------------------------
}
################################################################################
