################################################################################
#' Condorcet Method
#'
#' Determines the winner by comparing every candidate head-to-head against
#' every other candidate. In each pairwise contest the candidate preferred by a
#' strict majority of voters wins; the **Condorcet winner** is the candidate who
#' beats all others. Such a candidate need not exist, in which case `winners` is
#' empty. A Copeland-style score (wins minus losses) is always computed so the
#' full field is ordered even when no Condorcet winner exists.
#'
#' @inheritParams voting-params
#' @param return_pairwise Logical. If `TRUE`, two extra components are attached
#'   to the result: `$pairwise`, the candidate-by-candidate matrix where
#'   `[i, j]` is `1` iff candidate `i` beats candidate `j` head-to-head, and
#'   `$pairwise_margins`, the matrix whose `[i, j]` entry counts the voters who
#'   prefer candidate `i` over candidate `j`. Defaults to `FALSE`.
#'
#' @return An object of class `"condorcet_result"`, a list with elements
#'   `summary` (a data frame of candidates ordered by Copeland score, with
#'   columns `candidate`, `wins`, `losses`, `ties`, `rank` and `status`),
#'   `winners` (name(s) of the Condorcet winner, or `character(0)` if none),
#'   `losers` (name(s) of the Condorcet loser, or `character(0)` if none),
#'   `n_voters`, `n_valid`, `n_candidates`, and `method` (a list recording the
#'   inferred `type` and the `ties` rule). When `return_pairwise = TRUE` the
#'   elements `pairwise` and `pairwise_margins` are also included. Print the
#'   object or call [summary()] on it for a formatted results table.
#'
#' @seealso [borda()], [irv()], [approval()]
#'
#' @examples
#' ballots <- gen_ranks(n_voters = 30, n_candidates = 4, seed = 1)
#' condorcet(ballots)
#'
#' condorcet(ballots, return_pairwise = TRUE)
#'
#' @export
condorcet <- function(
    x,
    type = c("auto", "rank", "utility", "approval"),
    ties = c("random", "lexicographic", "all"),
    return_pairwise = FALSE
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
  # Per-type validation
  switch(
    inferred_type,
    approval = {
      if (!is.logical(x) && !is_binary_matrix(x)) {
        stop("`type = \"approval\"` requires a logical matrix or a {0, 1} numeric matrix.",
             call. = TRUE)
      }
    },
    rank = {
      vals <- x[!is.na(x) & x != 0]
      if (length(vals)) {
        if (!all(abs(vals - round(vals)) < .Machine$double.eps^0.5)) {
          stop("`type = \"rank\"` requires integer ranks.", call. = TRUE)
        }
        if (any(vals < 0)) {
          stop("Negative ranks are not allowed.", call. = TRUE)
        }
        if (any(vals > n_candidates)) {
          stop("Ranks must not exceed the number of candidates.", call. = TRUE)
        }
      }
      n_per_row <- rowSums(!is.na(x) & x > 0)
      n_uniq_per_row <- apply(x, 1L, function(row) length(unique(row[!is.na(row) & row > 0])))
      if (any(n_per_row != n_uniq_per_row)) {
        bad <- which(n_per_row != n_uniq_per_row)[1]
        stop(sprintf("Voter %d has duplicate ranks; each rank must appear at most once per voter.", bad),
             call. = TRUE)
      }
    },
    utility = invisible(NULL)
  )
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Count valid voters (at least one non-NA entry)
  n_valid <- sum(apply(x, 1L, function(row) any(!is.na(row))))
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Pairwise margins matrix: margins [i, j] = voters who prefer candidate i
  # over candidate j
  margins = matrix(0L, nrow = n_candidates, ncol = n_candidates,
                   dimnames = list(cand_names, cand_names))
  prefers_i <- switch(
    inferred_type,
    rank = function(ci, cj) !is.na(ci) & !is.na(cj) & ci > 0 & cj > 0 & ci < cj,
    utility = function(ci, cj) !is.na(ci) & !is.na(cj) & ci > cj,
    approval = function(ci, cj) !is.na(ci) & !is.na(cj) & ci == 1 & cj == 0
  )

  for (i in seq_len(n_candidates)) {
    col_i <- x[, i]
    for (j in seq_len(n_candidates)) {
      if (i == j) next
      col_j <- x[, j]
      margins[i, j] <- sum(prefers_i(col_i, col_j))
    }
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Pairwise wins matrix: wins [i, j] = 1 iff strict majority of voters who
  # expressed a preference between i and j prefer i. Pairs that tie
  # head-to-head leave both cells at 0
  pairwise_wins <- matrix(0L, nrow = n_candidates, ncol = n_candidates,
                          dimnames = list(cand_names, cand_names))
  for (i in seq_len(n_candidates)) {
    for (j in seq_len(n_candidates)) {
      if (i == j) next
      if (margins[i, j] > margins[j, i]) {
        pairwise_wins[i, j] <- 1L
      }
    }
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Per-candidate counts of pairwise wins, losses, and head-to-head ties
  wins_count <- rowSums(pairwise_wins)
  losses_count <- colSums(pairwise_wins)
  ties_count <- (n_candidates - 1L) - wins_count - losses_count
  if (n_candidates == 1L) {
    ties_count <- 0L
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Condorcet winner and Condorcet loser
  winner_idx <- which(wins_count == n_candidates - 1L)
  loser_idx <- which(losses_count == n_candidates - 1L)

  pick <- function(idx) {
    if (!length(idx)) return(character(0))
    nm <- cand_names[idx]
    if (length(nm) == 1L) return(nm)
    resolve_ties(nm, ties)
  }

  winners <- pick(winner_idx)
  losers <- pick(loser_idx)

  # Single candidate case
  if (n_candidates == 1L) {
    losers <- character(0)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Summary table - Copeland-style ordering by (wins - losses), then by wins.
  copeland <- wins_count - losses_count
  ord <- order(-copeland, -wins_count, method = "radix")

  status <- rep("", n_candidates)
  status[winner_idx] <- "winner"
  status[loser_idx] <- "loser"
  if (n_candidates == 1L) {
    status[1L] <- if (length(winners)) "winner" else ""
  }

  rank_by_copeland <- rank(-copeland, ties.method = "min")

  summary_df <- data.frame(
    candidate = factor(cand_names[ord], levels = cand_names[ord], ordered = TRUE),
    wins = as.integer(wins_count)[ord],
    losses = as.integer(losses_count)[ord],
    ties = as.integer(ties_count)[ord],
    rank = as.integer(rank_by_copeland)[ord],
    status = status[ord],
    stringsAsFactors = FALSE
  )

  rownames(summary_df) <- NULL
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Output assembly
  out <- list(
    summary = summary_df,
    winners = winners,
    losers = losers,
    n_voters = n_voters,
    n_valid = n_valid,
    n_candidates = n_candidates,
    method = list(type = inferred_type, ties = ties)
  )

  if (isTRUE(return_pairwise)) {
    out$pairwise <- pairwise_wins
    out$pairwise_margins <- margins
  }

  class(out) <- "condorcet_result"
  out
  #-----------------------------------------------------------------------------
}
################################################################################

################################################################################
# Printer for condorcet_result
#' @export
print.condorcet_result <- function(x, digits = 1, ...) {
  stopifnot(inherits(x, "condorcet_result"))
  .condorcet_pretty_print(x, digits = digits)
  invisible(x)
}
################################################################################

################################################################################
# Summary for condorcet_result
#' @export
summary.condorcet_result <- function(object, digits = 1, ...) {
  stopifnot(inherits(object, "condorcet_result"))
  .condorcet_pretty_print(object, digits = digits, title = "Condorcet")
  invisible(object)
}
################################################################################

################################################################################
# Internal formatter
#' @noRd
.condorcet_pretty_print <- function(x, digits = 1, title = "Condorcet") {

  #-----------------------------------------------------------------------------
  # Summary table
  df <- x$summary
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Column access
  candidate <- df$candidate
  wins      <- df$wins
  losses    <- df$losses
  ties_v    <- df$ties
  rank_v    <- df$rank
  status_v  <- df$status
  is_winner <- candidate %in% x$winners
  is_loser  <- candidate %in% x$losers
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Totals
  n_valid <- if (!is.null(x$n_valid)) x$n_valid else 0L
  n_total <- if (!is.null(x$n_voters)) x$n_voters else n_valid
  n_cand  <- if (!is.null(x$n_candidates)) x$n_candidates else length(candidate)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Calculate dynamic width based on info line
  info_line <- sprintf(
    " Voters: %d/%d valid  \u2502  Candidates: %d  \u2502  Type: %s  \u2502  Ties: %s",
    n_valid, n_total, n_cand, x$method$type, x$method$ties
  )
  info_width <- nchar(info_line)

  table_width <- max(76, info_width + 2)
  candidate_width <- 18 + (table_width - 76)

  # Header
  cat("\n")
  header_line <- strrep("\u2550", table_width)
  cat(header_line, "\n", sep = "")
  cat(" ", title, "\n", sep = "")
  cat(header_line, "\n", sep = "")

  # Info line
  cat(info_line, "\n")

  separator_line <- strrep("\u2500", table_width)
  cat(separator_line, "\n", sep = "")
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Table header
  header_fmt <- sprintf(
    " %%3s  %%-%ds  %%5s  %%6s  %%4s  %%4s  %%-8s\n",
    candidate_width
  )
  cat(sprintf(header_fmt, "#", "Candidate", "Wins", "Losses", "Ties", "Rank", "Status"))
  cat(separator_line, "\n", sep = "")
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Table rows
  row_fmt <- sprintf(
    " %%3d  %%-%ds  %%5d  %%6d  %%4d  %%4d  %%-8s\n",
    candidate_width
  )

  for (i in seq_along(candidate)) {
    mark <- if (is_winner[i]) {
      "winner X"
    } else if (is_loser[i]) {
      "loser  -"
    } else {
      ""
    }
    cat(sprintf(row_fmt, i,
                as.character(candidate[i]),
                wins[i], losses[i], ties_v[i],
                rank_v[i], mark))
  }
  cat(separator_line, "\n", sep = "")
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Winner / loser announcement
  winner_txt <- if (length(x$winners)) {
    paste(x$winners, collapse = ", ")
  } else {
    "no Condorcet winner (cycle or pairwise tie)"
  }
  loser_txt <- if (length(x$losers)) {
    paste(x$losers, collapse = ", ")
  } else {
    "no Condorcet loser"
  }

  cat("\nCondorcet winner: ", winner_txt, "\n", sep = "")
  cat(  "Condorcet loser : ", loser_txt,  "\n\n", sep = "")
  #-----------------------------------------------------------------------------
}
################################################################################
