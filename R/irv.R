################################################################################
#' Instant Runoff Voting
#'
#' Runs an Instant Runoff Voting. Each voter ranks the candidates, and in each
#' round the candidate with the fewest first-preference votes among the active
#' candidates is eliminated and their ballots transfer to the next-ranked active
#' candidate. The process repeats until a candidate's share of the active
#' (non-exhausted) votes **strictly exceeds** the `majority` threshold or only
#' one candidate remains.
#'
#' @inheritParams voting-params
#' @param elim_ties Character string giving the tie-breaking rule applied at the
#'   elimination step when two or more active candidates share the lowest vote
#'   count in a round. One of `"random"` (the default; eliminate one tied
#'   candidate at random), `"lexicographic"` (eliminate the alphabetically
#'   first) or `"all"` (batch elimination: simultaneously remove every candidate
#'   tied at the minimum).
#' @param majority Single numeric value in `[0, 1]` giving the fraction of
#'   active votes a candidate must **strictly exceed** to win a round. Defaults
#'   to `0.5`, so a candidate needs more than 50% of the active votes.
#' @param return_history Logical. If `TRUE`, the result gains a `$history`
#'   element: a character matrix (`n_voters` x `n_rounds`) recording which active
#'   candidate received each voter's vote in each round, or `NA` where the
#'   ballot was exhausted. Defaults to `FALSE`.
#'
#' @return An object of class `"irv_result"`, a list with the elements:
#'   - `summary`: a data frame with columns `candidate`, `vote`, `percentage`,
#'     `rank` and `eliminated_in_round`, where `vote` and `percentage` reflect
#'     each candidate's score at their last appearance.
#'   - `winners`: character vector of the winning candidate name(s).
#'   - `n_voters`: total number of voters.
#'   - `n_valid`: number of voters with at least one valid preference.
#'   - `n_candidates`: total number of candidates.
#'   - `rounds`: a list with one entry per round (active set, vote counts,
#'     percentages, totals, exhausted ballots and candidate(s) eliminated).
#'   - `counts`: candidate-by-round matrix of integer vote counts.
#'   - `percentages`: candidate-by-round matrix of active-vote percentages.
#'   - `method`: a list recording `type`, `ties`, `elim_ties` and `majority`.
#'   - `history`: present only when `return_history = TRUE` (see that argument).
#'
#'   Print the object or call [summary()] on it for a formatted results table.
#'
#' @seealso [fptp()], [tworound()], [borda()]
#'
#' @examples
#' ballots <- gen_ranks(n_voters = 40, n_candidates = 4, seed = 1)
#' result <- irv(ballots)
#' result
#'
#' # Deterministic tie-breaking and round-by-round history
#' irv(ballots, ties = "lexicographic", elim_ties = "lexicographic",
#'     return_history = TRUE)
#'
#' @export
irv <- function(
    x,
    type = c("auto", "rank", "utility", "approval"),
    ties = c("random", "lexicographic", "all"),
    elim_ties = c("random", "lexicographic", "all"),
    majority = 0.5,
    return_history = FALSE
) {

  #-----------------------------------------------------------------------------
  # Argument matching
  type <- match.arg(type)
  ties <- match.arg(ties)
  elim_ties <- match.arg(elim_ties)

  if (!is.numeric(majority) || length(majority) != 1L || is.na(majority) ||
      majority < 0 || majority > 1) {
    stop("`majority` must be a single non-NA numeric in [0, 1].", call. = TRUE)
  }
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
  inferred_type <- switch (
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
  ranks <- build_ranks(x, inferred_type)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Count valid voters (with at least one positive non-NA rank)
  voter_has_rank <- rowSums(!is.na(ranks) & ranks > 0) > 0L
  n_valid <- sum(voter_has_rank)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Helper: per-voter first preferences among a set of active candidates
  # Returns a character vector of candidate names, or NA for exhausted voters
  first_pref_among <- function(active_set) {
    active_idx <- match(active_set, cand_names)
    sub <- ranks[, active_idx, drop = FALSE]
    picks <- apply(sub, 1L, function(row) {
      valid <- which(!is.na(row) & row > 0)
      if (!length(valid)) return(NA_integer_)
      valid[which.min(row[valid])]
    })
    out <- rep(NA_character_, n_voters)
    has <- !is.na(picks)
    out[has] <- active_set[picks[has]]
    out
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Early return when no valid voters
  if (n_valid == 0L) {
    summary <- data.frame(
      candidate = factor(cand_names, levels = cand_names, ordered = TRUE),
      vote = integer(n_candidates),
      percentage = rep(0, n_candidates),
      rank = rep(1L, n_candidates),
      eliminated_in_round = rep(NA_integer_, n_candidates),
      stringsAsFactors = FALSE
    )
    out <- list(
      summary = summary,
      winners = character(0),
      n_voters = n_voters,
      n_valid = n_valid,
      n_candidates = n_candidates,
      rounds = list(),
      counts = matrix(0L, nrow = n_candidates, ncol = 0L,
                      dimnames = list(cand_names, character(0))),
      percentages = matrix(0, nrow = n_candidates, ncol = 0L,
                           dimnames = list(cand_names, character(0))),
      method = list(type = inferred_type,
                    ties = ties,
                    elim_ties = elim_ties,
                    majority = majority)
    )
    class(out) <- "irv_result"
    return(out)
  }

  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # IRV iteration
  active <- cand_names
  eliminated_in_round <- setNames(rep(NA_integer_, n_candidates), cand_names)
  rounds <- list()
  history_picks <- if (isTRUE(return_history)) {
    matrix(NA_character_, nrow = n_voters, ncol = 0L)
  } else {
    NULL
  }
  winners <- character(0)
  round_num <- 0L

  repeat {
    round_num <- round_num + 1L

    voter_pick <- first_pref_among(active)
    if (!is.null(history_picks)) {
      history_picks <- cbind(history_picks, voter_pick)
    }

    counts <- table(factor(voter_pick, levels = active))
    counts_int <- as.integer(counts)
    names(counts_int) <- active

    total_active <- sum(counts_int)
    exhausted <- n_valid - total_active

    pcts <- if (total_active > 0L) {
      counts_int / total_active * 100
    } else {
      rep(0, length(active))
    }
    names(pcts) <- active

    rounds[[round_num]] <- list(
      round = round_num,
      active = active,
      vote_counts = counts_int,
      vote_percentages = pcts,
      total_active = total_active,
      exhausted = exhausted,
      eliminated = character(0)
    )

    # Terminal: only one candidate left
    if (length(active) == 1L) {
      winners <- active
      break
    }

    # Terminal: every voter has exhausted their ballot
    if (total_active == 0L) {
      top_set <- active
      winners <- resolve_ties(top_set, ties)
      break
    }

    max_count <- max(counts_int)
    min_count <- min(counts_int)

    # Terminal: the leader exceeds the majority threshold
    if (max_count / total_active > majority) {
      top_set <- names(counts_int)[counts_int == max_count]
      winners <- resolve_ties(top_set, ties)
      break
    }

    #Terminal : full deadlock (all active candidates tied)
    if (max_count == min_count) {
      top_set <- active
      winners <- resolve_ties(top_set, ties)
      break
    }

    # Eliminate the lowest-scoring candidate(s)
    bottom_set <- names(counts_int)[counts_int == min_count]
    elim <- if (length(bottom_set) == 1L) {
      bottom_set
    } else if (elim_ties == "random") {
      sample(bottom_set, 1L)
    } else if (elim_ties == "lexicographic") {
      sort(bottom_set)[1L]
    } else {  # all -> batch elimination: remove every candidate tied at min
      bottom_set
    }

    rounds[[round_num]]$eliminated <- elim
    eliminated_in_round[elim] <- round_num
    active <- setdiff(active, elim)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Cross-tab matrices: rows = candidates, cols = rounds.
  # Eliminated candidates carry 0 from the round of elimination onward.
  round_labels <- paste0("R", seq_len(round_num))
  counts <- matrix(0L, nrow = n_candidates, ncol = round_num,
                   dimnames = list(cand_names, round_labels))
  percentages <- matrix(0, nrow = n_candidates, ncol = round_num,
                        dimnames = list(cand_names, round_labels))
  for (r in seq_len(round_num)) {
    rd <- rounds[[r]]
    if (length(rd$vote_counts)) {
      counts[names(rd$vote_counts), r] <- as.integer(rd$vote_counts)
    }
    if (length(rd$vote_percentages)) {
      percentages[names(rd$vote_percentages), r] <- as.numeric(rd$vote_percentages)
    }
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Build summary table.
  # `vote`/`percentage` reflect each candidate's score at their LAST appearance:
  # the round of elimination for eliminated candidates, the final round for
  # survivors. This carries IRV-meaningful information instead of zeros.
  last_round <- ifelse(is.na(eliminated_in_round),
                       round_num,
                       as.integer(eliminated_in_round))
  cand_idx   <- seq_len(n_candidates)
  last_vote  <- as.integer(counts[cbind(cand_idx, last_round)])
  last_pct   <- as.numeric(percentages[cbind(cand_idx, last_round)])

  # Sort: by last-appearance vote desc, then by elimination round desc (later
  # eliminations rank higher; survivors with NA sort first), then alphabetical.
  elim_sort <- ifelse(is.na(eliminated_in_round),
                      .Machine$integer.max,
                      as.integer(eliminated_in_round))
  ord <- order(-last_vote, -elim_sort, cand_names, method = "radix")
  ranks_by_votes <- rank(-last_vote, ties.method = "min")[ord]

  df <- data.frame(
    candidate = factor(cand_names[ord], levels = cand_names[ord], ordered = TRUE),
    vote = last_vote[ord],
    percentage = last_pct[ord],
    rank = as.integer(ranks_by_votes),
    eliminated_in_round = as.integer(eliminated_in_round)[ord],
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
    n_valid = n_valid,
    n_candidates = n_candidates,
    rounds = rounds,
    counts = counts,
    percentages = percentages,
    method = list(
      type = inferred_type,
      ties = ties,
      elim_ties = elim_ties,
      majority = majority
    )
  )

  if (isTRUE(return_history)) {
    colnames(history_picks) <- paste0("Round_", seq_len(round_num))
    rownames(history_picks) <- rownames(x)
    out$history <- history_picks
  }

  class(out) <- "irv_result"
  out
  #-----------------------------------------------------------------------------
}
################################################################################

################################################################################
# Printer for irv_result
#' @export
print.irv_result <- function(x, digits = 1, ...) {
  stopifnot(inherits(x, "irv_result"))
  .irv_pretty_print(x, digits = digits)
  invisible(x)
}
################################################################################

################################################################################
# Summary for irv_result
#' @export
summary.irv_result <- function(object, digits = 1, ...) {
  stopifnot(inherits(object, "irv_result"))
  .irv_pretty_print(object, digits = digits, title = "IRV (Instant Runoff)")
  invisible(object)
}
################################################################################

################################################################################
# Internal formatter
#' @noRd
.irv_pretty_print <- function(x, digits = 1, title = "IRV (Instant Runoff)") {

  #-----------------------------------------------------------------------------
  # Pull data
  rounds   <- x$rounds
  n_rounds <- length(rounds)
  df       <- x$summary

  cand_names    <- as.character(df$candidate)
  n_cand_rows   <- length(cand_names)
  n_valid       <- if (!is.null(x$n_valid))     x$n_valid     else 0L
  n_total       <- if (!is.null(x$n_voters))    x$n_voters    else n_valid
  n_cand        <- if (!is.null(x$n_candidates)) x$n_candidates else n_cand_rows
  is_winner_flg <- cand_names %in% x$winners
  elim_round    <- df$eliminated_in_round
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Info line
  info_line <- sprintf(" Voters: %d/%d valid  │  Candidates: %d  │  Rounds: %d  │  Type: %s  │  Majority: %.2f  │  Ties: %s  │  Elim ties: %s",
                       n_valid, n_total, n_cand, n_rounds,
                       x$method$type, x$method$majority,
                       x$method$ties, x$method$elim_ties)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Empty-result short-circuit
  if (n_rounds == 0L) {
    table_width <- max(60L, nchar(info_line) + 2)
    cat("\n", strrep("═", table_width), "\n ", title, "\n",
        strrep("═", table_width), "\n",
        info_line, "\n",
        strrep("─", table_width), "\n",
        " No valid votes.\n",
        "\nWinner(s): ", paste(x$winners, collapse = ", "), "\n\n", sep = "")
    return(invisible(NULL))
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Per-round matrices come from the result, reordered to df's row order.
  # df is already sorted in IRV-natural order (winners first, then by
  # elimination round desc), so no further reordering is needed.
  round_labels  <- colnames(x$counts)
  counts_mat    <- x$counts[cand_names, , drop = FALSE]
  pct_mat       <- x$percentages[cand_names, , drop = FALSE]
  exhausted_row <- vapply(rounds, function(rd) as.integer(rd$exhausted),    integer(1L))
  active_row    <- vapply(rounds, function(rd) as.integer(rd$total_active), integer(1L))
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Pre-format text cells
  cnt_cells       <- format(counts_mat, trim = TRUE)
  active_cells    <- format(active_row, trim = TRUE)
  exhausted_cells <- format(exhausted_row, trim = TRUE)
  pct_cells       <- matrix(paste0(format(round(pct_mat, digits), nsmall = digits), "%"),
                            nrow = n_cand_rows, ncol = n_rounds)
  elim_cells      <- vapply(elim_round, function(e) {
    if (is.na(e)) "—" else sprintf("R%d", e)
  }, character(1L))
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Column widths
  num_w  <- max(2L, nchar(as.character(n_cand_rows)))
  cand_w <- max(9L, max(nchar(cand_names)))                                  # "Candidate" = 9
  cnt_w  <- max(c(nchar(round_labels), nchar(cnt_cells),
                  nchar(active_cells), nchar(exhausted_cells), 4L))
  pct_w  <- max(c(nchar(round_labels), nchar(pct_cells), 5L))
  elim_w <- max(4L, max(nchar(elim_cells)))                                  # "Elim" = 4
  win_w  <- 6L                                                               # "Winner" = 6
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Format strings (counts table)
  cnt_round_fmt <- paste(sprintf("%%%ds", rep(cnt_w, n_rounds)), collapse = "  ")
  hdr_fmt_c <- sprintf(" %%%ds  %%-%ds  %s  %%-%ds  %%-%ds\n",
                       num_w, cand_w, cnt_round_fmt, elim_w, win_w)
  row_fmt_c <- sprintf(" %%%dd  %%-%ds  %s  %%-%ds  %%-%ds\n",
                       num_w, cand_w, cnt_round_fmt, elim_w, win_w)
  foot_fmt_c <- sprintf(" %%%ds  %%-%ds  %s\n",
                        num_w, cand_w, cnt_round_fmt)

  # Format strings (percentage table)
  pct_round_fmt <- paste(sprintf("%%%ds", rep(pct_w, n_rounds)), collapse = "  ")
  hdr_fmt_p <- sprintf(" %%%ds  %%-%ds  %s  %%-%ds  %%-%ds\n",
                       num_w, cand_w, pct_round_fmt, elim_w, win_w)
  row_fmt_p <- sprintf(" %%%dd  %%-%ds  %s  %%-%ds  %%-%ds\n",
                       num_w, cand_w, pct_round_fmt, elim_w, win_w)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Determine table width from a sample header (most accurate)
  sample_hdr_c <- do.call(sprintf, c(list(hdr_fmt_c, "#", "Candidate"),
                                     as.list(round_labels), list("Elim", "Winner")))
  sample_hdr_p <- do.call(sprintf, c(list(hdr_fmt_p, "#", "Candidate"),
                                     as.list(round_labels), list("Elim", "Winner")))
  body_w      <- max(nchar(sample_hdr_c), nchar(sample_hdr_p)) - 1L          # strip trailing \n
  table_width <- max(nchar(info_line) + 2, body_w)

  header_line    <- strrep("═", table_width)
  separator_line <- strrep("─", table_width)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Header
  cat("\n", header_line, "\n ", title, "\n", header_line, "\n",
      info_line, "\n", separator_line, "\n", sep = "")
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Vote counts table
  cat(" Vote counts by round\n", separator_line, "\n", sep = "")

  cat(do.call(sprintf, c(list(hdr_fmt_c, "#", "Candidate"),
                         as.list(round_labels), list("Elim", "Winner"))))
  cat(separator_line, "\n", sep = "")

  for (i in seq_len(n_cand_rows)) {
    mark <- if (is_winner_flg[i]) "  X  " else ""
    cat(do.call(sprintf, c(list(row_fmt_c, i, cand_names[i]),
                           as.list(cnt_cells[i, ]),
                           list(elim_cells[i], mark))))
  }
  cat(separator_line, "\n", sep = "")

  cat(do.call(sprintf, c(list(foot_fmt_c, "", "Active"),
                         as.list(active_cells))))
  cat(do.call(sprintf, c(list(foot_fmt_c, "", "Exhausted"),
                         as.list(exhausted_cells))))
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Vote percentages table
  cat(separator_line, "\n", sep = "")
  cat(" Vote percentages by round\n", separator_line, "\n", sep = "")

  cat(do.call(sprintf, c(list(hdr_fmt_p, "#", "Candidate"),
                         as.list(round_labels), list("Elim", "Winner"))))
  cat(separator_line, "\n", sep = "")

  for (i in seq_len(n_cand_rows)) {
    mark <- if (is_winner_flg[i]) "  X  " else ""
    cat(do.call(sprintf, c(list(row_fmt_p, i, cand_names[i]),
                           as.list(pct_cells[i, ]),
                           list(elim_cells[i], mark))))
  }
  cat(separator_line, "\n", sep = "")
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Winner announcement
  cat("\nWinner(s): ", paste(x$winners, collapse = ", "), "\n\n", sep = "")
  #-----------------------------------------------------------------------------
  invisible(NULL)
}
################################################################################
