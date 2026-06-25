################################################################################
#' D21 Voting with minus votes
#'
#' Runs the single-winner D21 method with minus votes. Each voter receives a
#' fixed number of plus votes (1, 2, or 3, depending on the number of
#' candidates) to support their most preferred candidates, and voters who use at
#' least two plus votes may additionally cast one minus vote against the
#' candidate they most oppose. Eligibility for plus votes is derived from ranks
#' or utility scores via a configurable `rule` and then capped to the allowed
#' number; the minus vote targets the lowest-scored candidate the voter did not
#' plus. The candidate with the highest net total wins.
#'
#' @inheritParams voting-params
#' @param rule The approval rule used to convert non-approval ballots (ranks or
#'   utilities) into eligibility for plus votes; ignored when the input is
#'   already an approval matrix. May be a preset string -- `"mean"` (the
#'   default; score above the voter's mean), `"median"` (above the voter's
#'   median), `"topk"` (top `k` by score, `k` from `threshold`), `"topp"` (top
#'   proportion by score, `p` from `threshold`), `"quantile"` (above the
#'   `threshold`-th quantile), `"above0"` (positive score, or all ranked
#'   candidates for rank input), or `"nonzero"` (non-zero score, or all ranked
#'   candidates for rank input) -- or a numeric scalar/vector of length
#'   `ncol(x)` (eligible if `score > rule` for utility, or `rank <= rule` for
#'   rank input), or a custom function with signature `function(row, inferred_type)`
#'   returning a logical vector of length `ncol(x)`.
#' @param threshold Single numeric value (or `NULL`, the default) parameterizing
#'   the `"topk"` (number of candidates to approve), `"topp"` (proportion in
#'   `[0, 1]`) and `"quantile"` (quantile in `[0, 1]`) presets. Ignored for all
#'   other rules.
#' @param minus_prob Numeric in `[0, 1]` (default `1`) giving the probability
#'   that a voter eligible for a minus vote actually casts it. Setting
#'   `minus_prob = 0` suppresses all minus votes.
#' @param overflow Character string giving the tie-breaking rule used at the
#'   plus-vote cap, within the `"topk"`/`"topp"` rules, and when selecting the
#'   worst candidate for the minus vote. One of `"random"` (the default; sample
#'   among tied candidates) or `"lexicographic"` (choose alphabetically by name).
#' @param return_approvals Logical. If `TRUE`, the result additionally includes
#'   the per-voter `$plus_matrix`, `$minus_matrix` and `$total_matrix`. Defaults
#'   to `FALSE`.
#'
#' @return An object of class `"d21_minus_result"`: a list with elements
#'   `summary` (a data frame of per-candidate plus, minus and total counts with
#'   percentages and ranks), `winners`, `n_voters`, `n_valid`, `n_candidates`
#'   and `method` (a list recording `type`, `rule`, `ties`, `threshold`,
#'   `plus_votes`, `minus_votes`, `minus_prob` and `overflow`). When
#'   `return_approvals = TRUE`, the elements `plus_matrix`, `minus_matrix` and
#'   `total_matrix` are also included. Print the object or call [summary()] on
#'   it for a formatted results table.
#'
#' @seealso [d21()], [approval()], [borda()]
#'
#' @examples
#' # Utility ballots with 5 candidates (2 plus votes, 1 minus vote)
#' u <- gen_utilities(n_voters = 50, n_candidates = 5, seed = 1)
#' d21_minus(u)
#'
#' # Ranking ballots, eligible = explicitly ranked candidates
#' r <- gen_ranks(n_voters = 50, n_candidates = 6, seed = 1)
#' d21_minus(r, rule = "above0", ties = "lexicographic")
#'
#' # Suppress the minus phase (equivalent to plain D21)
#' d21_minus(u, minus_prob = 0)
#'
#' @export
d21_minus <- function(
    x,
    type = c("auto", "rank", "utility", "approval"),
    rule = c("mean", "median", "topk", "topp", "quantile", "above0", "nonzero"),
    threshold = NULL,
    minus_prob = 1,
    ties = c("random", "lexicographic", "all"),
    overflow = c("random", "lexicographic"),
    return_approvals = FALSE
) {

  #-----------------------------------------------------------------------------
  # Argument matching
  type <- match.arg(type)
  rule_was_set <- !missing(rule)
  rule <- if (is.character(rule)) match.arg(rule) else rule
  ties <- match.arg(ties)
  overflow <- match.arg(overflow)

  if (!is.null(threshold) &&
      (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold))) {
    stop("`threshold` must be NULL or a single non-NA numeric value.", call. = TRUE)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Input preparation and validation
  x <- prepare_ballots(x)
  n_voters     <- nrow(x)
  n_candidates <- ncol(x)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Validate minus_prob
  if (!is.numeric(minus_prob) || length(minus_prob) != 1L || is.na(minus_prob)) {
    stop("`minus_prob` must be a single non-NA numeric value.", call. = TRUE)
  }
  if (minus_prob < 0 || minus_prob > 1) {
    stop("`minus_prob` must be between 0 and 1.", call. = TRUE)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Candidate names
  if (is.null(colnames(x))) {
    colnames(x) <- paste0("Candidate_", seq_len(n_candidates))
  }
  cand_names <- colnames(x)
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
  # Validate approval input: must be logical or {0, 1} numeric matrix
  if (inferred_type == "approval" && !is.logical(x) && !is_binary_matrix(x)) {
    stop("`type = \"approval\"` requires a logical matrix or a {0, 1} numeric matrix.",
         call. = TRUE)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Validate rank input: integer ranks in [1, n_candidates], no duplicates per voter
  if (inferred_type == "rank") {
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
    n_per_row      <- rowSums(!is.na(x) & x > 0)
    n_uniq_per_row <- apply(x, 1L, function(row) length(unique(row[!is.na(row) & row > 0])))
    if (any(n_per_row != n_uniq_per_row)) {
      bad <- which(n_per_row != n_uniq_per_row)[1]
      stop(sprintf("Voter %d has duplicate ranks; each rank must appear at most once per voter.", bad),
           call. = TRUE)
    }
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Warn if rule is ignored for approval type
  if (inferred_type == "approval" && rule_was_set) {
    warning("`rule` is ignored when input type is 'approval' (binary/logical matrix).", call. = FALSE)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # D21-Minus: how many "plus" and "minus" votes per voter
  plus_votes <- if (n_candidates <= 2L) {
    1L
  } else if (n_candidates >= 7L) {
    3L
  } else {
    2L
  }

  minus_votes <- if (n_candidates <= 3L) {
    0L
  } else {
    1L
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Build per-row eligibility, then cap to `plus_votes`
  build_d21_row <- function(row, inferred_type, rule, threshold, plus_votes, overflow) {

    idx_valid <- which(!is.na(row))
    if (!length(idx_valid)) return(rep(FALSE, length(row)))

    # If already approval matrix (logical matrix)
    if (inferred_type == "approval") {
      eligible <- rep(FALSE, length(row))
      eligible[idx_valid] <- as.logical(row[idx_valid])

      pos <- which(eligible)
      if (!length(pos) || plus_votes <= 0L) return(rep(FALSE, length(row)))

      # No scores available -> select via `overflow` strategy
      k <- min(plus_votes, length(pos))
      keep <- if (k == length(pos)) {
        pos
      } else if (overflow == "random") {
        sample(pos, k)
      } else {  # lexicographic
        pos[order(cand_names[pos], method = "radix")[seq_len(k)]]
      }
      out <- rep(FALSE, length(row))
      out[keep] <- TRUE
      return(out)
    }

    # Convert row to "score where bigger = better"
    # ranks: smaller is better -> score = -rank
    score <- if (inferred_type == "rank") {
      s <- rep(NA_real_, length(row))
      pos <- which(!is.na(row) & row > 0)
      if (length(pos)) s[pos] <- -row[pos]
      s
    } else {
      as.numeric(row)
    }

    score_valid <- which(!is.na(score))
    valid_scores <- score[score_valid]
    if (!length(valid_scores)) return(rep(FALSE, length(row)))

    # Build approvals
    eligible <- rep(FALSE, length(row))

    if (is.function(rule)) {
      custom <- rule(row, inferred_type)
      if (!is.logical(custom) || length(custom) != length(row)) {
        stop("Custom `rule` must return a logical vector of length n_candidates.", call. = TRUE)
      }
      eligible[idx_valid] <- custom[idx_valid]

    } else if (is.numeric(rule)) {
      k <- rule
      if (length(k) == 1L) k <- rep(k, length(row))
      if (length(k) != length(row)) {
        stop("Numeric `rule` must be scalar or length n_candidates.", call. = TRUE)
      }

      if (inferred_type == "rank") {
        pos <- which(!is.na(row) & row > 0)
        eligible[pos] <- row[pos] <= k[pos]
      } else {
        eligible[idx_valid] <- score[idx_valid] > k[idx_valid]
      }
    } else if (is.character(rule)) {

      if (rule == "nonzero") {
        if (inferred_type == "rank") {
          pos <- which(!is.na(row) & row > 0)
          eligible[pos] <- TRUE
        } else {
          eligible[idx_valid] <- score[idx_valid] != 0
        }

      }

      else if (rule == "above0") {
        if (inferred_type == "rank") {
          pos <- which(!is.na(row) & row > 0)
          eligible[pos] <- TRUE
        } else {
          eligible[idx_valid] <- score[idx_valid] > 0
        }
      }

      else if (rule == "mean") {
        thr <- mean(valid_scores, na.rm = TRUE)
        eligible[score_valid] <- score[score_valid] > thr
      }

      else if (rule == "median") {
        thr <- median(valid_scores, na.rm = TRUE)
        eligible[score_valid] <- score[score_valid] > thr
      }

      else if (rule == "topk") {
        k <- if (is.null(threshold)) ceiling(sum(!is.na(valid_scores)) / 2) else threshold
        k <- max(0, as.integer(k))
        score_pos <- which(!is.na(score))
        k_eff <- min(k, length(score_pos))
        if (k_eff > 0L) {
          keep_idx <- if (k_eff == length(score_pos)) {
            score_pos
          } else if (overflow == "random") {
            shuf <- sample(score_pos)
            shuf[order(score[shuf], decreasing = TRUE, method = "radix")][seq_len(k_eff)]
          } else {  # lexicographic
            score_pos[order(score[score_pos], cand_names[score_pos],
                            decreasing = c(TRUE, FALSE), method = "radix")][seq_len(k_eff)]
          }
          eligible[keep_idx] <- TRUE
        }
      }

      else if (rule == "topp") {
        p <- if (is.null(threshold)) 0.5 else as.numeric(threshold)
        p <- max(0, min(1, p))
        score_pos <- which(!is.na(score))
        k_eff <- min(ceiling(length(score_pos) * p), length(score_pos))
        if (k_eff > 0L) {
          keep_idx <- if (k_eff == length(score_pos)) {
            score_pos
          } else if (overflow == "random") {
            shuf <- sample(score_pos)
            shuf[order(score[shuf], decreasing = TRUE, method = "radix")][seq_len(k_eff)]
          } else {  # lexicographic
            score_pos[order(score[score_pos], cand_names[score_pos],
                            decreasing = c(TRUE, FALSE), method = "radix")][seq_len(k_eff)]
          }
          eligible[keep_idx] <- TRUE
        }
      }
      else if (rule == "quantile") {
        q <- if (is.null(threshold)) 0.5 else as.numeric(threshold)
        q <- max(0, min(1, q))
        thr <- as.numeric(quantile(valid_scores, probs = q, na.rm = TRUE, names = FALSE))
        eligible[score_valid] <- score[score_valid] > thr
      }

      else {
        stop("Unknown `rule` specification.", call. = TRUE)
      }
    } else {
      stop("Unknown `rule` specification.", call. = TRUE)
    }

    # Cap eligible approvals to `plus_votes` best by score
    # Ties at the cutoff are broken via `overflow` strategy
    pos <- which(eligible & !is.na(score))
    if (!length(pos) || plus_votes <= 0L) return(rep(FALSE, length(row)))

    k <- min(plus_votes, length(pos))
    keep <- if (k == length(pos)) {
      pos
    } else if (overflow == "random") {
      pos_shuffled <- sample(pos)
      ord_shuffled <- order(score[pos_shuffled], decreasing = TRUE, method = "radix")
      pos_shuffled[ord_shuffled][seq_len(k)]
    } else {  # lexicographic
      ord_lex <- order(score[pos], cand_names[pos],
                       decreasing = c(TRUE, FALSE), method = "radix")
      pos[ord_lex][seq_len(k)]
    }

    out <- rep(FALSE, length(row))
    out[keep] <- TRUE
    out
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Build D21 plus approval matrix

  plus_matrix <- matrix(
    vapply(seq_len(n_voters), function(i) {
      build_d21_row(x[i, ], inferred_type, rule, threshold, plus_votes, overflow)
    }, logical(n_candidates)),
    nrow = n_voters, ncol = n_candidates, byrow = TRUE
  )

  colnames(plus_matrix) <- cand_names
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Build D21 minus votes matrix
  # Voters who cast at least 2 plus votes are eligible to cast a minus vote.
  # Each eligible voter casts a minus vote with probability `minus_prob`,
  # against their lowest-scored candidate among those they did NOT plus.
  # Ties on the worst candidate are broken via the `overflow` strategy.
  # If a voter has plused all of their valid candidates, no minus is cast.

  minus_matrix <- matrix(0L, nrow = n_voters, ncol = n_candidates)
  colnames(minus_matrix) <- cand_names

  if (minus_votes > 0L && minus_prob > 0 && inferred_type != "approval") {
    plus_given <- rowSums(plus_matrix) >= 2L
    selected <- (runif(n_voters) < minus_prob) & plus_given

    for (i in which(selected)) {
      util_row <- if (inferred_type == "utility") {
        as.numeric(x[i, ])
      } else {  # rank
        s <- rep(NA_real_, n_candidates)
        pos <- which(!is.na(x[i, ]) & x[i, ] > 0)
        if (length(pos)) s[pos] <- -x[i, pos]
        s
      }

      # Exclude candidates already plused by this voter from the minus pool:
      # the same voter must not simultaneously support and oppose a candidate.
      util_row[plus_matrix[i, ]] <- NA

      # If the voter has plused all of their valid candidates, no minus is cast.
      if (all(is.na(util_row))) next

      min_val <- min(util_row, na.rm = TRUE)
      worst_pool <- which(!is.na(util_row) & util_row == min_val)

      min_idx <- if (length(worst_pool) == 1L) {
        worst_pool
      } else if (overflow == "random") {
        sample(worst_pool, 1L)
      } else {  # lexicographic
        worst_pool[order(cand_names[worst_pool], method = "radix")][1L]
      }
      minus_matrix[i, min_idx] <- -1L
    }
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Convert logical plus_matrix to integer and combine with minus matrix

  plus_matrix_int <- plus_matrix * 1L
  total_matrix <- plus_matrix_int + minus_matrix
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Aggregate vote counts and count valid voters
  plus_counts <- colSums(plus_matrix_int, na.rm = TRUE)
  minus_counts <- abs(colSums(minus_matrix, na.rm = TRUE))
  total_counts <- colSums(total_matrix, na.rm = TRUE)
  n_valid <- sum(apply(x, 1L, function(row) any(!is.na(row))))
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Early return when no candidate received any plus or minus vote
  if (sum(plus_counts) == 0L && sum(minus_counts) == 0L) {
    summary <- data.frame(
      candidate = factor(cand_names, levels = cand_names, ordered = TRUE),
      plus_vote = integer(n_candidates),
      plus_percentage = rep(0, n_candidates),
      minus_vote = integer(n_candidates),
      minus_percentage = rep(0, n_candidates),
      total_vote = integer(n_candidates),
      total_percentage = rep(0, n_candidates),
      rank = rep(1L, n_candidates),
      stringsAsFactors = FALSE
    )
    out <- list(
      summary = summary,
      winners = character(0),
      n_voters = n_voters,
      n_valid = n_valid,
      n_candidates = n_candidates,
      method = list(
        type = inferred_type,
        rule = rule,
        ties = ties,
        threshold = threshold,
        plus_votes = plus_votes,
        minus_votes = minus_votes,
        minus_prob = minus_prob,
        overflow = overflow
      )
    )
    if (isTRUE(return_approvals)) {
      out$plus_matrix <- plus_matrix_int
      out$minus_matrix <- minus_matrix
      out$total_matrix <- total_matrix
    }
    class(out) <- "d21_minus_result"
    return(out)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Summary table
  plus_percentage <- plus_counts / n_valid * 100
  minus_percentage <- minus_counts / n_valid * 100
  total_percentage <- total_counts / n_valid * 100

  ord <- order(total_counts, decreasing = TRUE, method = "radix")
  ranks_by_votes <- rank(-total_counts, ties.method = "min")[ord]

  df <- data.frame(
    candidate = factor(cand_names[ord], levels = cand_names[ord], ordered = TRUE),
    plus_vote = as.integer(plus_counts)[ord],
    plus_percentage = as.numeric(plus_percentage)[ord],
    minus_vote = as.integer(minus_counts)[ord],
    minus_percentage = as.numeric(minus_percentage)[ord],
    total_vote = as.integer(total_counts)[ord],
    total_percentage = as.numeric(total_percentage)[ord],
    rank = as.integer(ranks_by_votes),
    stringsAsFactors = FALSE
  )
  rownames(df) <- NULL
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Winner(s)

  max_votes <- max(total_counts)
  top_idx <- which(total_counts == max_votes)
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
    method = list(
      type = inferred_type,
      rule = rule,
      ties = ties,
      threshold = threshold,
      plus_votes = plus_votes,
      minus_votes = minus_votes,
      minus_prob = minus_prob,
      overflow = overflow
    )
  )

  if (isTRUE(return_approvals)) {
    out$plus_matrix <- plus_matrix_int
    out$minus_matrix <- minus_matrix
    out$total_matrix <- total_matrix
  }

  class(out) <- "d21_minus_result"
  out
  #-----------------------------------------------------------------------------
}
################################################################################

################################################################################
# Printer for d21_minus_result
#' @export
print.d21_minus_result <- function(x, digits = 1, ...) {
  stopifnot(inherits(x, "d21_minus_result"))
  .d21_minus_pretty_print(x, digits = digits)
  invisible(x)
}
################################################################################

################################################################################
# Summary for d21_minus_result
#' @export
summary.d21_minus_result <- function(object, digits = 1, ...) {
  stopifnot(inherits(object, "d21_minus_result"))
  .d21_minus_pretty_print(object, digits = digits, title = "D21 with minus vote")
  invisible(object)
}
################################################################################

################################################################################
# Internal formatter
#' @noRd
.d21_minus_pretty_print <- function(x, digits = 1, title = "D21 with minus vote") {
  #-----------------------------------------------------------------------------
  # Summary table
  df <- x$summary
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Column access
  candidate <- df$candidate
  plus_vote <- df$plus_vote
  plus_pct <- df$plus_percentage
  minus_vote <- df$minus_vote
  minus_pct <- df$minus_percentage
  total_vote <- df$total_vote
  total_pct <- df$total_percentage
  rank <- df$rank
  is_winner <- candidate %in% x$winners
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Rule description
  rule_chr <- if (is.character(x$method$rule)) {
    x$method$rule
  } else if (is.function(x$method$rule)) {
    "custom function"
  } else if (is.numeric(x$method$rule)) {
    "numeric rule"
  } else {
    "unknown"
  }

  thr_chr <- if (is.null(x$method$threshold)) {
    "NULL"
  } else if (length(x$method$threshold) == 1L) {
    as.character(x$method$threshold)
  } else {
    "vector"
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Total numbers
  n_valid <- if (!is.null(x$n_valid)) x$n_valid else 0L
  n_total <- if (!is.null(x$n_voters)) x$n_voters else n_valid
  n_cand <- if (!is.null(x$n_candidates)) x$n_candidates else length(candidate)
  plus_votes <- x$method$plus_votes
  minus_votes <- x$method$minus_votes
  minus_prob <- x$method$minus_prob
  total_plus <- if (length(plus_vote)) sum(as.integer(plus_vote)) else 0L
  total_minus <- if (length(minus_vote)) sum(as.integer(minus_vote)) else 0L
  total_all <- if (length(total_vote)) sum(as.integer(total_vote)) else 0L
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Calculate dynamic width based on info line
  info_line <- sprintf(" Votes: %d/%d valid  │  Candidates: %d  │  Plus votes: %d  │  Minus votes: %d  │  Minus prob: %.2f  │  Type: %s  │  Rule: %s  │  Threshold: %s  │  Ties: %s  │  Overflow: %s",
                       n_valid, n_total, n_cand, plus_votes, minus_votes, minus_prob,
                       x$method$type, rule_chr, thr_chr, x$method$ties, x$method$overflow)
  info_width <- nchar(info_line)

  # Set table width to be at least 95 or wider to fit info line
  table_width <- max(95, info_width + 2)

  # Calculate dynamic column widths
  candidate_width <- 20 + (table_width - 95)

  # Header
  cat("\n")
  header_line <- strrep("═", table_width)
  cat(header_line, "\n", sep = "")
  cat(" ", title, "\n", sep = "")
  cat(header_line, "\n", sep = "")

  # Info line
  cat(info_line, "\n")

  separator_line <- strrep("─", table_width)
  cat(separator_line, "\n", sep = "")
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Table header
  header_fmt <- sprintf(" %%3s  %%-%ds  %%6s  %%-7s  %%6s  %%-7s  %%6s  %%-7s  %%4s  %%-9s\n", candidate_width)
  cat(sprintf(header_fmt, "#", "Candidate",
              "Plus", "Plus%", "Minus", "Minus%", "Total", "Total%",
              "Rank", "Winner"))
  cat(separator_line, "\n", sep = "")
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Table rows
  row_fmt <- sprintf(" %%3d  %%-%ds  %%6d  %%-7s  %%6d  %%-7s  %%6d  %%-7s  %%4d  %%-9s\n", candidate_width)
  plus_pct_chr <- paste0(format(round(plus_pct, digits), nsmall = digits), "%")
  minus_pct_chr <- paste0(format(round(minus_pct, digits), nsmall = digits), "%")
  total_pct_chr <- paste0(format(round(total_pct, digits), nsmall = digits), "%")
  for (i in seq_along(candidate)) {
    winner_mark <- if (is_winner[i]) "  X  " else ""
    cat(sprintf(row_fmt, i, candidate[i],
                plus_vote[i], plus_pct_chr[i],
                minus_vote[i], minus_pct_chr[i],
                total_vote[i], total_pct_chr[i],
                rank[i], winner_mark))
  }
  cat(separator_line, "\n", sep = "")

  # Total row
  total_fmt <- sprintf("      %%-%ds  %%6d  %%-7s  %%6d  %%-7s  %%6d  %%-7s\n", candidate_width)
  total_plus_pct <- paste0(format(round(sum(plus_pct), digits), nsmall = digits), "%")
  total_minus_pct <- paste0(format(round(sum(minus_pct), digits), nsmall = digits), "%")
  total_total_pct <- paste0(format(round(sum(total_pct), digits), nsmall = digits), "%")
  cat(sprintf(total_fmt, "TOTAL",
              total_plus, total_plus_pct,
              total_minus, total_minus_pct,
              total_all, total_total_pct))

  # Plus votes and minus votes per voter
  if (n_valid > 0L && plus_votes > 0L) {
    max_possible_plus <- n_valid * plus_votes
    cat(sprintf(" Plus votes per voter (avg):  %.2f / %d (%.1f%%)\n",
                total_plus / n_valid, plus_votes,
                100 * total_plus / max_possible_plus))
  }
  if (n_valid > 0L && minus_votes > 0L) {
    max_possible_minus <- n_valid * minus_votes
    cat(sprintf(" Minus votes per voter (avg): %.2f / %d (%.1f%%)\n",
                total_minus / n_valid, minus_votes,
                100 * total_minus / max_possible_minus))
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Winner announcement
  cat("\nWinner(s): ", paste(x$winners, collapse = ", "), "\n\n", sep = "")
  #-----------------------------------------------------------------------------
}
################################################################################
