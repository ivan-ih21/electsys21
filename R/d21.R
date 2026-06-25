################################################################################
#' D21 Voting
#'
#' Runs the D21 method. Each voter is granted a fixed number of **plus votes**
#' (1, 2, or 3, depending on the number of candidates) and uses them to support
#' their most preferred candidates. When the input is not already an approval
#' matrix, each voter's eligible candidates are first selected from rankings or
#' utility scores using a configurable `rule`, then capped to the allowed number
#' of plus votes, keeping the highest-scored candidates. The candidate with the
#' most plus votes across all voters wins.
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
#' @param overflow Character string giving the tie-breaking rule applied per
#'   voter when more candidates are eligible than the plus-vote budget allows
#'   (and at the `"topk"`/`"topp"` cutoffs). One of `"random"` (the default;
#'   sample among tied candidates) or `"lexicographic"` (choose alphabetically
#'   by name).
#' @param return_approvals Logical; if `TRUE`, the result additionally includes
#'   the derived binary approval matrix as `$approvals`. Defaults to `FALSE`.
#'
#' @return An object of class `"d21_result"`: a list with elements `summary`
#'   (a data frame of candidates, plus-vote counts, percentages and ranks),
#'   `winners` (name(s) of the winning candidate(s)), `n_voters`, `n_valid`,
#'   `n_candidates`, and `method` (a list recording `type`, `rule`, `ties`,
#'   `threshold`, `plus_votes` and `overflow`). If `return_approvals = TRUE`,
#'   the element `approvals` (the derived approval matrix) is also included.
#'   Print the object or call [summary()] on it for a formatted results table.
#'
#' @seealso [d21_minus()], [approval()], [fptp()]
#'
#' @examples
#' u <- gen_utilities(n_voters = 40, n_candidates = 5, seed = 1)
#' d21(u, type = "utility")
#'
#' r <- gen_ranks(n_voters = 40, n_candidates = 5, seed = 1)
#' d21(r, type = "rank", rule = "topk", threshold = 2)
#'
#' @export
d21 <- function(
    x,
    type = c("auto", "rank", "utility", "approval"),
    rule = c("mean", "median", "topk", "topp", "quantile", "above0", "nonzero"),
    threshold = NULL,
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
  # D21: how many "plus" votes per voter
  plus_votes <- if (n_candidates <= 2L) {
    1L
  } else if (n_candidates >= 7L) {
    3L
  } else {
    2L
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
  # Build D21 approval matrix

  approvals <- matrix(
    vapply(seq_len(n_voters), function(i) {
      build_d21_row(x[i, ], inferred_type, rule, threshold, plus_votes, overflow)
    }, logical(n_candidates)),
    nrow = n_voters, ncol = n_candidates, byrow = TRUE
  )

  colnames(approvals) <- cand_names
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Aggregate vote counts and count valid voters
  vote_counts <- colSums(approvals, na.rm = TRUE)
  n_valid <- sum(apply(x, 1L, function(row) any(!is.na(row))))
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Early return when no candidate received any approval
  if (sum(vote_counts) == 0L) {
    summary <- data.frame(
      candidate = factor(cand_names, levels = cand_names, ordered = TRUE),
      vote = integer(n_candidates),
      percentage = rep(0, n_candidates),
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
        overflow = overflow
      )
    )
    if (isTRUE(return_approvals)) out$approvals <- approvals
    class(out) <- "d21_result"
    return(out)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Summary table
  percentage <- vote_counts / n_valid * 100
  ord <- order(vote_counts, decreasing = TRUE, method = "radix")
  ranks_by_votes <- rank(-vote_counts, ties.method = "min")[ord]

  df <- data.frame(
    candidate = factor(cand_names[ord], levels = cand_names[ord], ordered = TRUE),
    vote = as.integer(vote_counts)[ord],
    percentage = as.numeric(percentage)[ord],
    rank = as.integer(ranks_by_votes),
    stringsAsFactors = FALSE
  )
  rownames(df) <- NULL
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Winner(s)

  max_votes <- max(vote_counts)
  top_idx <- which(vote_counts == max_votes)
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
      overflow = overflow
    )
  )

  if (isTRUE(return_approvals)) out$approvals <- approvals
  class(out) <- "d21_result"
  out
  #-----------------------------------------------------------------------------
}
################################################################################

################################################################################
# Printer for d21_result
#' @export
print.d21_result <- function(x, digits = 1, ...) {
  stopifnot(inherits(x, "d21_result"))
  .d21_pretty_print(x, digits = digits)
  invisible(x)
}
################################################################################

################################################################################
# Summary for d21_result
#' @export
summary.d21_result <- function(object, digits = 1, ...) {
  stopifnot(inherits(object, "d21_result"))
  .d21_pretty_print(object, digits = digits, title = "D21 voting")
  invisible(object)
}
################################################################################

################################################################################
# Internal formatter
#' @noRd
.d21_pretty_print <- function(x, digits = 1, title = "D21 voting") {
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
  total_approvals <- if (length(votes)) sum(as.integer(votes)) else 0L
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Info line
  info_line <- sprintf(" Votes: %d/%d valid  │  Candidates: %d  │  Plus votes: %d  │  Type: %s  │  Rule: %s  │  Threshold: %s  │  Ties: %s  │  Overflow: %s",
                       n_valid, n_total, n_cand, plus_votes,
                       x$method$type, rule_chr, thr_chr, x$method$ties, x$method$overflow)
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
    digits      = digits,
    extras      = function() {
      if (n_valid > 0L && plus_votes > 0L) {
        max_possible <- n_valid * plus_votes
        cat(sprintf(" Approvals per voter (avg): %.2f / %d (%.1f%%)\n",
                    total_approvals / n_valid, plus_votes,
                    100 * total_approvals / max_possible))
      }
    }
  )
  #-----------------------------------------------------------------------------
}
################################################################################
