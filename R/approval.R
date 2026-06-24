################################################################################
approval <- function(
  x,
  type = c("auto", "rank", "utility", "approval"),
  rule = c("mean", "median", "topk", "topp", "quantile", "above0", "nonzero"),
  threshold = NULL,
  ties = c("random", "lexicographic", "all"),
  return_approvals = FALSE
) {

  #-----------------------------------------------------------------------------
  # Argument matching
  type <- match.arg(type)
  rule_was_set <- !missing(rule)
  rule <- if (is.character(rule)) match.arg(rule) else rule
  ties <- match.arg(ties)

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
  inferred_type <- switch (type,
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
  # Row-level approval builder (unified)
  build_approvals_row <- function(row, inferred_type, rule, threshold) {

    idx_valid <- which(!is.na(row))
    if (!length(idx_valid)) return(rep(FALSE, length(row)))

    # If already approval matrix (logical matrix)
    if (inferred_type == "approval") {
      out <- rep(FALSE, length(row))
      out[idx_valid] <- as.logical(row[idx_valid])
      return(out)
    }

    # Convert row to "score where bigger = better"
    # ranks: smaller is better -> score = -rank
    scores <- if (inferred_type == "rank") {
      s <- rep(NA_real_, length(row))
      pos <- which(!is.na(row) & row > 0)
      if (length(pos)) s[pos] <- -row[pos]
      s
    } else # utility
      {
      as.numeric(row)
      }

    score_valid <- which(!is.na(scores))
    valid_scores <- scores[score_valid]
    if (!length(valid_scores)) return(rep(FALSE, length(row)))

    out <- rep(FALSE, length(row))

    # Custom rule as function(row, inferred_type) -> logical
    if (is.function(rule)) {
      custom <- rule(row, inferred_type)
      if (!is.logical(custom) || length(custom) != length(row)) {
        stop("Custom `rule` must return a logical vector of length n_candidates.", call. = TRUE)
      }
      out[idx_valid] <- custom[idx_valid]
      return(out)
    }

    # Numeric threshold
    # * for utilities: approve if utility > threshold
    # * for ranks: approve if rank <= threshold
    if (is.numeric(rule)) {
      k <- rule
      if (length(k) == 1L) k <- rep(k, length(row))
      if (length(k) != length(row)) {
        stop("Numeric `rule` must be scalar or length n_candidates.", call. = TRUE)
      }

      if (inferred_type == "rank") {
        pos <- which(!is.na(row) & row > 0)
        out[pos] <- row[pos] <= k[pos]
      } else {
        out[idx_valid] <- scores[idx_valid] > k[idx_valid]
      }
      return(out)
    }

    # Presets (work for both rank and utility via `scores`)
    if (is.character(rule)) {

      if (rule == "nonzero") {
        # Approve all explicitly ranked / all positive utilities
        if (inferred_type == "rank") {
          pos <- which(!is.na(row) & row > 0)
          out[pos] <- TRUE
        } else {
          out[idx_valid] <- scores[idx_valid] != 0
        }
        return(out)
      }

      if (rule == "above0") {
        if (inferred_type == "rank") {
          pos <- which(!is.na(row) & row > 0)
          out[pos] <- TRUE
        } else {
          out[idx_valid] <- scores[idx_valid] > 0
        }
        return(out)
      }

      if (rule == "mean") {
        thr <- mean(valid_scores, na.rm = TRUE)
        out[score_valid] <- scores[score_valid] > thr
        return(out)
      }

      if (rule == "median") {
        thr <- median(valid_scores, na.rm = TRUE)
        out[score_valid] <- scores[score_valid] > thr
        return(out)
      }

      if (rule == "topk") {
        k <- if (is.null(threshold)) ceiling(sum(!is.na(valid_scores)) / 2) else threshold
        k <- max(0, as.integer(k))
        ord <- order(scores, decreasing = TRUE, na.last = NA, method = "radix")
        if (length(ord)) out[ord[seq_len(min(k, length(ord)))]] <- TRUE
        return(out)
      }

      if (rule == "topp") {
        p <- if (is.null(threshold)) 0.5 else as.numeric(threshold)
        p <- max(0, min(1, p))
        ord <- order(scores, decreasing = TRUE, na.last = NA, method = "radix")
        k <- ceiling(length(ord) * p)
        if (length(ord) && k > 0) out[ord[seq_len(k)]] <- TRUE
        return(out)
      }

      if (rule == "quantile") {
        q <- if (is.null(threshold)) 0.5 else as.numeric(threshold)
        q <- max(0, min(1, q))
        thr <- as.numeric(quantile(valid_scores, probs = q, na.rm = TRUE, names = FALSE))
        out[score_valid] <- scores[score_valid] > thr
        return(out)
      }
    }
    stop("Unknown `rule` specification.", call. = TRUE)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Build approval matrix
  approvals <- matrix(
    vapply(seq_len(n_voters), function(i) {
      build_approvals_row(x[i, ], inferred_type, rule, threshold)
    }, logical(n_candidates)),
    nrow = n_voters, ncol = n_candidates, byrow = TRUE
  )

  colnames(approvals) <- cand_names
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Aggregate vote counts and valid voters
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
      method = list(type = inferred_type,
                    rule = rule,
                    ties = ties,
                    threshold = threshold)
    )
    if (isTRUE(return_approvals)) out$approvals <- approvals
    class(out) <- "approval_result"
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
    method = list(type = inferred_type,
                  rule = rule,
                  ties = ties,
                  threshold = threshold)
  )

  if (isTRUE(return_approvals)) out$approvals <- approvals
  class(out) <- "approval_result"
  out
  #-----------------------------------------------------------------------------
}
################################################################################

################################################################################
# Printer for approval_result
print.approval_result <- function(x, digits = 1, ...) {
  stopifnot(inherits(x, "approval_result"))
  .approval_pretty_print(x, digits = digits)
  invisible(x)
}
################################################################################

################################################################################
# Summary for approval_result
summary.approval_result <- function(x, digits = 1, ...) {
  stopifnot(inherits(x, "approval_result"))
  .approval_pretty_print(x, digits = digits, title = "Approval Voting")
  invisible(x)
}
################################################################################

################################################################################
# Internal formatter
.approval_pretty_print <- function(x, digits = 1, title = "Approval Voting") {
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
  total_approvals <- if (length(votes)) sum(as.integer(votes)) else 0L
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Info line
  info_line <- sprintf(" Votes: %d/%d valid  \u2502  Candidates: %d  \u2502  Type: %s  \u2502  Rule: %s  \u2502  Threshold: %s  \u2502  Ties: %s",
                       n_valid, n_total, n_cand, x$method$type, rule_chr, thr_chr, x$method$ties)
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
      cat(sprintf(" Approvals per voter (avg): %.2f\n", total_approvals / n_valid))
    }
  )
  #-----------------------------------------------------------------------------
}
################################################################################
