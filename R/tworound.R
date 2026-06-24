################################################################################
tworound <- function(
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
  ranks <- build_ranks(x, inferred_type, context = "tworound")
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # First preferences (Round 1)
  first_choice_idx <- apply(ranks, 1L, function(row) {
    valid <- which(!is.na(row) & row > 0)
    if (!length(valid)) return(NA_integer_)
    valid[which.min(row[valid])]
  })

  valid_voters_r1 <- which(!is.na(first_choice_idx))
  n_valid_r1 <- length(valid_voters_r1)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Early return when no valid first choices
  if (n_valid_r1 == 0L) {
    summary_r1 <- data.frame(
      candidate = factor(cand_names, levels = cand_names, ordered = TRUE),
      votes = integer(n_candidates),
      percentage = rep(0, n_candidates),
      stringsAsFactors = FALSE
    )
    out <- list(
      round1 = list(summary = summary_r1, n_valid = 0L),
      round2 = NULL,
      finalists = character(0),
      winners = character(0),
      n_voters = n_voters,
      n_candidates = n_candidates,
      method = list(type = inferred_type, ties = ties, tworound = FALSE)
    )
    if (isTRUE(return_ranks)) out$ranks <- ranks
    class(out) <- "tworound_result"
    return(out)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  vote_names_r1 <- if (n_valid_r1) {
    cand_names[first_choice_idx[valid_voters_r1]]
  } else {
    character(0)
  }

  vote_counts_r1 <- table(factor(vote_names_r1, levels = cand_names))

  percentage_r1 <- if (n_valid_r1) {
    as.numeric(vote_counts_r1) / n_valid_r1 * 100
  } else {
    rep(0, n_candidates)
  }

  ord_r1 <- order(as.integer(vote_counts_r1), decreasing = TRUE, method = "radix")
  ranks_by_votes_r1 <- rank(-as.integer(vote_counts_r1), ties.method = "min")[ord_r1]

  df_r1 <- data.frame(
    candidate = factor(names(vote_counts_r1)[ord_r1], levels = names(vote_counts_r1)[ord_r1], ordered = TRUE),
    vote = as.integer(vote_counts_r1)[ord_r1],
    percentage = percentage_r1[ord_r1],
    rank = as.integer(ranks_by_votes_r1),
    stringsAsFactors = FALSE
  )
  rownames(df_r1) <- NULL
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Check for majority (>50% of valid votes) in round 1
  has_majority_r1 <- n_valid_r1 > 0 && max(percentage_r1, na.rm = TRUE) > 50
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Select finalists (if round 2 is needed)
  pick_finalists <- function(counts, ties, names_vec) {
    v <- as.integer(counts)
    # Indices of candidates with maximum votes (1st place)
    top1_val <- max(v)
    top1_idx <- which(v == top1_val)

    if (length(top1_idx) >= 2) {
      # Tie for first place
      if (ties == "random") {
        finalists_idx <- sample(top1_idx, min(2L, length(top1_idx)))
      } else if (ties == "lexicographic") {
        finalists_idx <- top1_idx[order(names_vec[top1_idx])][seq_len(min(2L, length(top1_idx)))]
      } else { # ties = "all"
        finalists_idx <- top1_idx
      }
      return(finalists_idx)
    }

    # Unique leader -> need to select second place
    leader_idx <- top1_idx
    v_2 <- v
    v_2[leader_idx] <- -Inf
    second_val <- max(v_2)
    if (!is.finite(second_val)) {
      # Only one candidate exists
      return(leader_idx)
    }
    second_idx <- which(v == second_val)

    if (length(second_idx) == 1) {
      finalists_idx <- c(leader_idx, second_idx)
    } else {
      # Tie for second place
      if (ties == "random") {
        finalists_idx <- c(leader_idx, sample(second_idx, 1L))
      } else if (ties == "lexicographic") {
        finalists_idx <- c(leader_idx, second_idx[order(names_vec[second_idx])][1L])
      } else { # ties == "all"
        finalists_idx <- c(leader_idx, second_idx)
      }
    }
    unique(finalists_idx)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # If no run-off is needed: declare winner from round 1
  if (has_majority_r1) {
    max_votes <- max(as.integer(vote_counts_r1))
    top_idx <- which(as.integer(vote_counts_r1) == max_votes)
    top_names <- names(vote_counts_r1)[top_idx]
    winners <- resolve_ties(top_names, ties)

    # Output assembly
    out <- list(
      round1 = list(
        summary = df_r1,
        n_valid = n_valid_r1
      ),
      round2 = NULL,
      finalists = character(0),
      winners = winners,
      n_voters = n_voters,
      n_candidates = n_candidates,
      method = list(type = inferred_type, ties = ties, tworound = FALSE)
    )

    if (isTRUE(return_ranks)) out$ranks <- ranks
    class(out) <- "tworound_result"
    return(out)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Second round: determine finalists (possibly >2 if ties == "all")
  finalists_idx <- pick_finalists(vote_counts_r1, ties = ties, names_vec = cand_names)
  finalists <- cand_names[finalists_idx]

  if (length(finalists) > 2L) {
    warning(sprintf("Tie produced %d finalists instead of 2.", length(finalists)))
  }

  # Restrict ranks matrix to finalists
  ranks_r2 <- as.matrix(ranks[ , finalists, drop = FALSE])

  # For each voter, pick best among finalists
  choice_r2_idx <- apply(ranks_r2, 1L, function(row) {
    valid <- which(!is.na(row) & row > 0)
    if (!length(valid)) return(NA_integer_)
    valid[which.min(row[valid])]
  })
  valid_voters_r2 <- which(!is.na(choice_r2_idx))
  n_valid_r2 <- length(valid_voters_r2)

  vote_names_r2 <- if (n_valid_r2) {
    finalists[choice_r2_idx[valid_voters_r2]]
  } else {
    character(0)
  }

  vote_counts_r2 <- table(factor(vote_names_r2, levels = finalists))
  percentage_r2 <- if (n_valid_r2) {
    as.numeric(vote_counts_r2) / n_valid_r2 * 100
  }
  else {
    rep(0, length(finalists))
  }

  ord_r2 <- order(as.integer(vote_counts_r2), decreasing = TRUE, method = "radix")
  ranks_by_votes_r2 <- rank(-as.integer(vote_counts_r2), ties.method = "min")[ord_r2]

  df_r2 <- data.frame(
    candidate = factor(names(vote_counts_r2)[ord_r2], levels = names(vote_counts_r2)[ord_r2], ordered = TRUE),
    vote = as.integer(vote_counts_r2)[ord_r2],
    percentage = percentage_r2[ord_r2],
    rank = as.integer(ranks_by_votes_r2),
    stringsAsFactors = FALSE
  )
  rownames(df_r2) <- NULL
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Winner(s) of round 2
  max_r2 <- if (length(vote_counts_r2)) {
    max(as.integer(vote_counts_r2))
  } else {
    -Inf
  }
  top2_idx <- which(as.integer(vote_counts_r2) == max_r2)
  top2_names <- names(vote_counts_r2)[top2_idx]
  winners <- resolve_ties(top2_names, ties)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Output assembly
  out <- list(
    round1 = list(
      summary = df_r1,
      n_valid = n_valid_r1
    ),
    round2 = list(
      summary = df_r2,
      n_valid = n_valid_r2
    ),
    finalists = finalists,
    winners = winners,
    n_voters = n_voters,
    n_candidates = n_candidates,
    method = list(type = inferred_type, ties = ties, tworound = TRUE)
  )
  if (isTRUE(return_ranks)) {
    out$ranks <- ranks
  }

  class(out) <- "tworound_result"
  out
  #-----------------------------------------------------------------------------
}
################################################################################

################################################################################
# Printer for tworound_result
print.tworound_result <- function(x, digits = 1, ...) {
  stopifnot(inherits(x, "tworound_result"))
  .tworound_pretty_print(x, digits = digits)
  invisible(x)
}
################################################################################

################################################################################
# Summary for tworound_result
summary.tworound_result <- function(x, digits = 1, ...) {
  stopifnot(inherits(x, "tworound_result"))
  .tworound_pretty_print(x, digits = digits, title = "Two-Round System")
  invisible(x)
}
################################################################################

################################################################################
# Internal formatter
.tworound_pretty_print <- function(x, digits = 1, title = "Two-Round System") {

  #-----------------------------------------------------------------------------
  # Internal round table printer (fptp style)
  .print_round_table <- function(df, n_valid, digits, candidate_width, table_width,
                                 separator_line, marked_names = character(0), mark_label = "Winner") {
    if (is.null(df) || nrow(df) == 0) {
      cat(" No valid votes.\n")
      return(invisible(NULL))
    }

    candidate <- df$candidate
    votes <- df$vote
    percentage <- df$percentage
    rank <- df$rank
    is_marked <- candidate %in% marked_names

    # Table header
    header_fmt <- sprintf(" %%3s  %%-%ds  %%10s  %%-11s  %%6s  %%-9s\n", candidate_width)
    cat(sprintf(header_fmt, "#", "Candidate", "Votes", "%", "Rank", mark_label))
    cat(separator_line, "\n", sep = "")

    # Table rows
    row_fmt <- sprintf(" %%3d  %%-%ds  %%10d  %%-11s  %%6d  %%-9s\n", candidate_width)
    pct_chr <- paste0(format(round(percentage, digits), nsmall = digits), "%")
    for (i in seq_along(candidate)) {
      mark <- if (is_marked[i]) "  X  " else ""
      cat(sprintf(row_fmt, i, candidate[i], votes[i], pct_chr[i],
                  rank[i], mark))
    }
    cat(separator_line, "\n", sep = "")

    # Total row
    total_fmt <- sprintf("      %%-%ds  %%10d  %%-11s\n", candidate_width)
    total_pct <- paste0(format(round(sum(percentage), digits), nsmall = digits), "%")
    cat(sprintf(total_fmt, "TOTAL", n_valid, total_pct))

    invisible(NULL)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Total numbers
  n_valid_r1 <- x$round1$n_valid
  n_total <- if (!is.null(x$n_voters)) x$n_voters else n_valid_r1
  n_cand <- if (!is.null(x$n_candidates)) x$n_candidates else 0L
  has_r2 <- isTRUE(x$method$tworound)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Calculate dynamic width based on info line
  info_line <- sprintf(" Votes: %d/%d valid  \u2502  Candidates: %d  \u2502  Type: %s  \u2502  Ties: %s  \u2502  Runoff: %s",
                       n_valid_r1, n_total, n_cand, x$method$type, x$method$ties,
                       ifelse(has_r2, "yes", "no"))
  info_width <- nchar(info_line)

  table_width <- max(71, info_width + 2)
  candidate_width <- 20 + (table_width - 71)

  header_line <- strrep("\u2550", table_width)
  separator_line <- strrep("\u2500", table_width)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Header
  cat("\n")
  cat(header_line, "\n", sep = "")
  cat(" ", title, "\n", sep = "")
  cat(header_line, "\n", sep = "")

  # Info line
  cat(info_line, "\n")
  cat(separator_line, "\n", sep = "")
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Round 1
  cat(" Round 1", if (!has_r2) " (decisive)" else "", "\n", sep = "")
  cat(separator_line, "\n", sep = "")

  finalists_names <- if (!is.null(x$finalists)) x$finalists else character(0)

  if (has_r2) {
    .print_round_table(x$round1$summary, n_valid_r1, digits, candidate_width,
                       table_width, separator_line, marked_names = finalists_names,
                       mark_label = "Finalist")
  } else {
    .print_round_table(x$round1$summary, n_valid_r1, digits, candidate_width,
                       table_width, separator_line, marked_names = x$winners,
                       mark_label = "Winner")
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Round 2
  if (has_r2 && !is.null(x$round2)) {
    cat("\n")
    cat(separator_line, "\n", sep = "")
    cat(" Round 2  |  Finalists: ", paste(x$finalists, collapse = ", "), "\n", sep = "")
    cat(separator_line, "\n", sep = "")

    .print_round_table(x$round2$summary, x$round2$n_valid, digits, candidate_width,
                       table_width, separator_line, marked_names = x$winners,
                       mark_label = "Winner")
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Winner announcement
  cat("\nWinner(s): ", paste(x$winners, collapse = ", "), "\n\n", sep = "")
  #-----------------------------------------------------------------------------
}
################################################################################
