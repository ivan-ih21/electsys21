################################################################################
majority_judgement <- function(
    x,
    type = c("auto", "rank", "utility", "approval", "score"),
    num_categories = NULL,
    median_tie_break = c("gauge", "iterative"),
    ties = c("random", "lexicographic", "all"),
    return_grades = FALSE
) {

  #-----------------------------------------------------------------------------
  # Argument matching
  type <- match.arg(type)
  median_tie_break <- match.arg(median_tie_break)
  ties <- match.arg(ties)

  if (!is.null(num_categories)) {
    if (!is.numeric(num_categories) || length(num_categories) != 1L ||
        is.na(num_categories) || num_categories < 2 ||
        abs(num_categories - round(num_categories)) > .Machine$double.eps^0.5) {
      stop("`num_categories` must be NULL or a single integer >= 2.", call. = TRUE)
    }
    num_categories <- as.integer(round(num_categories))
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
  # Score matrix detection helper: positive integers (duplicates allowed)
  is_score_matrix <- function(mat) {
    vals <- mat[!is.na(mat) & mat != 0]
    if (length(vals) == 0L) return(FALSE)
    if (!all(vals %% 1 == 0)) return(FALSE)
    all(as.integer(vals) >= 1L)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Per-row uniqueness check (used to disambiguate rank vs score in auto)
  has_unique_positive_per_row <- function(mat) {
    n_per_row      <- rowSums(!is.na(mat) & mat > 0)
    n_uniq_per_row <- apply(mat, 1L, function(row) length(unique(row[!is.na(row) & row > 0])))
    all(n_per_row == n_uniq_per_row)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Input type inference
  inferred_type <- switch(
    type,
    auto = {
      if (is.logical(x)) "approval"
      else if (is_binary_matrix(x)) "approval"
      else if (is_ranking_matrix(x) && has_unique_positive_per_row(x)) "rank"
      else if (is_score_matrix(x)) "score"
      else "utility"
    },
    rank = "rank",
    utility = "utility",
    approval = "approval",
    score = "score"
  )
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Validate approval input
  if (inferred_type == "approval" && !is.logical(x) && !is_binary_matrix(x)) {
    stop("`type = \"approval\"` requires a logical matrix or a {0, 1} numeric matrix.",
         call. = TRUE)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Validate rank input
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
  # Validate score input: positive integers (0 and NA treated as abstention)
  if (inferred_type == "score") {
    vals <- x[!is.na(x) & x != 0]
    if (length(vals)) {
      if (!all(abs(vals - round(vals)) < .Machine$double.eps^0.5)) {
        stop("`type = \"score\"` requires integer grades.", call. = TRUE)
      }
      if (any(vals < 1)) {
        stop("Score grades must be positive integers (>= 1); use NA or 0 for abstention.",
             call. = TRUE)
      }
    }
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Resolve num_categories
  # Convention: grade 1 = worst, grade K = best (higher number = better)
  K <- if (!is.null(num_categories)) {
    num_categories
  } else {
    switch(
      inferred_type,
      approval = 2L,
      rank = as.integer(n_candidates),
      utility = as.integer(min(5L, max(2L, n_candidates))),
      score = {
        score_vals <- x[!is.na(x) & x != 0]
        if (length(score_vals)) as.integer(max(score_vals)) else 2L
      }
    )
  }

  if (inferred_type == "approval" && K != 2L) {
    warning("Approval input is inherently binary; `num_categories` set to 2.", call. = FALSE)
    K <- 2L
  }

  if (inferred_type == "rank" && K > n_candidates) {
    warning(sprintf(
      "Rank input cannot use more grades than candidates; `num_categories` set to %d.",
      n_candidates), call. = FALSE)
    K <- as.integer(n_candidates)
  }

  if (inferred_type == "score") {
    score_vals <- x[!is.na(x) & x != 0]
    if (length(score_vals) && max(score_vals) > K) {
      stop(sprintf(
        "Score input contains grade %d, exceeding `num_categories = %d`.",
        as.integer(max(score_vals)), K), call. = TRUE)
    }
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Build grade matrix (n_voters x n_candidates)
  # Convention: grade 1 = worst (least preferred), grade K = best (most preferred).
  # NA preserved.
  grades <- matrix(NA_real_, nrow = n_voters, ncol = n_candidates)
  colnames(grades) <- cand_names

  if (inferred_type == "approval") {
    # logical or {0, 1} numeric -> K (approved, best) or 1 (rejected, worst)
    x_num <- if (is.logical(x)) {
      m <- matrix(NA_real_, nrow = n_voters, ncol = n_candidates)
      m[!is.na(x) & x] <- 1
      m[!is.na(x) & !x] <- 0
      m
    } else {
      x
    }
    grades[!is.na(x_num) & x_num == 1] <- K
    grades[!is.na(x_num) & x_num == 0] <- 1

  } else if (inferred_type == "rank") {
    # Convert ranks to grades (higher = better). Rank 1 (best preference) -> grade K.
    for (i in seq_len(n_voters)) {
      row <- x[i, ]
      pos <- which(!is.na(row) & row > 0)
      if (!length(pos)) next
      if (K == n_candidates) {
        grades[i, pos] <- n_candidates - row[pos] + 1L
      } else {
        # Bin ranks into K grades: rank 1 -> grade K, rank n_candidates -> grade 1
        binned <- ceiling(row[pos] * K / n_candidates)
        grades[i, pos] <- pmin(K, pmax(1L, K + 1L - binned))
      }
    }

  } else if (inferred_type == "score") {
    # Score IS the grade. Treat 0 as NA (abstention).
    mask <- !is.na(x) & x != 0
    grades[mask] <- x[mask]

  } else {
    # utility: global normalization to [0, 1] using range(x), then equal-width
    # binning into K grades. Higher utility -> higher (better) grade number.
    # Preserves absolute scale: same utility value gives same grade regardless
    # of other candidates in the row.
    valid_vals <- x[!is.na(x)]
    if (length(valid_vals)) {
      lo <- min(valid_vals)
      hi <- max(valid_vals)
      if (hi == lo) {
        # All utilities equal -> indifference -> top grade for everyone
        grades[!is.na(x)] <- K
      } else {
        u_scaled <- (x - lo) / (hi - lo)
        g <- ceiling(u_scaled * K)
        g <- pmin(K, pmax(1L, g))
        g[is.na(x)] <- NA_real_
        grades[] <- g
      }
    }
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # MJ median helper (higher-better orientation)
  # For even count, take the lower of two middles -> more conservative
  # (lower numeric grade = worse), so we report the "worst-among-majority" grade.
  mj_median <- function(v) {
    v <- v[!is.na(v)]
    if (!length(v)) return(NA_real_)
    s <- sort(v)
    n <- length(s)
    s[floor((n + 1L) / 2L)]
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Initial medians
  medians_init <- apply(grades, 2L, mj_median)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Grade distribution table (rows = grades 1..K, cols = candidates)
  grade_dist <- matrix(0L, nrow = K, ncol = n_candidates)
  rownames(grade_dist) <- paste0("Grade_", seq_len(K))
  colnames(grade_dist) <- cand_names
  for (j in seq_len(n_candidates)) {
    col <- grades[, j]
    col <- col[!is.na(col)]
    if (length(col)) {
      tab <- tabulate(as.integer(col), nbins = K)
      grade_dist[, j] <- tab
    }
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Per-candidate gauge: proportion strictly better and strictly worse than median
  # In higher-better orientation:
  #   p_better = prop with grade > median (strictly above)
  #   p_worse  = prop with grade < median (strictly below)
  p_better <- numeric(n_candidates)
  p_worse <- numeric(n_candidates)
  gauge_sign <- character(n_candidates)

  for (j in seq_len(n_candidates)) {
    col <- grades[, j]
    col <- col[!is.na(col)]
    n_j <- length(col)
    if (n_j == 0L || is.na(medians_init[j])) {
      p_better[j] <- NA_real_
      p_worse[j] <- NA_real_
      gauge_sign[j] <- NA_character_
      next
    }
    m <- medians_init[j]
    p_better[j] <- sum(col > m) / n_j
    p_worse[j]  <- sum(col < m) / n_j
    gauge_sign[j] <- if (p_better[j] > p_worse[j]) {
      "+"
    } else if (p_worse[j] > p_better[j]) {
      "-"
    } else {
      "0"
    }
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Count valid voters (at least one non-NA entry)
  n_valid <- sum(apply(x, 1L, function(row) any(!is.na(row))))
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Early return when no candidate has any grade
  if (all(is.na(medians_init))) {
    summary_df <- data.frame(
      candidate = factor(cand_names, levels = cand_names, ordered = TRUE),
      median = rep(NA_real_, n_candidates),
      p_above = rep(NA_real_, n_candidates),
      p_below = rep(NA_real_, n_candidates),
      rank = rep(1L, n_candidates),
      stringsAsFactors = FALSE
    )
    out <- list(
      summary = summary_df,
      winners = character(0),
      n_voters = n_voters,
      n_valid = n_valid,
      n_candidates = n_candidates,
      grade_distribution = as.data.frame(grade_dist),
      method = list(type = inferred_type,
                    num_categories = K,
                    median_tie_break = median_tie_break,
                    ties = ties)
    )
    if (isTRUE(return_grades)) out$grades <- grades
    class(out) <- "majority_judgement_result"
    return(out)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Build per-candidate ordering key (smaller key = better candidate).
  # Components:
  #   - rank by median: NA goes to the end; HIGHER median is better
  #     (we negate the median so radix-ascending sort puts highest median first)
  #   - sign code: "+" -> 1, "0" -> 2, "-" -> 3
  #   - tie-break value:
  #       "+"   -> -p_better (larger p_better -> more negative -> better)
  #       "-"   ->  p_worse  (smaller p_worse  -> better)
  #       "0"   -> 0
  build_gauge_keys <- function(med, pb, pw, sgn) {
    sign_code <- ifelse(is.na(sgn), 4L,
                        ifelse(sgn == "+", 1L,
                               ifelse(sgn == "0", 2L, 3L)))
    tb_val <- ifelse(is.na(sgn), 0,
                     ifelse(sgn == "+", -pb,
                            ifelse(sgn == "-",  pw, 0)))
    list(median = med, sign_code = sign_code, tb = tb_val)
  }

  keys <- build_gauge_keys(medians_init, p_better, p_worse, gauge_sign)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # iterative tie-break via "majority sequence":
  # For each tied candidate compute the sequence of MJ-medians obtained by
  # repeatedly removing the MJ-median from its sorted grade vector until empty.
  # Two candidates are then compared lexicographically on these sequences:
  # the one whose sequence is larger (better median earlier) ranks higher.
  # Returns dense within-group ranks (1..g, with ties preserved).
  iterative_break <- function(tied_idx) {
    compute_seq <- function(j) {
      v <- sort(grades[, j][!is.na(grades[, j])])
      if (!length(v)) return(integer(0))
      seq_out <- integer(length(v))
      for (i in seq_along(v)) {
        n <- length(v)
        idx <- floor((n + 1L) / 2L)
        seq_out[i] <- v[idx]
        v <- v[-idx]
      }
      seq_out
    }

    seqs <- lapply(tied_idx, compute_seq)

    # Pad shorter sequences with sentinel = 0 (worse than any real grade), so a
    # candidate who runs out of grades first is treated as worse beyond that point.
    # Note: real grades are >= 1, so 0 < any grade.
    max_len <- max(vapply(seqs, length, integer(1)))
    if (max_len == 0L) {
      return(rep(1L, length(tied_idx)))
    }
    sentinel <- 0L
    width <- nchar(format(K))
    fmt <- paste0("%0", width, "d")

    seq_keys <- vapply(seqs, function(s) {
      padded <- c(s, rep(sentinel, max_len - length(s)))
      paste(sprintf(fmt, padded), collapse = "_")
    }, character(1))

    # Higher (lex-larger) key = better candidate.
    # Map each key to its position in the descending-sorted unique keys,
    # then rank with ties.method = "min".
    codes <- match(seq_keys, sort(unique(seq_keys), decreasing = TRUE))
    rank(codes, ties.method = "min")
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Apply median tie-break to determine ranks
  # Start with rank based on median only; resolve within-median ties by selected method.

  # Initial rank: group by (median); NA goes to the end (treated as -Inf).
  # Higher median is better -> sort groups in DESCENDING order of median.
  med_for_rank <- ifelse(is.na(medians_init), -Inf, medians_init)
  med_groups <- split(seq_len(n_candidates), med_for_rank)
  med_groups <- med_groups[order(as.numeric(names(med_groups)), decreasing = TRUE)]

  final_rank <- integer(n_candidates)
  next_rank <- 1L

  for (grp in med_groups) {
    if (length(grp) == 1L) {
      final_rank[grp] <- next_rank
      next_rank <- next_rank + 1L
      next
    }

    # Multiple candidates with same median -> apply median_tie_break
    if (median_tie_break == "gauge") {
      # Sub-order by (sign_code, tb) among grp
      sub_key1 <- keys$sign_code[grp]
      sub_key2 <- keys$tb[grp]
      sub_ord <- order(sub_key1, sub_key2, method = "radix")
      ordered_grp <- grp[sub_ord]

      # Assign ranks honoring ties (same sign_code AND same tb -> same rank)
      sub_keys_combined <- paste(sub_key1[sub_ord], sub_key2[sub_ord], sep = "|")
      rle_keys <- rle(sub_keys_combined)
      pos <- 1L
      for (run_len in rle_keys$lengths) {
        final_rank[ordered_grp[seq(pos, pos + run_len - 1L)]] <- next_rank
        next_rank <- next_rank + run_len
        pos <- pos + run_len
      }

    } else {
      # iterative
      sub_ranks <- iterative_break(grp)
      # sub_ranks are 1..n among grp with possible ties; convert to global ranks
      uniq_sorted <- sort(unique(sub_ranks))
      for (sr in uniq_sorted) {
        members <- grp[sub_ranks == sr]
        final_rank[members] <- next_rank
        next_rank <- next_rank + length(members)
      }
    }
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Determine winners: those with rank == 1
  top_idx <- which(final_rank == min(final_rank))
  top_names <- cand_names[top_idx]

  winners <- if (length(top_names) == 1L) {
    top_names
  } else {
    resolve_ties(top_names, ties)
  }
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Build summary table ordered by final_rank
  ord <- order(final_rank, method = "radix")

  summary_df <- data.frame(
    candidate = factor(cand_names[ord], levels = cand_names[ord], ordered = TRUE),
    median = medians_init[ord],
    p_above = p_better[ord],
    p_below = p_worse[ord],
    rank = as.integer(final_rank[ord]),
    stringsAsFactors = FALSE
  )
  rownames(summary_df) <- NULL
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Output assembly
  out <- list(
    summary = summary_df,
    winners = winners,
    n_voters = n_voters,
    n_valid = n_valid,
    n_candidates = n_candidates,
    grade_distribution = as.data.frame(grade_dist),
    method = list(
      type = inferred_type,
      num_categories = K,
      median_tie_break = median_tie_break,
      ties = ties
    )
  )

  if (isTRUE(return_grades)) out$grades <- grades
  class(out) <- "majority_judgement_result"
  out
  #-----------------------------------------------------------------------------
}
################################################################################

################################################################################
# Printer for majority_judgement_result
print.majority_judgement_result <- function(x, digits = 1, ...) {
  stopifnot(inherits(x, "majority_judgement_result"))
  .majority_judgement_pretty_print(x, digits = digits)
  invisible(x)
}
################################################################################

################################################################################
# Summary for majority_judgement_result
summary.majority_judgement_result <- function(object, digits = 1, ...) {
  stopifnot(inherits(object, "majority_judgement_result"))
  .majority_judgement_pretty_print(object, digits = digits, title = "Majority Judgement")
  invisible(object)
}
################################################################################

################################################################################
# Internal formatter
.majority_judgement_pretty_print <- function(x, digits = 1, title = "Majority Judgement") {

  #-----------------------------------------------------------------------------
  # Summary table
  df <- x$summary
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Column access
  candidate <- df$candidate
  med <- df$median
  pb <- df$p_above
  pw <- df$p_below
  rank_v <- df$rank
  is_winner <- candidate %in% x$winners
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Totals
  n_valid <- if (!is.null(x$n_valid)) x$n_valid else 0L
  n_total <- if (!is.null(x$n_voters)) x$n_voters else n_valid
  n_cand  <- if (!is.null(x$n_candidates)) x$n_candidates else length(candidate)
  K <- x$method$num_categories
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Calculate dynamic width based on info line
  info_line <- sprintf(
    " Voters: %d/%d valid  │  Candidates: %d  │  Grades: %d  │  Type: %s  │  Median tie-break: %s  │  Ties: %s",
    n_valid, n_total, n_cand, K, x$method$type, x$method$median_tie_break, x$method$ties
  )
  info_width <- nchar(info_line)

  table_width <- max(74, info_width + 2)
  candidate_width <- 20 + (table_width - 74)

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
  header_fmt <- sprintf(
    " %%3s  %%-%ds  %%7s  %%9s  %%9s  %%6s  %%-9s\n",
    candidate_width
  )
  cat(sprintf(header_fmt, "#", "Candidate", "Median", "% Above", "% Below",
              "Rank", "Winner"))
  cat(separator_line, "\n", sep = "")
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Table rows
  row_fmt <- sprintf(
    " %%3d  %%-%ds  %%7s  %%9s  %%9s  %%6d  %%-9s\n",
    candidate_width
  )

  format_pct <- function(p) {
    if (is.na(p)) return("    NA")
    paste0(format(round(p * 100, digits), nsmall = digits), "%")
  }
  format_med <- function(m) {
    if (is.na(m)) "NA" else as.character(as.integer(m))
  }

  for (i in seq_along(candidate)) {
    winner_mark <- if (is_winner[i]) "  X  " else ""
    cat(sprintf(row_fmt, i,
                as.character(candidate[i]),
                format_med(med[i]),
                format_pct(pb[i]),
                format_pct(pw[i]),
                rank_v[i],
                winner_mark))
  }
  cat(separator_line, "\n", sep = "")
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Winner announcement
  cat("\nWinner(s): ", paste(x$winners, collapse = ", "), "\n\n", sep = "")
  #-----------------------------------------------------------------------------
}
################################################################################
