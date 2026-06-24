################################################################################
# Binary matrix detection helper
is_binary_matrix <- function(mat) {
  vals <- mat[!is.na(mat)]
  length(vals) > 0L && all(vals %in% c(0, 1))
}
################################################################################

################################################################################
# Ranking matrix detection helper
is_ranking_matrix <- function(mat) {
  vals <- mat[!is.na(mat) & mat != 0]
  if (length(vals) == 0L) return(FALSE)
  is_integer <- all(vals %% 1 == 0)
  if (!is_integer) return(FALSE)
  vals <- as.integer(vals)
  vals <- vals[vals > 0L]
  if (!length(vals)) return(FALSE)
  max_rank <- max(vals)
  if (max_rank > ncol(mat)) return(FALSE)
  unique_vals <- sort(unique(vals))
  expected_vals <- seq_len(max_rank)
  length(unique_vals) == length(expected_vals) && all(unique_vals == expected_vals)
}
################################################################################

################################################################################
# Coerce `x` to a numeric/logical matrix, validate it is non-empty, and ensure
# candidate (column) names exist. Returns the prepared matrix; callers read
# nrow()/ncol()/colnames() from the result
prepare_ballots <- function(x) {
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  if (!is.matrix(x)) {
    stop("`x` must be a matrix or data.frame.", call. = TRUE)
  }
  if (!is.numeric(x) && !is.logical(x)) {
    stop("`x` must contain numeric or logical values.", call. = TRUE)
  }
  if (nrow(x) == 0L || ncol(x) == 0L) {
    stop("`x` must have at least 1 row (voter) and 1 column (candidate).", call. = TRUE)
  }
  if (is.null(colnames(x))) {
    colnames(x) <- paste0("Candidate_", seq_len(ncol(x)))
  }
  x
}
################################################################################

################################################################################
# Build a numeric ranking matrix (rank 1 = best, NA = not ranked) from an
# approval, rank, or utility ballot matrix
build_ranks <- function(x, inferred_type, context = NULL) {
  n_voters     <- nrow(x)
  n_candidates <- ncol(x)
  suffix <- if (is.null(context)) "" else paste0(" for ", context)

  switch(
    inferred_type,
    approval = {
      # Must be logical or {0, 1} numeric matrix
      if (!is.logical(x) && !is_binary_matrix(x)) {
        stop("`type = \"approval\"` requires a logical matrix or a {0, 1} numeric matrix.",
             call. = TRUE)
      }
      # At most one TRUE/1 per row: rank-based methods would otherwise tie every
      # approved candidate at rank 1 and over-award them.
      vote_counts <- rowSums(x == 1 | (is.logical(x) & x), na.rm = TRUE)
      if (any(vote_counts > 1)) {
        stop(sprintf("Approval/logical matrix must have at most one TRUE/1 per row (voter)%s.", suffix),
             call. = TRUE)
      }
      # Convert to rank matrix: 1 where TRUE/1, NA elsewhere
      r <- matrix(NA_real_, nrow = nrow(x), ncol = ncol(x))
      r[x == 1 | (is.logical(x) & x & !is.na(x))] <- 1
      dimnames(r) <- dimnames(x)
      r
    },
    rank = {
      r <- x
      vals <- r[!is.na(r) & r != 0]
      if (!all(abs(vals - round(vals)) < .Machine$double.eps^0.5)) {
        stop("`type='rank'` requires integer ranks.", call. = TRUE)
      }
      if (any(vals < 0)) {
        stop("Negative ranks are not allowed.", call. = TRUE)
      }
      if (length(vals) && any(vals > n_candidates)) {
        stop("Ranks must not exceed the number of candidates.", call. = TRUE)
      }
      n_per_row      <- rowSums(!is.na(r) & r > 0)
      n_uniq_per_row <- apply(r, 1L, function(row) length(unique(row[!is.na(row) & row > 0])))
      if (any(n_per_row != n_uniq_per_row)) {
        bad <- which(n_per_row != n_uniq_per_row)[1]
        stop(sprintf("Voter %d has duplicate ranks; each rank must appear at most once per voter.", bad),
             call. = TRUE)
      }
      mode(r) <- "numeric"
      r
    },
    utility = {
      # Higher utility -> better rank; ties resolved at random
      r <- matrix(
        apply(x, 1L, function(row) {
          out <- rep(NA_real_, length(row))
          idx <- which(!is.na(row))
          if (length(idx)) {
            out[idx] <- rank(-row[idx], ties.method = "random")
          }
          out
        }),
        nrow = n_voters, ncol = n_candidates, byrow = TRUE
      )
      colnames(r) <- colnames(x)
      r
    }
  )
}
################################################################################