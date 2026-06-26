################################################################################
# Render a single-metric result table by voting methods
# (fptp, borda, approval, d21)

print_results_table <- function(candidate, value, percentage, rank, winners,
                                 title, info_line, value_label,
                                 digits = 1, extras = NULL) {
  #-----------------------------------------------------------------------------
  # Dynamic widths
  table_width     <- max(71L, nchar(info_line) + 2L)
  candidate_width <- 20L + (table_width - 71L)
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Header
  cat("\n")
  header_line <- strrep("\u2550", table_width)
  cat(header_line, "\n", sep = "")
  cat(" ", title, "\n", sep = "")
  cat(header_line, "\n", sep = "")

  cat(info_line, "\n")

  separator_line <- strrep("\u2500", table_width)
  cat(separator_line, "\n", sep = "")
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Table header
  header_fmt <- sprintf(" %%3s  %%-%ds  %%10s  %%-11s  %%6s  %%-9s\n", candidate_width)
  cat(sprintf(header_fmt, "#", "Candidate", value_label, "%", "Rank", "Winner"))
  cat(separator_line, "\n", sep = "")
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Table rows
  cand_chr  <- as.character(candidate)
  is_winner <- cand_chr %in% winners
  row_fmt   <- sprintf(" %%3d  %%-%ds  %%10d  %%-11s  %%6d  %%-9s\n", candidate_width)
  pct_chr   <- paste0(format(round(percentage, digits), nsmall = digits), "%")
  for (i in seq_along(cand_chr)) {
    winner_mark <- if (is_winner[i]) "  X  " else ""
    cat(sprintf(row_fmt, i, cand_chr[i], as.integer(value[i]), pct_chr[i],
                as.integer(rank[i]), winner_mark))
  }
  cat(separator_line, "\n", sep = "")
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Total row
  total_fmt <- sprintf("      %%-%ds  %%10d  %%-11s\n", candidate_width)
  total_pct <- paste0(format(round(sum(percentage), digits), nsmall = digits), "%")
  cat(sprintf(total_fmt, "TOTAL", as.integer(sum(value)), total_pct))
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Method-specific footer lines
  if (is.function(extras)) extras()
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # Winner announcement
  cat("\nWinner(s): ", paste(winners, collapse = ", "), "\n\n", sep = "")
  #-----------------------------------------------------------------------------
  invisible(NULL)
}
################################################################################
