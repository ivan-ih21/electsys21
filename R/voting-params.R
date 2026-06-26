#' Common arguments for the voting functions
#'
#' Central definitions of the arguments shared across the voting functions in
#' \pkg{electsys21}. These descriptions are pulled into each function's help
#' page via `@inheritParams`, so the wording only has to be maintained in one
#' place. This page is for internal reference and is not meant to be called.
#'
#' @param x A matrix of ballots with one row per voter and one column per
#'   candidate. Accepts a ranking matrix (rank `1` = most preferred), a utility
#'   (score) matrix (higher = more preferred), or a binary/logical approval
#'   matrix. Column names are used as candidate names; if absent, candidates are
#'   named `Candidate_1`, `Candidate_2`, and so on.
#' @param type Character string giving the ballot type. One of `"auto"` (the
#'   default, detected from `x`), `"rank"`, `"utility"` or `"approval"`.
#' @param ties Character string giving the tie-breaking rule applied when
#'   several candidates are tied for a winning position. One of `"random"` (the
#'   default; pick one tied candidate at random), `"lexicographic"` (pick the
#'   alphabetically first) or `"all"` (return all tied candidates).
#' @param return_ranks Logical. If `TRUE`, the ranking matrix derived
#'   internally is attached to the result as `$ranks`. Defaults to `FALSE`.
#'
#' @keywords internal
#' @name voting-params
NULL
