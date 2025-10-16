#' @title Summarize a Variable by Groups
#'
#' @description
#' Computes summary statistics for a numeric variable grouped by one or more
#' categorical variables. Supports common statistics including mean, median,
#' standard deviation, minimum, and maximum.
#'
#' @param data A data frame or tibble containing the variable and grouping columns.
#' @param variable The numeric variable to summarize (unquoted).
#' @param groups A character vector of column names to group by. Defaults to
#'   \code{c("site_initials", "site_label", "event_label")}.
#' @param stats A character vector of statistics to compute. Options include
#'   \code{"mean"}, \code{"median"}, \code{"sd"}, \code{"min"}, \code{"max"}.
#'   Defaults to all five.
#'
#' @return
#' A \code{\link[tibble]{tibble}} containing one row per group combination and
#' columns for each requested statistic, with names in the format
#' \code{<stat>_<variable>}.
#'
#' @details
#' The function constructs a formula of the form \code{variable ~ group1 + group2 + ...}
#' and applies each requested statistic using \code{stats::aggregate}. Results
#' are then merged together using \code{atri_join} to produce a single tibble.
#'
#' @examples
#' \dontrun{
#' # Compute mean and sd of 'de_score' by site and event
#' summarize_by(demo_data, variable = de_score, stats = c("mean", "sd"))
#' }

summarize_by <- function(
  data,
  variable = NULL,
  groups = c("site_initials", "site_label", "event_label"),
  stats = c("mean", "median", "sd", "min", "max")
) {
  variable <- rlang::ensym(variable)

  fm <- paste0(
    rlang::as_string(variable),
    " ~ ",
    paste(groups, collapse = " + ")
  )

  newnames <- paste0(stats, "_", rlang::as_string(variable))

  aggregated_data <- purrr::map2(
    stats,
    newnames,
    function(x, y) {
      res <- stats::aggregate(
        stats::as.formula(fm),
        data = data,
        FUN = match.fun(x),
        na.rm = TRUE
      )
      colnames(res)[ncol(res)] <- y
      return(res)
    }
  )

  aggregated_data_combined <-
    purrr::reduce(
      aggregated_data,
      atri_join,
      by = groups,
      join_type = inner_join
    )

  return(tibble::as_tibble(aggregated_data_combined))
}
