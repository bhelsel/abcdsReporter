#' Count occurrences by position across IDs
#'
#' This function counts how many IDs have a value of 1 at each sequential
#' position. It's useful for understanding the distribution of repeated
#' measurements or events across subjects.
#'
#' @param data A data frame containing the data to analyze
#' @param id An unquoted column name representing the ID variable to group by
#' @param ... Grouping variables to count by (e.g., site_label)
#' @param column An unquoted column name to filter and count
#' @param value The value to filter for in the column (default: 1)
#'
#' @return A data frame with:
#'   \item{variable}{The name of the column that was counted}
#'   \item{1, 2, 3, ...}{Columns representing each position, with values
#'         indicating how many IDs had an occurrence at that position}
#'
#' @examples
#' # Count how many subjects completed a task at each time point
#' count_by(atri_cognition, ids, crtt_done)
#'
#' # Use with map to count multiple variables
#' purrr::map(
#'   c("crtt_done", "dsmse_done"),
#'   ~ count_by(atri_cognition, ids, !!rlang::sym(.x))
#' )
#'
#' @export

count_by <- function(data, id, ..., column, value = 1) {
  data %>%
    dplyr::filter({{ column }} == value) %>%
    dplyr::group_by({{ id }}) %>%
    dplyr::mutate(position = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::count(..., position, name = "total") %>%
    tidyr::pivot_wider(
      names_from = position,
      values_from = total,
      values_fill = 0
    ) %>%
    dplyr::filter(dplyr::if_any(c(...), ~ !is.na(.x))) %>%
    dplyr::mutate(variable = rlang::as_label(rlang::enquo(column)), .before = 1)
}

#' Count Multiple Variables and Combine Results
#'
#' Iterates over multiple variable names, counts occurrences by specified ID
#' columns, and combines the results into a single summary table with custom
#' labels. This is useful for creating frequency tables across multiple related
#' variables.
#'
#' @param data A data frame containing the variables to be counted.
#' @param names Character vector of column names in \code{data} to count.
#'   These are the actual variable names as they appear in the dataset.
#' @param labels Character vector of display labels corresponding to each name
#'   in \code{names}. Must be the same length as \code{names}. These labels
#'   will replace the variable names in the output.
#' @param ids Unquoted column name(s) to group by when counting. Can be a single
#'   variable or multiple variables using \code{c()}.
#' @param varname Character string specifying the name for the variable column
#'   in the output. Default column name "variable" will be renamed to this value.
#'
#' @return A tibble with the following structure:
#'   \itemize{
#'     \item A column with the name specified by \code{varname} containing the
#'       labels from the \code{labels} parameter
#'     \item Columns for each unique value in the grouping variable(s), containing
#'       counts of occurrences
#'     \item All \code{NA} values in numeric columns are converted to 0
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Iterates through each variable name and its corresponding label using
#'     \code{purrr::map2()}
#'   \item For each variable, calls \code{abcdsReporter::count_by()} to count
#'     occurrences grouped by the specified ID column(s)
#'   \item Replaces the first column (variable identifier) with the custom label
#'   \item Combines all individual count tables into a single data frame
#'   \item Converts any \code{NA} values in numeric columns to 0 for cleaner output
#'   \item Renames the variable column to the specified \code{varname}
#' }
#'
#' This function is particularly useful for creating summary tables that show
#' counts across multiple related variables (e.g., multiple imaging sequences,
#' multiple assessment types) with human-readable labels.
#'
#' @examples
#' \dontrun{
#' # Count multiple MRI sequences by participant group
#' mri_summary <- multiple_count_by(
#'   data = imaging_data,
#'   names = c("mri_t1", "mri_flair", "mri_dti"),
#'   labels = c("T1 MPRAGE", "3D FLAIR", "DTI"),
#'   ids = subject_label,
#'   varname = "MRI_Sequence"
#' )
#'
#' # Count assessments by subject and site
#' assessment_summary <- multiple_count_by(
#'   data = assessments,
#'   names = c("cdr_done", "mmse_done", "faq_done"),
#'   labels = c("CDR", "MMSE", "FAQ"),
#'   ids = subject_label,
#'   site_label,
#'   varname = "Assessment"
#' )
#' }
#'
#' @seealso \code{\link{count_by}}
#'
#' @export

multiple_count_by <- function(data, names, labels, ids, ..., varname) {
  purrr::map2(names, labels, .f = function(x, y) {
    counts <- abcdsReporter::count_by(
      data = data,
      id = ids,
      ...,
      column = !!rlang::sym(x)
    )
    counts[[1]] <- y
    return(counts)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric),
      ~ ifelse(is.na(.x), 0, .x)
    )) %>%
    dplyr::rename(!!varname := variable)
}
