#' @title Retrieve Variables from ABC-DS Data

#' @description
#' Extracts one or more variables from an ABC-DS dataset using the specified
#' codebook. The function supports optional filtering by site and cycle and allows applying
#' variable labels for enhanced interpretability.

#' @param dataset A symbol or string specifying the dataset name within the ABC-DS data repository.
#' @param codebook A symbol or string specifying the corresponding codebook to use for metadata.
#' @param variables A character vector of variables to extract from the dataset
#' @param site Optional; a site identifier or vector of site codes to subset data by site. Default is `NULL`.
#' @param cycle Optional; a cycle identifier or vector of cycles to subset data by cycle. Default is `NULL`.
#' @param apply_labels Logical; if `TRUE`, applies variable labels from the codebook to the returned data. Default is `FALSE`.

#' @return
#' A data frame containing the selected variables and any applied filters (site and/or cycle).
#' If `apply_labels = TRUE`, variable labels are attached to the output as attributes.

#' @details
#' Extracts one or more variables from an ABC-DS dataset using the specified
#' codebook. The function supports optional filtering by site and cycle and allows applying
#' variable labels for enhanced interpretability.
#'
#' @seealso
#'  \code{\link[rlang]{as_string}}, \code{\link[rlang]{defusing-advanced}}
#'  \code{\link[tidyr]{pivot_wider}}
#'  \code{\link[dplyr]{reexports}}
#' @rdname get_abcds_data
#' @export
#' @importFrom rlang as_string enexpr
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr all_of

get_abcds_data <- function(
  dataset,
  codebook,
  variables,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE
) {
  dataset <- rlang::as_string(rlang::enexpr(dataset))
  codebook <- rlang::as_string(rlang::enexpr(codebook))
  files <- get_atri_files(abcds, edc, crf_data_exclude_phi, latest)
  data <- import_atri_file(abcds, files, !!dataset)

  data <- data[data$dd_field_name %in% variables, ]

  if (!is.null(site)) {
    data <- filter_by_site(data, site)
  }

  if (!is.null(cycle)) {
    data <- filter_by_cycle(data, cycle)
  }

  ids <- get_ids(data)

  # Exam date is found in translated value instead of field value
  if ("examdate" %in% variables) {
    data[, "dd_revision_field_value"] <- data[,
      "dd_revision_field_translated_value"
    ]
  }

  data <- tidyr::pivot_wider(
    data,
    id_cols = dplyr::all_of(ids),
    names_from = dd_field_name,
    values_from = dd_revision_field_value
  )

  if (apply_labels) {
    data <- apply_labels(data, abcds, !!codebook)
  }

  data$site_label <- gsub("^(Puerto Rico University).*", "\\1", data$site_label)

  return(data)
}
