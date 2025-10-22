#' @title Retrieve Registry Variables from ABC-DS Data
#' @description
#' Extracts one or more registry variables (e.g., examdate) from an ABC-DS dataset using the specified
#' codebook. The function supports optional filtering by site and cycle and allows applying
#' variable labels for enhanced interpretability.
#'
#' @param ... One or more unquoted variable names to retrieve from the dataset.
#' @param dataset A symbol or string specifying the dataset name within the ABC-DS data repository.
#' @param codebook A symbol or string specifying the corresponding codebook to use for metadata.
#' @param site Optional; a site identifier or vector of site codes to subset data by site. Default is `NULL`.
#' @param cycle Optional; a cycle identifier or vector of cycles to subset data by cycle. Default is `NULL`.
#' @param apply_labels Logical; if `TRUE`, applies variable labels from the codebook to the returned data. Default is `FALSE`.
#'
#' @return
#' A data frame containing the selected variables and any applied filters (site and/or cycle).
#' If `apply_labels = TRUE`, variable labels are attached to the output as attributes.
#'
#' @details
#' This function provides a convenient wrapper around get_abcds_data
#' to streamline access to ABC-DS registry variables. Quasiquotation is used to support
#' tidy evaluation, allowing unquoted variable names and symbol references.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # Retrieve selected health variables for a specific site and cycle
#'   registry_data <- get_health(
#'     examdate,
#'     dataset = registry,
#'     codebook = registry,
#'     apply_labels = TRUE
#'   )
#' }
#' }
#'
#'
#' @seealso
#'  \code{\link[rlang]{as_string}}, \code{\link[rlang]{defusing-advanced}},
#'
#' @rdname get_registry
#' @export
#' @importFrom rlang as_string enexpr ensyms

get_registry <- function(
  ...,
  dataset,
  codebook,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE
) {
  dataset <- rlang::as_string(rlang::enexpr(dataset))
  codebook <- rlang::as_string(rlang::enexpr(codebook))
  variables <- as.character(rlang::ensyms(...))
  get_abcds_data(
    dataset,
    codebook,
    variables,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels
  )
}
