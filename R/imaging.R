#' @title Retrieve ABC-DS Imaging Data
#'
#' @description
#' Retrieves imaging metadata from the ABC-DS study in the ATRI EDC system
#' and returns a clean, analysis-ready dataset. Supports filtering by
#' site and study cycle, selecting specific imaging variables, and applying
#' variable and factor labels.
#'
#' @param ... One or more unquoted variable names to select from the imaging dataset.
#' @param site Optional. A site code, site initials, or partial site label to filter
#'   the dataset. Defaults to \code{NULL}, returning all sites.
#' @param cycle Optional. A numeric cycle or month value to filter the dataset.
#'   Defaults to \code{NULL}, returning all cycles.
#' @param apply_labels Logical. Whether to apply variable and factor labels from
#'   the ABC-DS data dictionary. Defaults to \code{FALSE}.
#' @param imaging Character. The imaging dataset to retrieve. Must be one of
#'   \code{"amymeta"}, \code{"fdgmeta"}, or \code{"mrimeta"}. Defaults to
#'   \code{c("amymeta", "fdgmeta", "mrimeta")}, and the first matching option
#'   will be selected.
#'
#' @return
#' A \code{\link[tibble]{tibble}} with participant identifiers and the selected
#' imaging variables. If \code{apply_labels = TRUE}, variable and factor labels
#' from the ABC-DS data dictionary are applied.
#'
#' @details
#' The \code{get_imaging()} function simplifies retrieval of imaging metadata
#' for ABC-DS participants. It performs the following steps:
#' \enumerate{
#'   \item Imports the selected imaging dataset from ATRI EDC using \code{import_atri_file}.
#'   \item Filters the dataset for the requested variables, site, and cycle.
#'   \item Reshapes the dataset from long to wide format with one row per participant.
#'   \item Optionally applies labels using \code{\link{apply_labels}}.
#' }
#' The function supports multiple imaging datasets (\code{"amymeta"}, \code{"fdgmeta"},
#' \code{"mrimeta"}) and ensures that the resulting data is ready for analysis or
#' merging with other ABC-DS datasets.
#'
#' @seealso
#'  \code{\link{filter_by_site}},
#'  \code{\link{filter_by_cycle}},
#'  \code{\link{apply_labels}},
#'  \code{\link{get_ids}},
#'  \code{\link[tidyr]{pivot_wider}}
#'
#' @examples
#' \dontrun{
#' # Retrieve selected MRI metadata for all sites and cycles
#' mri_data <- get_imaging(done, imaging = "mrimeta")
#'
#' # Retrieve labeled FDG metadata for site "KU" and cycle 2
#' fdg_data <- get_imaging(
#'   done,
#'   site = "KU",
#'   cycle = 2,
#'   apply_labels = TRUE,
#'   imaging = "fdgmeta"
#' )
#' }
#'
#' @rdname get_imaging
#' @export
#' @importFrom tidyr pivot_wider

get_imaging <- function(
    ...,
    site = NULL,
    cycle = NULL,
    apply_labels = FALSE,
    imaging = c("amymeta", "fdgmeta", "mrimeta")
) {
    variables <- as.character(rlang::ensyms(...))
    imaging <- match.arg(imaging)
    files <- get_atri_files(abcds, edc, crf_data_exclude_phi, latest)
    data <- import_atri_file(abcds, files, !!imaging)

    imaging_data <- data[data$dd_field_name %in% as.character(variables), ]

    if (!is.null(site)) {
        imaging_data <- filter_by_site(imaging_data, site)
    }

    if (!is.null(cycle)) {
        imaging_data <- filter_by_cycle(imaging_data, cycle)
    }

    ids <- get_ids(data)

    imaging_data <- tidyr::pivot_wider(
        imaging_data,
        id_cols = ids,
        names_from = dd_field_name,
        values_from = dd_revision_field_value,
        values_fn = max
    )

    if (apply_labels) {
        imaging_data <- apply_labels(imaging_data, abcds, !!imaging)
    }

    return(imaging_data)
}
