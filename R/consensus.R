#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @param site PARAM_DESCRIPTION, Default: NULL
#' @param cycle PARAM_DESCRIPTION, Default: NULL
#' @param apply_labels PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{defusing-advanced}}
#'  \code{\link[tidyr]{pivot_wider}}
#'  \code{\link[dplyr]{reexports}}
#' @rdname get_consensus
#' @export
#' @importFrom rlang ensyms
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr all_of

get_consensus <- function(
  ...,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE
) {
  variables <- as.character(rlang::ensyms(...))
  files <- get_atri_files(abcds, edc, crf_data_exclude_phi, latest)
  data <- import_atri_file(abcds, files, consensus)

  ids <- get_ids(data)

  consensus <- data[data$dd_field_name %in% as.character(variables), ]

  if (!is.null(site)) {
    consensus <- filter_by_site(consensus, site)
  }

  if (!is.null(cycle)) {
    consensus <- filter_by_cycle(consensus, cycle)
  }

  consensus <- tidyr::pivot_wider(
    consensus,
    id_cols = dplyr::all_of(ids),
    names_from = dd_field_name,
    values_from = dd_revision_field_value
  )

  if (apply_labels) {
    consensus <- apply_labels(consensus, abcds, consensus)
  }

  return(consensus)
}
