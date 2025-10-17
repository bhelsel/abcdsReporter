get_abcds_data <- function(
  dataset,
  codebook,
  variables,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE
) {
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
