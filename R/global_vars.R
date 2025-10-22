if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "crf_data_exclude_phi",
    "coenroll_exclude_phi",
    "dd_field_name",
    "dd_revision_field_value",
    "subject_label",
    "site_label",
    "Site",
    "items",
    "edc",
    "ptdemog",
    "inner_join",
    ".",
    "latest",
    "de_race",
    "abcds",
    "full_join",
    "data_dictionary"
  ))
}


.all_reports <- c(
  "apoe",
  "consensus_primary_by_cycle",
  "demographics",
  "enrollment_by_cycle",
  "healthhistory",
  "imaging",
  "imaging_by_site",
  "karyotype",
  "karyotype_by_site",
  "omics"
)
