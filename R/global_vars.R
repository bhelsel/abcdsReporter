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
  "enrollment_by_cycle",
  "demographics",
  "cognition_task_counts",
  "imaging_scan_counts",
  "mri_sequence_counts",
  "imaging_scan_counts_by_site",
  "consensus_primary_by_cycle",
  "healthhistory",
  "apoe",
  "karyotype",
  "karyotype_by_site",
  "blood",
  "csf",
  "omics"
)


.cognition_tasks <- c(
  "blockhaxby",
  "blockwisc",
  "catdog",
  "dsmse",
  "moca",
  "pegboard",
  "ppvt5",
  "radd",
  "recall",
  "tbatgs",
  "verbal",
  "vmi"
)


.mri_sequence_key <- tibble::tribble(
  ~id , ~abbreviation      , ~description                                                                        ,
  "1" , "T1 MPRAGE/ISSPGR" , "T1 MPRAGE/ISSPGR Sagittal 3D Accelerated MPRAGE/IRSPGR (ADNI3 Sequence)"           ,
  "2" , "3D FLAIR"         , "3D FLAIR Sagittal 3D FLAIR (ADNI3 Sequence)"                                       ,
  "3" , "T2*/SWI"          , "T2*/SWI Axial 3D ME T2 GRE – Axial 3D T2 GRE – Axial T2 Star/GRE"              ,
  "4" , "DTI"              , "DTI Axial DTI PA (multiband if applicable) Axial DTI AP (multiband if applicable)" ,
  "5" , "ASL"              , "ASL Axial 3D pCASL or Axial 3D PASL"                                               ,
  "6" , "T2 FSE"           , "T2 FSE Sagittal 3D T2 Weighted Sequence"                                           ,
  "7" , "rs-fMRI"          , "rs-fMRI Axial fcMRI (multiband if applicable)"                                     ,
)
