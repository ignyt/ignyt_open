bone_exclusive <- paste0(
  c(
    # CT
    "DIEP Angiography",
    "Musculoskeletal",
    "Extremity",
    "Runoff",
    "Perforator",
    # MR
    "SOFT TISSUE",
    "BONE"
  ),
  collapse = "|"
)


# usethis::use_data(bone_exclusive, overwrite = TRUE, internal = TRUE)
