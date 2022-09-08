
dmts <- c(
  "gu",
  "gyn",
  "hpb",
  "chest",
  "rectal",
  "melanoma",
  "lymphoma",
  "bone",
  "rare_syn"
)

strings_desc_gu <- c(
  "prost", "urogr", "kidney", "renal", "bladder", "cystogr"
) %>% toupper()

strings_desc_gyn <- c(
  "uterus", "cervix", "vulva", "ovary"
) %>% toupper()

strings_desc_hpb <- c(
  "Pancreas: Pre-Op Angio", "Liver Angio"
)

strings_desc_colorect <- c(
  "Rectum", "Anus", "Enterography"
)

strings_desc_bone <- c(
  toupper(c("bone", "soft tissue")),
  "Bone", "Soft tissue", "Soft Tissue",
  "Musculoskeletal"
)

strings_desc_rare_syn <- c(
  "Neurofibromatosis", "Rare Syndome"
)

strings_desc_lymphoma <- c(
  "Lymphoma", "Leukemia"
)

match_desc_list <- list(
  "gu" = strings_desc_gu,
  "gyn" = strings_desc_gyn,
  "hpb" = strings_desc_hpb,
  "liver_iron" = "IRON",
  "rectal" = strings_desc_colorect,
  "chest" = "Airway",
  "lymphoma" = strings_desc_lymphoma,
  "bone" = strings_desc_bone,
  "myeloma" = "Multiple Myeloma",
  "syndromes" = strings_desc_rare_syn
)

# usethis::use_data(match_desc_list, overwrite = TRUE, internal = TRUE)
