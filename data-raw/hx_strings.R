library(stringr)
permute_cases <- function(v) {
  unique(c(toupper(v), tolower(v), str_to_title(v), str_to_sentence(v)))
}

strings_hx_gu <- c(
  "renal", "kidney", "testicular", "scrotal", "scrotum", "urethral", "bladder", "prost", "psa"
)

strings_hx_gyn <- c(
  "cervical", "ovary", "uter", "cervix", "ovarian", "menopaus",
  "endometr", "borderline tumor"
)

strings_hx_hpb <- c(
  "liver", "hcc", "hepatic", "pancrea", "hepatocellular", "biliary",
  "gallbladder", "cholangio"
)

strings_hx_colorect <- c(
  "rectal", "colorectal", "sigmoid", "colon"
)

strings_hx_chest <- c(
  "lung", "bronchial", "pulmonary", "nsclc"
)

strings_hx_bone <- c(
  "sarcoma", "desmoid", "lipoma"
)

strings_hx_lymphoma <- c(
  "lymphoma", "dlbcl", "leukemia", "cll", "malt"
)

strings_hx_rare_syn <- c(
  "neurofibromatosis", "rare syndome"
)

strings_peds <- c(
  "neuroblasoma", "nephroblastoma"
)

strings_gastric <- c(
  "gastric", "linitis plastica"
)

strings_esophageal <- c(
  "gastroesophageal", "esophageal", "EG junction", "GE junction", "gej"
)

match_hx_list <- list(
  "peds" = strings_peds,
  "lymphoma" = strings_hx_lymphoma,
  "gu" = strings_hx_gu,
  "gyn" = strings_hx_gyn,
  "hpb" = strings_hx_hpb,
  "chest" = strings_hx_chest,
  "rectal" = strings_hx_colorect,
  "bone" = strings_hx_bone,
  "syndromes" = strings_hx_rare_syn,
  "myeloma" = "myeloma"
) %>%
  lapply(permute_cases)

# usethis::use_data(match_hx_list, overwrite = TRUE, internal = TRUE)

match_hx_ct_list <- list(
  "gastric" = strings_gastric,
  "esophageal" = strings_esophageal,
  "thyroid" = "thyroid",
  "breast" = c("breast", "DCIS", "phyllodes")
) %>%
  lapply(permute_cases)
