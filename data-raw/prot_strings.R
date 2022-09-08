
dmts <- c(
  "gu",
  "gyn",
  "hpb",
  "rectal",
  "melanoma",
  "lymphoma"
)

strings_gu <- c(
  "prost", "urogram", "kidney", "renal", "bladder", "cystogram"
) %>% toupper()

strings_hpb <- c(
  "cholangiogram", "liver"
) %>% toupper()

strings_colorect <- c(
  "rectum", "anus", "enterography"
) %>% toupper()

match_prot_list <- list(
  "gu" = strings_gu,
  # "hpb" = strings_hpb, # too inclusive #17 and #23
  "rectal" = strings_colorect,
  "lymphoma" = "LYMPHOMA",
  "myeloma" = "MYELOMA",
  "bone" = "EXTR",
  "syndromes" = "RARE",
  "gyn" = "GYN"
)

# usethis::use_data(match_prot_list, overwrite = TRUE, internal = TRUE)
