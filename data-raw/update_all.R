all_files <- list.files("data-raw", ".R", full.names = TRUE)
invisible(sapply(all_files[!stringr::str_detect(all_files, "update_all")], source))

usethis::use_data(loc_by_state, fac_dmts, fac_dmts_ct, fellow_table,
  fac_dmts_mr, exam_weights, min_exams,
  match_desc_list, match_hx_list, match_hx_ct_list, ref_dmts,
  match_prot_list, test_week, bone_exclusive, test_faculty, 
  testweek_days, ct_exclusives, ref_peds, dmt_meetings,
  overwrite = TRUE, internal = TRUE
)
