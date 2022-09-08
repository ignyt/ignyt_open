#' divide_ct core function
#'
#' Creates dataframe with CT's assigned to attendings.
#' Expects dataframes with specific columns as returned
#' by the "prep" functions
#'
#' Returns a list containing two dataframes:
#' 1. Assigned cases
#' 2. Leftovers
#' and a 2 x n matrix:
#' 3. Bootstrap iteration | Optimization parameter (more = better)
#'
#' The latter is mostly for academic/testing purposes and can be ignored
#' in production.
#'
#' @param wl,atts Data frames (tibbles) as produced by "prep" functions.
#' @param bootstrap_samples Number of bootstrapped optimization samples. Defaults to 50.
#' @param late_dry Don't give wet reads to people on a late shift (parameter "swim" in df.)
#' @param bypass_ps Bypasses check of PS360 database (kicks out dictated/queued exams). Defaults to FALSE.
#'
#' @import dplyr stringr
#'
#' @examples
#' worklist <- prep_worklist(sim_worklist(1))
#' ct_att <- sim_shift_rads(1, "CT", "am")
#' assignments <- divide_ct(worklist, ct_att, 2, FALSE, TRUE)
#' @export

divide_ct <- function(wl, atts, bootstrap_samples = 20, late_dry = FALSE, bypass_ps = FALSE) {
  environment(pat_move_df) <- environment()
  
  if (!is.logical(bypass_ps)) {
    warning("Invalid bypass_ps value, falling back to FALSE")
    bypass_ps <- FALSE
  }
  
  ct_att_blank <- mutate(atts, assigned_exams = NA)
  
  ct_att_blank$assigned_exams %<>% as.list()
  
  wl %<>% filter(modality_code == "CT") # Do separately to avoid wl containing MRIs for pat_move_df
  todo_ct <- wl %>% arrange(desc(wet_read_request), ris_exam_end_dt)
  
  # PS integrity check -----------------------------------------------------
  
  if (bypass_ps == FALSE) {
    todo_wl_ps <- left_join(
      todo_ct,
      ps_get_status(todo_ct$ris_accession_number),
      by = "ris_accession_number"
    )
    log_not_assigned_ps <- todo_wl_ps %>% filter(ps_status != "Completed")
    todo_wl_ps %<>% filter(ps_status == "Completed")
  } else if (bypass_ps == TRUE) {
    todo_wl_ps <- todo_ct %>% mutate(ps_status = "Completed")
    log_not_assigned_ps <- todo_wl_ps[0, ]
  }
  
  if (nrow(todo_wl_ps) == 0) {
    return(list("assignments" = todo_wl_ps, "not_assigned" = log_not_assigned_ps, "mc_matrix" = matrix()))
  }
  
  # Filter inpatients and UCC -----------------------------------------------
  
  log_not_assigned_ps %<>% bind_rows(
    todo_wl_ps %>%
      filter(patient_status_code == "I" | str_detect(patient_location_code, "UCC|SCC|ER")) %>%
      mutate(ps_status = "UCC or inpatient")
  )
  
  todo_wl_ps %<>% filter(!ris_accession_number %in% log_not_assigned_ps$ris_accession_number)
  
  if (nrow(todo_wl_ps) == 0) {
    return(list("assignments" = todo_wl_ps, "not_assigned" = log_not_assigned_ps, "mc_matrix" = matrix()))
  }
  
  # Bootstrap "for" loop ----------------------------------------------------
  
  loss_var <- 0
  bootstrap_loop <- 0
  bootstrap_iterations <- matrix(c(0, 0), ncol = 2)
  min_assignments <- sum(todo_wl_ps$exam_weighting, na.rm = TRUE) / nrow(ct_att_blank)
  
  for (m in 1:bootstrap_samples) {
    bootstrap_loop <- bootstrap_loop + 1
    
    wct_available <- sum(todo_wl_ps$exam_weighting, na.rm = TRUE)
    rad_att <- ct_att_blank[sample(nrow(ct_att_blank)), ] # Random shuffle for fairness and optim.
    todo_ex_wl <- todo_wl_ps
    log_not_assigned <- todo_ex_wl[0, ]
    ct_late <- rad_att[0, ]
    
    # Fill readers up successively:
    while (any(rad_att$min_exams > 0) & nrow(todo_ex_wl) > 0) {
      old_min_sum <- sum(rad_att$min_exams, na.rm = TRUE) # Prevent endless loops
      # print(old_min_sum)
      
      # Exclude late people from receiving wet reads (only main AM round) ----
      
      if (late_dry == TRUE & nrow(rad_att[which(rad_att$swim == FALSE), ]) > 0 & any(todo_ex_wl$wet_read_request == "wet_read")) {
        ct_late <- rad_att %>% filter(swim == FALSE)
        rad_att %<>% filter(swim == TRUE)
      }
      
      # Assign exclusive exams first --------------------------------------
      # Bone exclusives
      
      # print("Assigning bone cases")
      # step <- "bone"
      bone_exclusive_exams <- todo_ex_wl %>% filter(str_detect(protocol_description, bone_exclusive) | service == "Muscl")
      while (nrow(bone_exclusive_exams) > 0 & any(rad_att[which(rad_att$dmt.1 == "bone"), ]$min_exams > 0)) {
        if (nrow(rad_att[which(rad_att$dmt.1 == "bone"), ]) == 0) break
        if ("NJ" %in% bone_exclusive_exams$state_code & !"NJ" %in% rad_att[which(rad_att$dmt.1 == "bone" & rad_att$min_exams > 0), ]$nj.license) {
          log_not_assigned %<>% force_rbind(
            bone_exclusive_exams %>%
              mutate(ps_status = "No NJ licensed bone attendings available")
          )
          todo_ex_wl %<>% filter(!ris_accession_number %in% log_not_assigned$ris_accession_number)
          break
        }
        for (j in 1:nrow(rad_att[which(rad_att$dmt.1 == "bone"), ])) {
          if (nrow(bone_exclusive_exams) == 0) break
          if (rad_att[which(rad_att$dmt.1 == "bone"), ][j, ]$min_exams <= 0) next
          if (bone_exclusive_exams[1, ]$state_code %in% rad_att[which(rad_att$dmt.1 == "bone"), ][j, ][c("nj.license", "ny.license")]) {
            # $
            rad_att[which(rad_att$dmt.1 == "bone"), ][j, ]$assigned_exams[[1]] %<>% force_rbind(bone_exclusive_exams[1, ])
            todo_ex_wl %<>% filter(ris_accession_number != bone_exclusive_exams[1, ]$ris_accession_number)
            rad_att[which(rad_att$dmt.1 == "bone"), ][j, ]$min_exams <- rad_att[which(rad_att$dmt.1 == "bone"), ][j, ]$min_exams - bone_exclusive_exams[1, ]$exam_weighting
            rad_att[which(rad_att$dmt.1 == "bone"), ][j, ]$sum_assigned <- rad_att[which(rad_att$dmt.1 == "bone"), ][j, ]$sum_assigned + bone_exclusive_exams[1, ]$exam_weighting
            lkp_mrn <- bone_exclusive_exams[1, ]$ris_mrn
            bone_exclusive_exams %<>% slice(-1)
            pat_move_df(flavor = "dual-only")
            bone_exclusive_exams %<>% filter(ris_mrn != lkp_mrn)
            todo_ex_wl %<>% filter(!ris_accession_number %in% (rad_att %>% unnest(assigned_exams) %>% pull(ris_accession_number)))
            rm(lkp_mrn)
            # /$
          } else if (rad_att[which(rad_att$dmt.1 == "bone" & rad_att$nj.license == "NJ"), ][1, ]$min_exams > 0) {
            # $
            rad_att[which(rad_att$dmt.1 == "bone" & rad_att$nj.license == "NJ"), ][1, ]$assigned_exams[[1]] %<>% force_rbind(bone_exclusive_exams[1, ])
            todo_ex_wl %<>% filter(ris_accession_number != bone_exclusive_exams[1, ]$ris_accession_number)
            rad_att[which(rad_att$dmt.1 == "bone" & rad_att$nj.license == "NJ"), ][1, ]$min_exams <- rad_att[which(rad_att$dmt.1 == "bone" & rad_att$nj.license == "NJ"), ][1, ]$min_exams - bone_exclusive_exams[1, ]$exam_weighting
            rad_att[which(rad_att$dmt.1 == "bone" & rad_att$nj.license == "NJ"), ][1, ]$sum_assigned <- rad_att[which(rad_att$dmt.1 == "bone" & rad_att$nj.license == "NJ"), ][1, ]$sum_assigned + bone_exclusive_exams[1, ]$exam_weighting
            lkp_mrn <- bone_exclusive_exams[1, ]$ris_mrn
            bone_exclusive_exams %<>% slice(-1)
            pat_move_df(flavor = "dual-only")
            bone_exclusive_exams %<>% filter(ris_mrn != lkp_mrn)
            todo_ex_wl %<>% filter(!ris_accession_number %in% (rad_att %>% unnest(assigned_exams) %>% pull(ris_accession_number)))
            rm(lkp_mrn)
            # /$
          }
        }
      }
      
      if (nrow(bone_exclusive_exams) > 0) {
        log_not_assigned %<>% force_rbind(
          bone_exclusive_exams %>%
            mutate(ps_status = "No/not enough musculoskeletal attendings available")
        )
        todo_ex_wl %<>% filter(!ris_accession_number %in% log_not_assigned$ris_accession_number)
        bone_exclusive_exams <- tibble()
      }
      
      # PEDs: Filter out now, assign later to include young patients not in other exclusives
      peds_exclusive_exams <- todo_ex_wl %>%
        filter(modifier %in% c("PED", "PEDS")) %>%
        mutate(dmt = "peds")
      todo_ex_wl %<>% filter(!ris_accession_number %in% peds_exclusive_exams$ris_accession_number)
      # Continued below after other exclusives...
      
      # Chest and HPB exclusives (by exam_code) --------------------------------------
      
      # step <- "exam_code_exclusives"
      for (x_dmt in ct_exclusives$dmt) {
        exclusive_exams <- todo_ex_wl %>% filter(exam_code %in% ct_exclusives[which(ct_exclusives$dmt == x_dmt), ]$data[[1]]$exam_code)
        if (nrow(exclusive_exams) == 0) next
        
        dmt_ix <- which(rad_att$dmt.1 == x_dmt | rad_att$dmt.2 == x_dmt | rad_att$dmt.3 == x_dmt | rad_att$dmt.4 == x_dmt | rad_att$pref.1 == x_dmt)
        if (nrow(exclusive_exams) > 0 & length(dmt_ix) >= 1 & any(rad_att[dmt_ix, ]$min_exams >= 0)) {
          for (j in 1:nrow(exclusive_exams)) {
            st <- which(rad_att[dmt_ix, ]$nj.license == exclusive_exams[j, ]$state_code | rad_att[dmt_ix, ]$ny.license == exclusive_exams[j, ]$state_code)
            if (length(st) == 0) next
            lucky1 <- which.max(rad_att[dmt_ix, ][st, ]$min_exams)
            if (rad_att[dmt_ix, ][st, ][lucky1, ]$min_exams >= 0) {
              # $
              pat_move_df("ct_excl")
              # /$
            }
          }
        }
        todo_ex_wl %<>% filter(!exam_code %in% ct_exclusives[which(ct_exclusives$dmt == x_dmt), ]$data[[1]]$exam_code)
        if (!all(rad_att %>% pull(assigned_exams) %>% is.na())) {
          # Only runs if anything has been assigned so far at all:
          exclusive_exams %<>% filter(
            !ris_accession_number %in% (rad_att %>% unnest(assigned_exams) %>% pull(ris_accession_number)),
            ris_accession_number != "11111111"
          )
        }
        if (nrow(exclusive_exams) > 0) {
          log_not_assigned %<>% force_rbind(
            exclusive_exams %>% mutate(ps_status = "No suitable specialized attendings available")
          )
          exclusive_exams <- tibble()
        }
      }
      
      # Ref-phys exclusives ------------------------------------------------
      
      # step <- "ref_phys_exclusives"
      if (any(ref_dmts[which(ref_dmts$comment == "exclusive"), ]$referring_md_name %in% todo_ex_wl$referring_md_name)) {
        for (referrer in ref_dmts[which(ref_dmts$comment == "exclusive"), ]$referring_md_name) {
          x_dmt <- ref_dmts %>%
            filter(referring_md_name == referrer) %>%
            pull(dmt.1)
          exclusive_exams <- todo_ex_wl %>% filter(referring_md_name == referrer)
          if (nrow(exclusive_exams) == 0) next
          exclusive_exams$dmt <- x_dmt
          
          dmt_ix <- which(rad_att$dmt.1 == x_dmt | rad_att$dmt.2 == x_dmt | rad_att$dmt.3 == x_dmt | rad_att$dmt.4 == x_dmt)
          if (nrow(exclusive_exams) > 0 & length(dmt_ix) >= 1 & any(rad_att[dmt_ix, ]$min_exams >= 0)) {
            for (j in 1:nrow(exclusive_exams)) {
              st <- which(rad_att[dmt_ix, ]$nj.license == exclusive_exams[j, ]$state_code | rad_att[dmt_ix, ]$ny.license == exclusive_exams[j, ]$state_code)
              if (length(st) == 0) next
              lucky1 <- which.max(rad_att[dmt_ix, ][st, ]$min_exams)
              if (rad_att[dmt_ix, ][st, ][lucky1, ]$min_exams >= 0) {
                # $
                pat_move_df("ct_excl")
                # /$
              }
            }
          }
          todo_ex_wl %<>% filter(!referring_md_name == referrer)
          if (!all(rad_att %>% pull(assigned_exams) %>% is.na())) {
            # Only runs if anything has been assigned so far at all:
            exclusive_exams %<>% filter(
              !ris_accession_number %in% (rad_att %>% unnest(assigned_exams) %>% pull(ris_accession_number)),
              ris_accession_number != "11111111"
            )
          }
          if (nrow(exclusive_exams) > 0) {
            log_not_assigned %<>% force_rbind(
              exclusive_exams %>% mutate(ps_status = "No suitable specialized attendings available")
            )
            exclusive_exams <- tibble()
          }
        }
      }
      
      # PEDs exclusives (continued.) and other completely exclusive DMTs (diep) ---------
      
      # Add more PEDs cases for PEDs DMT assignment below
      # step <- "peds_exclusives"
      nonexclusive_peds <- todo_ex_wl %>%
        filter(age <= 25, dmt != "peds") %>%
        select(dmt, ris_accession_number)
      
      todo_ex_wl %<>% mutate(dmt = if_else(age <= 25, "peds", dmt))
      
      for (x_dmt in c("peds", "diep")) {
        if (x_dmt == "peds" & nrow(peds_exclusive_exams) > 0) {
          exclusive_exams <- bind_rows(peds_exclusive_exams, filter(todo_ex_wl, dmt == x_dmt))
        } else {
          exclusive_exams <- filter(todo_ex_wl, dmt == x_dmt)
        }
        if (nrow(exclusive_exams) == 0) next
        todo_ex_wl %<>% filter(!ris_accession_number %in% exclusive_exams$ris_accession_number)
        
        dmt_ix <- which(rad_att$dmt.1 == x_dmt | rad_att$dmt.2 == x_dmt | rad_att$dmt.3 == x_dmt | rad_att$dmt.4 == x_dmt)
        if (nrow(exclusive_exams) > 0 & length(dmt_ix) >= 1 & any(rad_att[dmt_ix, ]$min_exams >= 0)) {
          for (j in 1:nrow(exclusive_exams)) {
            st <- which(rad_att[dmt_ix, ]$nj.license == exclusive_exams[j, ]$state_code | rad_att[dmt_ix, ]$ny.license == exclusive_exams[j, ]$state_code)
            if (length(st) == 0) next
            lucky1 <- which.max(rad_att[dmt_ix, ][st, ]$min_exams)
            if (rad_att[dmt_ix, ][st, ][lucky1, ]$min_exams >= 0) {
              # $
              pat_move_df("ct_excl")
              # /$
            }
          }
        }
        if (!all(rad_att %>% pull(assigned_exams) %>% is.na())) {
          # Only runs if anything has been assigned so far at all:
          exclusive_exams %<>% filter(
            !ris_accession_number %in% (rad_att %>% unnest(assigned_exams) %>% pull(ris_accession_number)),
            ris_accession_number != "11111111"
          )
        }
        if (nrow(exclusive_exams) > 0) {
          if (nrow(nonexclusive_peds) > 0) {
            if (any(nonexclusive_peds$ris_accession_number %in% exclusive_exams$ris_accession_number)) {
              todo_ex_wl %<>% rbind(
                exclusive_exams %>%
                  filter(ris_accession_number %in% nonexclusive_peds$ris_accession_number) %>%
                  select(-dmt) %>%
                  left_join(nonexclusive_peds, by = "ris_accession_number")
              ) %>%
                arrange(ris_exam_end_dt)
              exclusive_exams %<>% filter(ris_accession_number %not_in% nonexclusive_peds)
            }
          }
          
          log_not_assigned %<>% force_rbind(
            exclusive_exams %>% mutate(ps_status = "No suitable specialized attendings available")
          )
          exclusive_exams <- tibble()
        }
      }
      
      
      # Assign non-exclusive dual-exams -----------------------------------------
      
      if (!all(rad_att %>% pull(assigned_exams) %>% is.na())) {
        candidate_mrns <- rad_att %>%
          unnest(assigned_exams) %>%
          filter(ris_mrn %in% todo_ex_wl$ris_mrn) %>%
          select(ris_mrn, `Provider Number`)
        if (nrow(candidate_mrns) > 0) {
          for (u in seq_len(nrow(candidate_mrns))) {
            j <- which(rad_att$`Provider Number` %in% candidate_mrns[u, ]$`Provider Number`)
            lic <- which(todo_ex_wl$ris_mrn %in% candidate_mrns[u, ]$ris_mrn & todo_ex_wl$state_code %in% rad_att[j, ][c("nj.license", "ny.license")])
            if (length(lic) > 0) {
              rad_att[j, ]$assigned_exams[[1]] <- force_rbind(rad_att[j, ]$assigned_exams[[1]], todo_ex_wl[lic, ])
              rad_att[j, ]$min_exams <- rad_att[j, ]$min_exams - sum(todo_ex_wl[lic, ]$exam_weighting)
              rad_att[j, ]$sum_assigned <- rad_att[j, ]$sum_assigned + sum(todo_ex_wl[lic, ]$exam_weighting)
              todo_ex_wl <- slice(todo_ex_wl, -c(lic))
            }
          }
        }
      }
      
      # Assign to attendings w/ DMT ---------------------------------------
      
      # step <- "dmt"
      # print("Assigning cases to DMT members")
      for (j in 1:nrow(rad_att)) {
        if (bootstrap_samples > 1) {
          if (rad_att[j, ]$min_exams < 0 | any(rad_att[-j, ][which(rad_att[-j, ]$min_exams >= 0), ]$sum_assigned < rad_att[j, ]$sum_assigned, na.rm = TRUE)) next
        } else { # bootstrap_samples == 1
          if (any(rad_att[-j, ]$min_exams > rad_att[j, ]$min_exams, na.rm = TRUE)) next
        }
        if (is.na(rad_att[j, ]$dmt.1)) next
        
        dmt_match <- data.frame()
        for (DMT in rad_att[j, c("dmt.1", "dmt.2", "dmt.3", "dmt.4")]) {
          if (is.na(DMT)) break
          if (nrow(filter(todo_ex_wl, dmt == DMT)) == 0) {
            next
          } else if (nrow(filter(todo_ex_wl, dmt == DMT)) > 0) {
            for (ctrow in 1:nrow(filter(todo_ex_wl, dmt == DMT))) {
              dmt_match <- todo_ex_wl %>%
                filter(dmt == DMT) %>%
                slice(ctrow)
              if (dmt_match$state_code %in% rad_att[j, ][c("nj.license", "ny.license")]) {
                pat_move_df()
                # Check for number exams in subsequent DMTs could go here
                break
              }
            }
          }
          # Allow assignment of subsequent DMTs for same rad under certain cond.
          if (rad_att[j, ]$sum_assigned >= min_assignments | min_assignments < 3 | rad_att[j, ]$min_exams <= 0) break
        }
      }
      
      # Assign to attendings w/ preferences ----------------------------------
      
      # print("Assigning exams according to preferences")
      # step <- "prefs"
      for (j in 1:nrow(rad_att)) {
        if (rad_att[j, ]$min_exams < 0 | any(rad_att[-j, ][which(rad_att[-j, ]$min_exams >= 0), ]$sum_assigned < rad_att[j, ]$sum_assigned, na.rm = TRUE)) next
        if (is.na(rad_att[j, ]$pref.1)) next
        
        dmt_match <- data.frame()
        for (DMT in rad_att[j, c("pref.1", "pref.2", "pref.3")]) {
          if (is.na(DMT)) break
          if (rad_att[j, ]$min_exams < 0 | any(rad_att[-j, ][which(rad_att[-j, ]$min_exams >= 0), ]$sum_assigned < rad_att[j, ]$sum_assigned, na.rm = TRUE)) break
          if (nrow(filter(todo_ex_wl, dmt == DMT)) == 0) {
            next
          } else if (nrow(filter(todo_ex_wl, dmt == DMT)) > 0) {
            for (ctrow in 1:nrow(filter(todo_ex_wl, dmt == DMT))) {
              dmt_match <- todo_ex_wl %>%
                filter(dmt == DMT) %>%
                slice(ctrow)
              if (dmt_match$state_code %in% rad_att[j, ][c("nj.license", "ny.license")]) {
                # $
                pat_move_df()
                # /$
                break
              }
            }
            break # avoids overassigning to ppl w/ multiple prefs
          }
        }
      }
      
      # Assign rest ----------------------------------------------------------
      # NJ first
      
      # step <- "rest_nj"
      njl <- which(rad_att$nj.license == "NJ")
      lic <- which(todo_ex_wl$state_code == "NJ")
      # print("Assigning remainder of exams")
      for (j in njl) {
        if (length(lic) == 0 | all(rad_att[njl, ]$min_exams <= 0)) break
        if (rad_att[j, ]$min_exams <= 0) next
        if (nrow(rad_att[njl, ]) > 1) {
          if (bootstrap_samples > 1 & any(rad_att[njl[-j], ][which(rad_att[njl[-j], ]$min_exams >= 0), ]$sum_assigned < rad_att[j, ]$sum_assigned, na.rm = TRUE)) next
          if (bootstrap_samples == 1 & any(rad_att[njl[-j], ]$min_exams > rad_att[j, ]$min_exams, na.rm = TRUE)) next
        }
        # $
        pat_move_df("random")
        lic <- which(todo_ex_wl$state_code == "NJ")
        # /$
      }
      if (nrow(todo_ex_wl) == 0 | all(rad_att$min_exams < 1)) break
      
      # Then NJ and NY
      # step <- "rest_all"
      for (j in 1:nrow(rad_att)) {
        # print("Assigning remainder of exams")
        if (rad_att[j, ]$min_exams <= 0) next
        if (bootstrap_samples > 1 & any(rad_att[-j, ][which(rad_att[-j, ]$min_exams >= 0), ]$sum_assigned < rad_att[j, ]$sum_assigned, na.rm = TRUE)) next
        if (bootstrap_samples == 1 & any(rad_att[-j, ]$min_exams > rad_att[j, ]$min_exams, na.rm = TRUE)) next
        lic <- which(todo_ex_wl$state_code == "NY" | todo_ex_wl$state_code %in% rad_att[j, ]$nj.license)
        if (length(lic) == 0) next # = no NJ license and only NJ cases
        # $
        pat_move_df("random")
        # /$
      }
      # if(nrow(todo_ex_wl)==0 | all(rad_att$min_exams<=0)) break
      if (all(rad_att[which(rad_att$nj.license == "NJ"), ]$min_exams <= 0, na.rm = TRUE) & all(todo_ex_wl$state_code == "NJ", na.rm = TRUE)) break
      if (old_min_sum == sum(rad_att$min_exams, na.rm = TRUE)) break
      
      if (nrow(ct_late) > 0 & !any(todo_ex_wl$wet_read_request == "wet_read")) {
        rad_att <- bind_rows(rad_att, ct_late)
        ct_late <- ct_late[0, ]
      }
      
      # end while:
    }
    
    if (nrow(todo_ex_wl) > 0) {
      if (all(rad_att$min_exams <= 0.9)) {
        log_not_assigned %<>% bind_rows(
          todo_ex_wl %>% mutate(ps_status = "Maximum number of exams assigned")
        )
      } else {
        log_not_assigned %<>% bind_rows(
          todo_ex_wl %>% mutate(ps_status = "Local minimum reached")
        )
      }
    }
    
    rad_att %<>% filter(!is.na(assigned_exams)) # Can happen with PEDS ppl
    
    ct_att_unnested_new <- rad_att %>%
      tidyr::unnest(assigned_exams)
    if ("service" %in% colnames(ct_att_unnested_new)) {
      ct_att_unnested_new %<>% filter(!is.na(service))
    } else {
      return(
        list(
          "assignments" = ct_att_unnested_new,
          "not_assigned" = bind_rows(log_not_assigned_ps, log_not_assigned),
          "mc_matrix" = bootstrap_iterations
        )
      )
    }
    
    ct_att_unnested_new %<>% mutate(
      dmt_match = 0,
      dmt_match = if_else(is.na(dmt.1), dmt_match, if_else(dmt == dmt.1, dmt_match + 1, dmt_match)),
      dmt_match = if_else(is.na(dmt.2), dmt_match, if_else(dmt == dmt.2, dmt_match + 1, dmt_match)),
      dmt_match = if_else(is.na(dmt.3), dmt_match, if_else(dmt == dmt.3, dmt_match + 1, dmt_match)),
      dmt_match = if_else(is.na(dmt.4), dmt_match, if_else(dmt == dmt.4, dmt_match + 1, dmt_match)),
      dmt_match = if_else(is.na(pref.1), dmt_match, if_else(dmt == pref.1, dmt_match + .5, dmt_match)),
      dmt_match = if_else(is.na(pref.2), dmt_match, if_else(dmt == pref.2, dmt_match + .5, dmt_match)),
      dmt_match = if_else(is.na(pref.3), dmt_match, if_else(dmt == pref.3, dmt_match + .5, dmt_match))
    )
    
    if (!exists("ct_att_unnested")) ct_att_unnested <- ct_att_unnested_new
    if (sum(ct_att_unnested_new$dmt_match) > loss_var | bootstrap_samples == 1) {
      loss_var <- sum(ct_att_unnested_new$dmt_match)
      bootstrap_iterations <- rbind(bootstrap_iterations, c(bootstrap_loop, loss_var))
      ct_att_unnested <- ct_att_unnested_new
    }
    ### End of bootstrap loop:
  }
  
  # Failsafe for NA min_exams
  if (any(is.na(ct_att_unnested$min_exams))) {
    # Recover old min
    missing_ix <- which(is.na(ct_att_unnested$min_exams))
    missing_ix_usr <- ct_att_unnested[missing_ix, ]$Name
    old_mins <- ct_att_blank$min_exams
    names(old_mins) <- ct_att_blank$Name
    ix_old_min <- old_mins[missing_ix_usr]
    ct_att_unnested[missing_ix, ]$min_exams <- ix_old_min
    # Recover sum_assigned
    recovered_sums <- ct_att_unnested %>%
      filter(Name %in% missing_ix_usr) %>%
      group_by(Name) %>%
      summarise(sums = sum(exam_weighting, na.rm = TRUE))
    rec_sums <- recovered_sums$sums
    names(rec_sums) <- recovered_sums$Name
    ct_att_unnested[missing_ix, ]$sum_assigned <- rec_sums[missing_ix_usr]
    # Compute new min
    ct_att_unnested[missing_ix, ] <- ct_att_unnested[missing_ix, ] %>%
      mutate(min_exams = min_exams - sum_assigned)
  }
  
  log_not_assigned_ct <- bind_rows(log_not_assigned_ps, log_not_assigned)
  return(list("assignments" = ct_att_unnested, "not_assigned" = log_not_assigned_ct, "mc_matrix" = bootstrap_iterations))
}
