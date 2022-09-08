#' divide_mr core function
#'
#' Creates dataframe with MR's assigned to attendings.
#' Expects dataframes with specific columns as returned
#' by the "prep" functions.
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
#' If only 1 bootstrap sample is selected the assignment
#' will not be done randomly but in the sequence of min_exams.
#' I.e. attendings with more open points will preferentially be
#' assigned exams (for cleanup round/crossover reads).
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
#' mr_att <- sim_shift_rads(1, "MR", "am")
#' divide_mr(worklist, mr_att, 5, FALSE, TRUE)
#' @export

divide_mr <- function(wl, atts, bootstrap_samples = 20, late_dry = FALSE, bypass_ps = FALSE) {
  environment(pat_move_df) <- environment()

  if (!is.logical(bypass_ps)) {
    warning("Invalid bypass_ps value, falling back to FALSE")
    bypass_ps <- FALSE
  }

  if (nrow(wl) == 0 | nrow(atts) == 0) {
    warning("Empty data provided")
    return(list("assignments" = wl[0, ], "not_assigned" = wl, "mc_matrix" = matrix()))
  }

  mr_att_blank <- mutate(atts, assigned_exams = NA)

  mr_att_blank$assigned_exams %<>% as.list()

  todo_mr <- wl %>%
    # filter(modality_code == "MR") %>% # Filter later for dual exam assgn.
    arrange(desc(wet_read_request), ris_exam_end_dt) %>%
    mutate(
      modifier = tidyr::replace_na(modifier, "none"),
      dmt = if_else(modifier == "PED", "peds", dmt),
      dmt = if_else(age < 21 & dmt == "general", "peds", dmt)
    )

  # Integrity checks --------------------------------------------------------
  if (bypass_ps == FALSE) {
    todo_wl_ps <- left_join(
      todo_mr,
      ps_get_status(todo_mr$ris_accession_number),
      by = "ris_accession_number"
    )
    log_not_assigned_ps <- todo_wl_ps %>% filter(ps_status != "Completed")
  } else if (bypass_ps == TRUE) {
    todo_wl_ps <- todo_mr %>% mutate(ps_status = "Completed")
    log_not_assigned_ps <- todo_wl_ps[0, ]
  }

  todo_mr_wl_blank <- todo_wl_ps %>%
    filter(modality_code == "MR") %>% # Filter MR here after todo_wl_ps
    filter(ps_status == "Completed")

  if (nrow(todo_mr_wl_blank) == 0) {
    return(list("assignments" = todo_mr_wl_blank, "not_assigned" = log_not_assigned_ps, "mc_matrix" = matrix()))
  }

  # Bootstrap "for" loop ----------------------------------------------------

  loss_var <- 0
  bootstrap_loop <- 0
  bootstrap_iterations <- matrix(c(0, 0), ncol = 2)

  for (m in 1:bootstrap_samples) {
    bootstrap_loop <- bootstrap_loop + 1

    if (bootstrap_samples > 1) {
      rad_att <- mr_att_blank[sample(nrow(mr_att_blank)), ] # Random shuffle for fairness and optim.
    } else { # For cleanup:
      rad_att <- mr_att_blank %>% arrange(desc(min_exams))
    }
    todo_ex_wl <- todo_mr_wl_blank
    mr_late <- rad_att[0, ]

    while (any(rad_att$min_exams > 0)) {
      dmt_nrow <- 0

      # Exclude late people from receiving wet reads (only main AM round) ----

      if (late_dry == TRUE & nrow(rad_att[which(rad_att$swim == FALSE), ]) > 0 & any(todo_ex_wl$wet_read_request %in% c("wet_read", "stat"))) {
        mr_late <- rad_att %>% filter(swim == FALSE)
        rad_att %<>% filter(swim == TRUE)
      }

      # Assign according to DMT -----------------------------------------

      for (j in 1:nrow(rad_att)) {
        if (rad_att[j, ]$min_exams <= 0) next

        for (DMT in rad_att[j, c("dmt.1", "dmt.2", "dmt.3", "dmt.4", "dmt.0")]) {
          if (is.na(DMT)) next
          if (rad_att[j, ]$min_exams <= 0) break
          if (nrow(filter(todo_ex_wl, dmt == DMT)) == 0) {
            next
          } else if (any(sapply(todo_ex_wl[which(todo_ex_wl$dmt == DMT), ]$state_code, function(x) x %in% rad_att[j, ][, c("nj.license", "ny.license")]))) {
            if (rad_att[j, ]$nj.license == "NJ") {
              dmt_match <- todo_ex_wl[which(todo_ex_wl$dmt == DMT), ]
            } else {
              dmt_match <- todo_ex_wl[which(todo_ex_wl$dmt == DMT & todo_ex_wl$state_code == "NY"), ]
            }
            if (nrow(dmt_match) == 0) next
            dmt_match %<>% slice(1)
            dmt_nrow <- sum(dmt_nrow, nrow(dmt_match), na.rm = TRUE)
            # $
            pat_move_df()
            # /$
            break
          }
        }
      }
      if (dmt_nrow == 0) break # whole round w/o assignment
      if (nrow(mr_late) > 0 & all(todo_ex_wl$wet_read_request %not_in% c("wet_read", "stat"))) {
        rad_att <- bind_rows(rad_att, mr_late)
        mr_late <- mr_late[0, ]
      }
    }

    rad_att %<>% filter(!is.na(assigned_exams)) # Can happen with PEDs

    mr_att_unnested_new <- rad_att %>%
      tidyr::unnest(assigned_exams) %>%
      filter(!is.na(Service))
    if (nrow(mr_att_unnested_new) > 0) {
      mr_att_unnested_new %<>% mutate(
        dmt_match = 0,
        dmt_match = if_else(is.na(dmt.0), dmt_match, if_else(dmt == dmt.0, dmt_match + 1, dmt_match)),
        dmt_match = if_else(is.na(dmt.1), dmt_match, if_else(dmt == dmt.1, dmt_match + 1, dmt_match)),
        dmt_match = if_else(is.na(dmt.2), dmt_match, if_else(dmt == dmt.2, dmt_match + 1, dmt_match)),
        dmt_match = if_else(is.na(dmt.3), dmt_match, if_else(dmt == dmt.3, dmt_match + 1, dmt_match)),
        dmt_match = if_else(is.na(dmt.4), dmt_match, if_else(dmt == dmt.4, dmt_match + 1, dmt_match))
      )

      if (sum(mr_att_unnested_new$dmt_match) >= loss_var) {
        loss_var <- sum(mr_att_unnested_new$dmt_match)
        bootstrap_iterations <- rbind(bootstrap_iterations, c(bootstrap_loop, loss_var))
        mr_att_unnested <- mr_att_unnested_new
        leftover_mrs <- todo_ex_wl
      }
    } else {
      if (!exists("bootstrap_iterations")) bootstrap_iterations <- matrix()
      if (!exists("mr_att_unnested")) mr_att_unnested <- mr_att_unnested_new
      if (!exists("leftover_mrs")) leftover_mrs <- todo_ex_wl
    }
    ### End of bootstrap loop:
  }

  log_not_assigned_mr <- bind_rows(log_not_assigned_ps, leftover_mrs)

  return(list("assignments" = mr_att_unnested, "not_assigned" = log_not_assigned_mr, "mc_matrix" = bootstrap_iterations))
}
