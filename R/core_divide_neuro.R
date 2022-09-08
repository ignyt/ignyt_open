#' divide_neuro core function
#'
#' Creates dataframe with Neuro CT and MR's assigned to attendings.
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
#' @param ctmr,atts Data frames (tibbles) as produced by "prep" functions.
#' @param bootstrap_samples Number of bootstrapped optimization samples. Defaults to 50.
#' @param bypass_ps Bypasses check of PS360 database (kicks out dictated/queued exams). Defaults to FALSE.
#'
#' @import dplyr stringr
#'
#' @examples
#' worklist <- prep_worklist(sim_worklist(1))
#' mr_att <- sim_shift_rads(1, "MR", "am")
#' \dontrun{
#' divide_mr(worklist, mr_att, 5)
#' }
#'
#' @export

divide_neuro <- function(ctmr, atts, bootstrap_samples = 1, bypass_ps = FALSE) {
  if (!is.logical(bypass_ps)) {
    warning("Invalid bypass_ps value, falling back to FALSE")
    bypass_ps <- FALSE
  }

  if (nrow(ctmr) == 0 | nrow(atts) == 0) {
    warning("Empty data provided")
    return(list(ctmr[0, ], ctmr, matrix()))
  }

  mr_att_blank <- mutate(atts, assigned_exams = NA)

  mr_att_blank$assigned_exams %<>% as.list()

  todo_mr <- ctmr %>%
    # filter(modality_code == "MR") %>%
    arrange(desc(wet_read_request), ris_exam_end_dt) %>%
    mutate(
      modifier = tidyr::replace_na(modifier, "none"),
      dmt = ifelse(modifier == "PED", "peds", dmt),
      dmt = ifelse(age < 21 & dmt == "general", "peds", dmt)
    )

  # Integrity checks --------------------------------------------------------
  if (bypass_ps == FALSE) {
    todo_mr_ps <- left_join(
      todo_mr,
      ps_get_status(todo_mr$ris_accession_number),
      by = "ris_accession_number"
    )
    log_not_assigned_ps <- todo_mr_ps %>% filter(ps_status != "Completed")
  } else if (bypass_ps == TRUE) {
    todo_mr_ps <- todo_mr %>% mutate(ps_status = "Completed")
    log_not_assigned_ps <- todo_mr_ps[0, ]
  }

  todo_mr_wl_blank <- todo_mr_ps %>% filter(ps_status == "Completed")

  if (nrow(todo_mr_wl_blank) == 0) {
    return(list(todo_mr_wl_blank, log_not_assigned_ps, matrix()))
  }

  # Bootstrap "for" loop ----------------------------------------------------

  loss_var <- 0
  bootstrap_loop <- 0
  bootstrap_iterations <- matrix(c(0, 0), ncol = 2)

  for (m in 1:bootstrap_samples) {
    bootstrap_loop <- bootstrap_loop + 1

    if (bootstrap_samples > 1) {
      mr_att <- mr_att_blank[sample(nrow(mr_att_blank)), ] # Random shuffle for fairness and optim.
    } else {
      # For cleanup/crossover:
      mr_att <- mr_att_blank %>% arrange(desc(min_exams))
    }
    todo_mr_wl <- todo_mr_wl_blank

    while (any(mr_att$min_exams > 0)) {
      dmt_nrow <- 0
      # mr_att %<>% arrange(desc(min_exams))

      # Assign according to DMT -----------------------------------------

      for (j in 1:nrow(mr_att)) {
        if (mr_att[j, ]$min_exams <= 0) next

        for (DMT in mr_att[j, c("dmt.1", "dmt.2", "dmt.3", "dmt.4", "dmt.0")]) {
          if (is.na(DMT)) next
          if (mr_att[j, ]$min_exams <= 0) next
          if (nrow(filter(todo_mr_wl, dmt == DMT)) == 0) {
            next
          } else if (any(sapply(todo_mr_wl[which(todo_mr_wl$dmt == DMT), ]$state_code, function(x) x %in% mr_att[j, ][, c("nj.license", "ny.license")]))) {
            if (mr_att[j, ]$nj.license == "NJ") {
              dmt_match <- todo_mr_wl[which(todo_mr_wl$dmt == DMT), ]
            } else {
              dmt_match <- todo_mr_wl[which(todo_mr_wl$dmt == DMT & todo_mr_wl$state_code == "NY"), ]
            }
            if (nrow(dmt_match) == 0) next
            dmt_match %<>% slice(1)
            dmt_nrow <- sum(dmt_nrow, nrow(dmt_match), na.rm = TRUE)
            # $
            mr_att[j, ]$assigned_exams[[1]] %<>% force_rbind(dmt_match)
            mr_att[j, ]$min_exams <- mr_att[j, ]$min_exams - dmt_match$exam_weighting
            mr_att[j, ]$sum_assigned <- mr_att[j, ]$sum_assigned + dmt_match$exam_weighting
            todo_mr_wl %<>% filter(ris_accession_number != dmt_match$ris_accession_number)
            # /$
            # break
          }
        }
      }
      if (dmt_nrow == 0) break # whole round w/o assignment
    }

    mr_att %<>% filter(!is.na(assigned_exams))

    mr_att_unnested_new <- mr_att %>%
      tidyr::unnest(assigned_exams) %>%
      filter(!is.na(Service))
    mr_att_unnested_new %<>% mutate(
      dmt_match = 0,
      dmt_match = ifelse(is.na(dmt.0), dmt_match, ifelse(dmt == dmt.0, dmt_match + 1, dmt_match)),
      dmt_match = ifelse(is.na(dmt.1), dmt_match, ifelse(dmt == dmt.1, dmt_match + 1, dmt_match)),
      dmt_match = ifelse(is.na(dmt.2), dmt_match, ifelse(dmt == dmt.2, dmt_match + 1, dmt_match)),
      dmt_match = ifelse(is.na(dmt.3), dmt_match, ifelse(dmt == dmt.3, dmt_match + 1, dmt_match)),
      dmt_match = ifelse(is.na(dmt.4), dmt_match, ifelse(dmt == dmt.4, dmt_match + 1, dmt_match))
    )

    if (sum(mr_att_unnested_new$dmt_match) >= loss_var) {
      loss_var <- sum(mr_att_unnested_new$dmt_match)
      bootstrap_iterations <- rbind(bootstrap_iterations, c(bootstrap_loop, loss_var))
      mr_att_unnested <- mr_att_unnested_new
      leftover_mrs <- todo_mr_wl
    }
    ### End of bootstrap loop:
  }

  log_not_assigned_mr <- bind_rows(log_not_assigned_ps, leftover_mrs)

  return(list("assignments" = mr_att_unnested, "not_assigned" = log_not_assigned_mr, "mc_matrix" = bootstrap_iterations))
}
