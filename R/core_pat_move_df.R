#' Move patient row from worklist to radiologist
#'
#' Used in *divide* functions. Need to set
#' environment(pat_move_df) <- environment() in parent
#' function.
#' Will look for duplicated MRNs and assign to same
#' radiologist if he/she has the according DMT (or "general").
#'
#' @section Variables in parent environment
#' Will modify the data frames "rad_att", "todo_ex_wl" and use
#' "todo_wl_ps" for dual exams. Make sure they exist in the
#' calling function, otherwise  \code{\link{<<-}}
#' will create it in .GlobalEnv (and probably cause errors).
#' Uses the variables "lkp_mrn", "q", "r" and "s" internally,
#' if they are present in the parent environment this can lead to
#' unintended behavior.
#'
#' @param flavor Determines which variant is used. Available
#' values are "basic" (default), "ct_excl" and "random". If
#' any other parameter is given the variables "lkp_mrn" needs to
#' have been set and it will
#'
#' @noRd
#'
#' @keywords Internal
pat_move_df <- function(flavor = "basic") {
  if (flavor == "basic") {
    rad_att[j, ]$assigned_exams[[1]] <<- force_rbind(rad_att[j, ]$assigned_exams[[1]], dmt_match)
    rad_att[j, ]$min_exams <<- rad_att[j, ]$min_exams - sum(dmt_match$exam_weighting)
    rad_att[j, ]$sum_assigned <<- rad_att[j, ]$sum_assigned + sum(dmt_match$exam_weighting)
    todo_ex_wl <<- filter(todo_ex_wl, ris_accession_number %not_in% dmt_match$ris_accession_number)
    lkp_mrn <- dmt_match$ris_mrn
  } else if (flavor == "ct_excl") {
    if (exclusive_exams[j, ]$ris_accession_number == "11111111") {
      return(NULL) # skipping in-loop modified exams (see below)
    }
    rad_att[dmt_ix, ][st, ][lucky1, ]$assigned_exams[[1]] <<- force_rbind(rad_att[dmt_ix, ][st, ][lucky1, ]$assigned_exams[[1]], exclusive_exams[j, ])
    rad_att[dmt_ix, ][st, ][lucky1, ]$sum_assigned <<- rad_att[dmt_ix, ][st, ][lucky1, ]$sum_assigned + exclusive_exams[j, ]$exam_weighting
    rad_att[dmt_ix, ][st, ][lucky1, ]$min_exams <<- rad_att[dmt_ix, ][st, ][lucky1, ]$min_exams - exclusive_exams[j, ]$exam_weighting
    # todo_ex_wl is handled beforehand outside of pat_move_df
    lkp_mrn <- exclusive_exams[j, ]$ris_mrn
  } else if (flavor == "random") {
    rad_att[j, ]$assigned_exams[[1]] <<- force_rbind(rad_att[j, ]$assigned_exams[[1]], todo_ex_wl[lic, ][1, ])
    rad_att[j, ]$min_exams <<- rad_att[j, ]$min_exams - todo_ex_wl[lic, ][1, ]$exam_weighting
    rad_att[j, ]$sum_assigned <<- rad_att[j, ]$sum_assigned + todo_ex_wl[lic, ][1, ]$exam_weighting
    lkp_mrn <- todo_ex_wl[lic, ][1, ]$ris_mrn
    todo_ex_wl <<- slice(todo_ex_wl, -lic[1])
  } else if (flavor != "dual-only") {
    warning("Using unknown parameter to assign dual exams!")
  }
  # Dual exams:
  if (flavor != "random") {
    q <- which(todo_wl_ps$ris_mrn == lkp_mrn & todo_wl_ps$ris_accession_number %not_in% (rad_att %>% unnest(assigned_exams) %>% pull(ris_accession_number)))
  } else {
    q <- which(todo_ex_wl[lic, ]$ris_mrn == lkp_mrn & todo_ex_wl[lic, ]$ris_accession_number %not_in% (rad_att %>% unnest(assigned_exams) %>% pull(ris_accession_number)))
  }
  if (length(q) > 0) {
    for (r in q) {
      s <- which(sapply(rad_att$assigned_exams, function(x) if (!all(is.na(x))) any(x$ris_mrn == lkp_mrn) else FALSE))
      if (length(s) > 1) s <- s[1]
      
      # Special case  e.g. for peds DMT assigned post-hoc for younger patients:
      if (flavor == "ct_excl") {
        posthoc_dmt <- exclusive_exams[which(exclusive_exams$ris_accession_number == todo_wl_ps[r, ]$ris_accession_number), ]$dmt
        if (length(posthoc_dmt) == 0) posthoc_dmt <- "none"
      } else {
        posthoc_dmt <- "none"
      }
      if (posthoc_dmt %not_in% c("none", todo_wl_ps[r, ]$dmt)) todo_wl_ps[r, ]$dmt <<- posthoc_dmt
      # End special case
      
      if (flavor != "random" & (todo_wl_ps[r, ]$dmt == "general" | todo_wl_ps[r, ]$dmt %in% rad_att[s, c("dmt.1", "dmt.2", "dmt.3", "dmt.4", "pref.1", "pref.2", "pref.3")])) {
        rad_att[s, ]$assigned_exams[[1]] <<- force_rbind(rad_att[s, ]$assigned_exams[[1]], todo_wl_ps[r, ])
        rad_att[s, ]$min_exams <<- rad_att[s, ]$min_exams - todo_wl_ps[r, ]$exam_weighting
        rad_att[s, ]$sum_assigned <<- rad_att[s, ]$sum_assigned + todo_wl_ps[r, ]$exam_weighting
        # Random assignment used in CT can ignore DMT but has to respect
        # previously excluded exclusive exams = use todo_ex_wl instead of todo_wl_ps:
      } else if (flavor == "random") {
        rad_att[s, ]$assigned_exams[[1]] <<- force_rbind(rad_att[s, ]$assigned_exams[[1]], todo_ex_wl[lic, ][r, ])
        rad_att[s, ]$min_exams <<- rad_att[s, ]$min_exams - todo_ex_wl[lic, ][r, ]$exam_weighting
        rad_att[s, ]$sum_assigned <<- rad_att[s, ]$sum_assigned + todo_ex_wl[lic, ][r, ]$exam_weighting
      }
    }
    todo_ex_wl <<- filter(todo_ex_wl, ris_accession_number %not_in% rad_att[s, ]$assigned_exams[[1]]$ris_accession_number)
    if (flavor == "ct_excl") {
      # Can't modify df in place due to running loop, modify accession number instead and skip in later pass:
      exclusive_exams[which(exclusive_exams$ris_accession_number %in% rad_att[s, ]$assigned_exams[[1]]$ris_accession_number), ]$ris_accession_number <<- "11111111"
    }
  }
}
