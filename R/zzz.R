setLoadAction(function(ns) {
  # Check connection
  if (!is.null(curl::nslookup("smartsheets.com", error = FALSE))) {
    # Update faculty DMT list
    if (Sys.getenv("smartsheet_api_key") != "") {
      if (Sys.getenv("smartsheet_dmt") != "") {
        sget <- purrr::safely(get_smartsheet)
        spivot <- purrr::safely(pivot_dmts)
        new_fac <- sget(Sys.getenv("smartsheet_dmt"), Sys.getenv("smartsheet_api_key"))
        if (is.null(new_fac$error)) {
          if (smartsheet_df_check(new_fac$result)) {
            new_fell <- sget(Sys.getenv("smartsheet_trainees"), Sys.getenv("smartsheet_api_key"))
            new_lut <- spivot(new_fac$result, new_fell$result)
            if (is.null(new_lut$error)) {
              assign("fac_dmts", new_lut$result, envir = ns)
              assign("fac_dmts_ct", dplyr::select(dplyr::filter(new_lut$result, mod == "CT"), -dmt.0), envir = ns)
              assign("fac_dmts_mr", dplyr::filter(new_lut$result, mod == "MR"), envir = ns)
              message("Radiologist lookup tables successfully updated.")
            } else {
              warning(sprintf("Smartsheet DMT list inconsistent. Falling back to built-in tables. Error was: %s", new_lut$error$message))
            }
          } else {
            warning("Smartsheet DMT list inconsistent. Falling back to built-in tables.")
          }
        } else {
          warning(sprintf("Update from smartsheet failed: %s", as.character(new_dmts$error)))
        }
      } else {
        message("No smartsheet IDs provided. Using built-in tables.")
      }
    } else {
      message("No smartsheet API key. Using built-in tables.")
    }
  } else {
    message("No internet connection. Skipping radiologist lookup table update.")
  }
})
