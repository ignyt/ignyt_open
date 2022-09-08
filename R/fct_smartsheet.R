
#' Get smartsheet
#'
#' Uses the smartsheet.com API to fetch a given spreadsheet.
#' Returns a tibble (uses read_csv internally).
#'
#' @param sheet_id ID of spreadsheet
#' @param api_key Alphanumeric key to connect to smartsheet API
#'
#' @importFrom httr GET content
#' @importFrom dplyr %>% as_tibble
#' @importFrom readr read_csv
#'
#' @return Tibble with smartsheet-spreadsheet content
#' @export
#'
#' @keywords internal

get_smartsheet <- function(sheet_id, api_key) {
  api_key <- paste0("Bearer ", api_key)
  content_type <- "text/csv"

  url_path <- paste0("https://api.smartsheet.com/2.0/sheets/", sheet_id)
  request <- GET(
    url = url_path,
    add_headers(
      Authorization = api_key,
      Accept = content_type
    )
  )
  if (request$status_code != 200) {
    stop(paste(request$status_code, "Smartsheet API Call Failed:", request$request$url))
  }
  else {
    content(request, as = "text", encoding = "UTF-8") %>%
      read_csv() %>%
      as_tibble()
  }
}

#' Pivot DMT tables
#' 
#' Brings DMT tables from wide to long format.
#' Adds column "mod" with modality code.
#' Will currently discard all columns that are not
#' in the built-in tables.
#'
#' @param faculty Dataframe with faculty members and their CT/MR DMTs
#' @param fellows Optional. Table with fellows.
#' @param .keep_cols Defaults to FALSE, discards columns not present in internal table.
#'
#' @keywords Internal
pivot_dmts <- function(faculty, fellows=NULL, .keep_cols=FALSE){
  dmt_tbl <- faculty %>% 
    {if(!is.null(fellows)) bind_rows(., fellows) else .} %>%
    pivot_longer(
      cols = ct_dmt.1:mr_dmt.4,
      names_sep = "_",
      names_to = c("mod", "param")
    ) %>%
    pivot_wider(names_from = param) %>%
    mutate(mod = toupper(mod)) %>%
    select(mod, everything()) %>%
    select(mod:nj.license, dmt.0, everything())
  if(.keep_cols) {
    keep_cols <- colnames(dmt_tbl)
  } else {
    keep_cols <- colnames(dmt_tbl)[colnames(dmt_tbl) %in% colnames(fac_dmts)]
  }
  dmt_tbl <- dmt_tbl[, keep_cols]
}

#' Test DMT lists from Smartsheet download
#' 
#' Performs various checks to avoid later assignment
#' errors after updating the DMT list from Smartsheets.
#' Returns TRUE if all checks are passed successfully.
#'
#' @param fac Dataframe with faculty members
#' @param fell Dataframe with fellows. Optional.
#' 
#' @importFrom purrr safely
#' @importFrom dplyr %>%
#'
#' @export
#'
#' @keywords Internal
smartsheet_df_check <- function(fac, fell=NULL) {
  if(is.null(fac)) return(FALSE)
  if(!is.data.frame(fac)) return(FALSE)
  spivot <- safely(pivot_dmts)
  df <- spivot(fac, fell)$result
  if(is.null(df)) return(FALSE)
  if(!all(colnames(fac_dmts) %in% colnames(df))) return(FALSE)
  if(!all(nchar(fac$`Provider Number`)==6)) return(FALSE)
  return(TRUE)
}

