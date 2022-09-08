#' dictation system API authentication
#'
#' Initializes dictation system API session. Returns HTTP response code.
#' Will try to get ps_UX and ps_PW via Sys.getenv if they are left empty.
#'
#' @param ps_UX,ps_PW Dication System API username and password
#' @param test If TRUE uses PS test API. Default is FALSE.
#'
#' @importFrom httr POST
#'
#' @examples
#' \dontrun{
#' api_response <- ps_auth() # if set previously via Sys.setenv
#'
#' api_response <- ps_auth(Sys.getenv("myPSusr"), Sys.getenv("myPSpwd"))
#'
#' # Careful! Clear text password in code is generally discouraged
#' api_response <- ps_auth("username", "password", test = TRUE)
#' }
#'
#' @export
#'
ps_auth <- function(ps_UX, ps_PW, test = FALSE) {
  if (missing(ps_UX) & missing(ps_PW)) {
    ps_UX <- Sys.getenv("ps_UX")
    ps_PW <- Sys.getenv("ps_PW")
  }

  if (test == FALSE) {
    api_path <- "http://dict_sys_url/"
  } else if (test == TRUE) {
    api_path <- "http://tdict_sys_url/"
  } else {
    stop("Parameter 'test' must logical/TRUE or FALSE")
  }

  auth_req <- POST(
    url = api_path,
    path = "RadPortal/services/2008/auth.asmx/SignIn",
    body = list(
      systemID = 0,
      accessCode = 0,
      username = ps_UX,
      password = ps_PW
    ),
    encode = "form"
  )
}

#' Close dictation system API Session
#'
#' Closes dictation system API session. Returns HTTP response code.
#'
#'
#' @importFrom httr POST
#' @param test If TRUE uses PS test API. Default is FALSE.
#'
#' @examples
#' \dontrun{
#' api_response <- ps_close() # if set previously via Sys.setenv
#'
#' api_response <- ps_close()
#' }
#'
#' @export
#'
ps_close <- function(test = FALSE) {
  if (test == FALSE) {
    api_path <- "http://dict_sys_url/"
  } else if (test == TRUE) {
    api_path <- "http://tdict_sys_url/"
  } else {
    stop("Parameter 'test' must logical/TRUE or FALSE")
  }

  auth_req <- POST(
    url = api_path,
    path = "RadPortal/services/2008/auth.asmx/SignOut",
    body = list(
      systemID = 0,
      accessCode = 0
    ),
    encode = "form"
  )
}

#' dictation system get status of examination
#'
#' Gets status of accession number in dictation system.
#' Originally used as a fallback if the RIS database lags behind
#' but can be used for other purposes.
#' Needs an active API session initialized with ps_auth().
#' Returns a tibble/dataframe.
#'
#' @param acc_nos Accession numbers of examinations.
#' Can be either a single string separated by commas
#' or a vector with multiple numbers.
#' @param test If TRUE uses PS test API. Default is FALSE.
#'
#' @importFrom dplyr %>% slice mutate
#' @importFrom xml2 xml_children xml_child xml_text
#' @importFrom tibble tibble
#' @importFrom lubridate as_date as_datetime
#' @importFrom httr content status_code POST
#'
#' @export

ps_get_status <- function(acc_nos, test = FALSE) {
  if (test == FALSE) {
    api_path <- "http://dict_sys_url/"
  } else if (test == TRUE) {
    api_path <- "http://tdict_sys_url/"
  } else {
    stop("Parameter 'test' must logical/TRUE or FALSE")
  }

  # In case acc_nos is not a single string separate them by commas:
  if (length(acc_nos) > 1) {
    acc_nos <- paste0(acc_nos, collapse = ",")
  }

  # Internal dirty helper function to convert xml -> df
  populate_df_with_xml <- function(xml_df, xmlobj) {
    if ((xmlobj %>% xml_children() %>% length()) >= 1) {
      for (i in 1:(xmlobj %>% xml_children() %>% length())) {
        for (j in 1:(xmlobj %>% xml_child(i) %>% xml_children() %>% length())) {
          xml_df[i, j] <- xmlobj %>%
            xml_child(i) %>%
            xml_child(j) %>%
            xml_text() %>%
            as.character()
        }
      }
    }
    return(xml_df)
  }

  search_acc <- httr::POST(
    url = api_path,
    path = "RadPortal/services/2008/explorer.asmx/SearchByAccession",
    body = list(
      site = "main",
      accessions = acc_nos,
      sort = ""
    ),
    encode = "form"
  )
  if (httr::status_code(search_acc) != 200) stop("Dictation System API search connection unsuccessful")

  xmlobj <- search_acc %>%
    httr::content()

  xml_df <- tibble::tibble(
    orderid = "string",
    reportid = "string",
    ris_accession_number = "string",
    orderdate = "string",
    procedures = "string",
    ps_status = "string",
    patient = "string",
    reader = "string"
  ) %>%
    slice(-1)

  xml_df <- populate_df_with_xml(xml_df, xmlobj) %>%
    mutate(
      reportid = as.numeric(reportid),
      oderdate = as_datetime(orderdate),
      ris_accession_number = as.character(ris_accession_number)
    )

  return(xml_df)
}


#' Assigns given accession number to attending
#'
#' Returns string "OK" if successful and HTTP response content if not.
#'
#' @param acc_no Single accession number
#' @param attn Attending "Provider Number". Accepts 0 or NULL for unassignment.
#' @param fellow_res Fellow or Resident "Provider Number" (not implemented yet)
#' @param test Whether to use dictation system test or prod API, default is FALSE (prod)
#'
#' @importFrom dplyr %>%
#' @importFrom xml2 xml_child xml_text
#' @import httr
#'
#' @export

assign_attending_accno <- function(acc_no, attn, fellow_res = "", test = FALSE) {
  if (test == FALSE) {
    api_path <- "http://dict_sys_url/"
  } else if (test == TRUE) {
    api_path <- "http://tdict_sys_url/"
  } else {
    stop("Parameter 'test' must logical/TRUE or FALSE")
  }

  # Unassignment workaround for NULL bug in AssignOrderByAccession:
  if (attn == 0 | attn == "0" | is.null(attn) | is.na(attn)) {
    order_req <- httr::POST(
      url = api_path,
      path = "RadPortal/services/2008/explorer.asmx/SearchByAccession",
      body = list(
        site = "main",
        accessions = acc_no,
        sort = 1
      ),
      encode = "form"
    )

    if (status_code(order_req) == 200) {
      if (length(order_req %>% httr::content() %>% xml_children()) > 0) {
        orderid <- order_req %>%
          httr::content() %>%
          xml2::xml_child() %>%
          xml2::xml_child() %>%
          xml2::xml_text()
      } else {
        return("Empty order")
      }
    } else {
      return(httr::content(order_req))
    }
    assign_report <- httr::POST(
      url = api_path,
      path = "RadPortal/services/2008/order.asmx/AssignOrder",
      body = list(
        orderID = orderid,
        accountID = 0
      ),
      encode = "form"
    )
  } else {
    # Actual assignment
    assign_report <- httr::POST(
      url = api_path,
      path = "RadPortal/services/2008/order.asmx/AssignOrderByAccession",
      body = list(
        site = "main",
        accession = acc_no,
        accountIdentifier = attn
      ),
      encode = "form"
    )
  }

  if (httr::status_code(assign_report) == 200) {
    return("OK")
  } else {
    return(httr::content(assign_report))
  }
}

#' assign core function
#'
#' Essentially just a small wrapper for assign_attending_accno()
#' Returns a dataframe, last column contains "OK" for successful
#' assignments and the error message for failures.
#'
#' @param df (tibbles) as produced by "prep" functions.
#' @param ps_test Whether to use the test or prod API. Defaults to FALSE (prod)
#'
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' worklist <- prep_worklist(sim_worklist(1))
#' ct_att <- sim_shift_rads(1, "CT", "am")
#' cts <- divide_ct(worklist, ct_att, 2, bypass_ps = TRUE)
#'
#' ct_att_unnested <- assign_reports(cts[[1]], ps_test = TRUE)
#' }
#'
#' @export

assign_reports <- function(df, ps_test = FALSE) {
  df %<>% mutate(assigned_status = "not yet")

  for (r in 1:nrow(df)) {
    df[r, ]$assigned_status <- assign_attending_accno(
      acc_no = df[r, ]$ris_accession_number,
      attn = df[r, ]$`Provider Number`,
      test = ps_test
    )
    print("Dictation API call complete. Waiting 0.5 seconds.")
    Sys.sleep(0.5)
  }
  return(df)
}
