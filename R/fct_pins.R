
#' Check if pin name exists
#'
#' @param name Name of pin
#' @param board Defaults to "rsconnect" (the default board for ignyt)
#'
#' @import pins
#'
#' @export

pin_exists <- function(name, board = "rsconnect") {
  output <- ifelse(nrow(pin_find(name = name, board = board)) == 0, FALSE, TRUE)
}

#' Balance Sheet pin -> Kable
#'
#' Gets balance sheet from pin and outputs to nice knitr::kable
#' table via ignyt_kable()
#'
#' @param test_pin Logical. Whether to write to production or test pin. Defaults to FALSE = prod.
#' @param service Defaults to "Body", can alternatively be "Neuro" or "Nuclear"
#'
#' @export

kable_balance_sheet <- function(test_pin = FALSE, service = "Body") {
  pin_name <- get_bal_sheet_name(test_pin, service)
  if (nrow(pins::pin_find(name = pin_name)) > 0) {
    pins::pin_get(pin_name, "rsconnect") %>%
      ignyt_kable()
  }
}

#' Helper function: Balance sheet production vs. test name
#'
#' @param lgl FALSE = return production balance sheet name
#' @param service Defaults to "Body", can alternatively be "Neuro" or "Nuclear"
#'
#' @noRd

get_bal_sheet_name <- function(lgl, service = "Body") {
  if (lgl == FALSE) {
    if (service == "Body") {
      return("ignyt_balance_sheet")
    }
    if (service == "Neuro") {
      return("ignyt_balance_sheet_neuro")
    }
  } else {
    return("ignyt_balance_sheet_test")
  }
}

#' Register Ignyt Board
#'
#' Reigster ignyt pinboard either locally
#' or on RSConnect.
#' Uses environment variable "rsconnect_api_key"
#' if not otherwise specified
#'
#' @param local Logical. Whether to use local or remote pinboard. Defaults to remote (FALSE).
#' @param api_key RStudio Connect API key. Defaults to environment variable "rsconnect_api_key".
#'
#' @import pins
#'
#' @export

register_ignyt_board <- function(local = FALSE, api_key = Sys.getenv("rsconnect_api_key")) {
  if (local) {
    board_register_local(name = "rsconnect")
  } else {
    httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
    board_register_rsconnect(
      name = "rsconnect",
      server = "https://rconnect.hospital.org/",
      key = api_key
    )
  }
}

#' Carry over exam weights from prior shift
#'
#'  Gets overflow or underassigned numbers from previous
#'  shift saved in pinned balance sheet and adjusts current
#'  weights accordingly.
#'
#' @param reader_df Dataframe from QGenda schedule db
#' @param test_pin Logical. Whether to write to production or test pin. Defaults to FALSE = prod.
#' @param pin_board Defaults to "rsconnect"
#' @param service Defaults to "Body", can alternatively be "Neuro" or "Nuclear"
#'
#' @import dplyr tidyr pins
#'
#' @export

carryover_weights <- function(reader_df, test_pin = FALSE, service = "Body", pin_board = "rsconnect") {
  pin_name <- get_bal_sheet_name(test_pin, service = service)

  if (nrow(pin_find(name = pin_name, board = pin_board)) == 0) {
    return(reader_df)
  } else {
    bal <- pin_get(pin_name, board = pin_board)

    bal %<>% filter(`User Name` %in% reader_df$`User Name`)

    if (nrow(bal) > 0) {
      adjusted <- bal %>%
        merge.data.frame(reader_df, by = "User Name", all = TRUE) %>%
        mutate(
          balance = replace_na(balance, 0),
          min_exams = min_exams + balance
        ) %>%
        select(-balance)
      return(adjusted)
    } else {
      return(reader_df)
    }
  }
}

#' Update weighted exam balances in pin
#'
#' There is a pin used to store overflow and underassignment.
#' This function takes the unnested dataframe of assigned exams,
#' boils it down to per-reader and updated the pin.
#'
#' @param df Dataframe, needs to have columns "User Name" and "min_exams"
#' @param test_pin Logical. Whether to write to production or test pin. Defaults to FALSE = prod.
#' @param service Defaults to "Body", can alternatively be "Neuro" or "Nuclear"
#' @param overwrite Logical. Whether to overwrite the pin. Defaults to FALSE = add new balance.
#'
#' @import pins
#' @importFrom dplyr %>% group_by slice
#' @importFrom tidyr replace_na
#'
#' @export

update_pin_bal <- function(df, test_pin = FALSE, service = "Body", overwrite = FALSE) {
  pin_name <- get_bal_sheet_name(test_pin, service = service)

  sel_df <- df %>%
    select(
      `User Name`,
      min_exams
    ) %>%
    group_by(`User Name`) %>%
    slice(which.min(min_exams)) %>%
    ungroup()

  if (nrow(pin_find(name = pin_name, board = "rsconnect")) == 0) {
    pin(sel_df %>% rename(c("balance" = "min_exams")), pin_name, board = "rsconnect")
  } else {
    old <- pin_get(pin_name, board = "rsconnect")
    if (overwrite == TRUE) {
      new <- old %>%
        merge.data.frame(sel_df, by = "User Name", all = TRUE) %>%
        mutate(
          balance = replace_na(balance, 0),
          balance = ifelse(is.na(min_exams), balance, min_exams)
        ) %>%
        select(-min_exams)
    } else {
      new <- old %>%
        merge.data.frame(sel_df, by = "User Name", all = TRUE) %>%
        mutate(
          balance = replace_na(balance, 0),
          balance = ifelse(is.na(min_exams), balance, balance + min_exams)
        ) %>%
        select(-min_exams)
    }
    pin(new, pin_name, board = "rsconnect")
  }
}

#' Null outstanding pinned balances
#'
#' Nulls positive balances in pin board.
#' Run by AM report.
#'
#' @param test_pin Logical. Whether to write to production or test pin. Defaults to FALSE = prod.
#' @param service Defaults to "Body", can alternatively be "Neuro" or "Nuclear"
#'
#' @importFrom dplyr mutate
#' @import pins
#'
#' @return Data frame with new balance sheet.
#' @export
null_pin_bal <- function(test_pin = FALSE, service = "Body") {
  pin_name <- get_bal_sheet_name(test_pin, service = service)
  if (nrow(pin_find(name = pin_name, board = "rsconnect")) == 0) {
    warning("Pin not found. Nothing to null.")
  } else {
    new_pin <- pin_get(pin_name, board = "rsconnect") %>%
      mutate(balance = ifelse(balance > 0, 0, balance))
    if (lubridate::hour(Sys.time()) == 0) new_pin %<>% mutate(balance = ifelse(balance < (-2), -2, balance))
    pin(new_pin, name = pin_name, board = "rsconnect")
  }
}
