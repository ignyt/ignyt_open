library(lubridate)
library(dplyr)

test_week <- readr::read_csv2("data-raw/test_week.csv.gz")
test_faculty <- readr::read_csv2("data-raw/test_week_fac.csv.gz")
testweek_days <- as_date("2020-03-02"):as_date("2020-03-08") %>% 
  as_date()

# usethis::use_data(test_week, overwrite = TRUE, internal = TRUE)
