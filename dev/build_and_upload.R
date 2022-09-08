# Automation of package building and pushing of modules to RConnect

# Build and push to drat github repo ----------------------------------

ignyt::ps_close()
source("data-raw/update_all.R")
rstudioapi::executeCommand("buildAll")

Sys.sleep(30)

rstudioapi::executeCommand("buildSourcePackage")

Sys.sleep(90)

library(here)
set_here(path='..')

built_pkg <- list.files(here(), "tar.gz", full.names = TRUE)

drat_repo <- here("ignyt_drat_repo")

drat::insertPackage(built_pkg, drat_repo)

unlink(built_pkg)

git_cmd <- glue::glue(sprintf('cd "%s" && make git m="{packageVersion("ignyt")}"', drat_repo))
system(git_cmd, wait = TRUE)

app_dir <- here("ignyt/inst/rmd")


# Deploying main modules --------------------------------------------------

rsconnect::deployApp(
  appDir = app_dir, appPrimaryDoc = "ignyt_ctmr_master.Rmd", 
  appId = 1, appName = "ignyt_ctmr_am", 
  account = "test_user", logLevel = "verbose", forceUpdate = TRUE
)
rsconnect::deployApp(
  appDir = app_dir, appPrimaryDoc = "ignyt_ctmr_master.Rmd", 
  appId = 2, appName = "ignyt_ctmr_pm", 
  account = "test_user", logLevel = "verbose", forceUpdate = TRUE
)

rsconnect::deployApp(
  appDir = app_dir, appPrimaryDoc = "ignyt_cleanup_master.Rmd", 
  appId = 3, appName = "ignyt_cleanup_am", 
  account = "test_user", logLevel = "verbose", forceUpdate = TRUE
)
rsconnect::deployApp(
  appDir = app_dir, appPrimaryDoc = "ignyt_cleanup_master.Rmd",
  appId = 4, appName = "ignyt_cleanup_pm", 
  account = "test_user", logLevel = "verbose", forceUpdate = TRUE
)

# Deploy test modules -----------------------------------------------------

rsconnect::deployApp(
  appDir = app_dir, appPrimaryDoc = "ignyt_ctmr_test.Rmd", 
  appId = 5, appName = "ignyt_ctmr_test", #387 (old, non-parameterized)
  account = "test_user", logLevel = "verbose", forceUpdate = TRUE
)
rsconnect::deployApp(
  appDir = app_dir, appPrimaryDoc = "ignyt_cleanup_test.Rmd", 
  appId = 6, appName = "ignyt_cleanup_test", 
  account = "test_user", logLevel = "verbose", forceUpdate = TRUE
)

# Deploy ancillary modules ------------------------------------------------

rsconnect::deployApp(
  appDir = app_dir, appPrimaryDoc = "ignyt_statmailr.Rmd",
  appId = 7, appName = "ignyt_statmailr", 
  account = "test_user", logLevel = "verbose", forceUpdate = TRUE
)

# Deploy pinboard related modules -----------------------------------------

rsconnect::deployApp(
  appDir = app_dir, appPrimaryDoc = "ignyt_update_pinbal.Rmd",
  appId = 8, appName = "ignyt_update_pinbal", 
  account = "test_user", logLevel = "verbose", forceUpdate = TRUE
)

rsconnect::deployApp(
  appDir = app_dir, appPrimaryDoc = "ignyt_pin2sql.Rmd",
  appId = 9, appName = "ignyt_pin2sql", 
  account = "test_user", logLevel = "verbose", forceUpdate = TRUE
)
