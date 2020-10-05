##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

##' @return
##' @author gorkang
##' @export
test_testhat <- function(...) {

  # [TODO]: Load all objects in "..." with   targets::tar_load(XXX, envir = .GlobalEnv) --------------------
  
  # Print
  num_tests = list.files("tests/testthat/") %>% length()
  cat(blue(paste0(" - ", num_tests, " tests to run: ")))
  
  
  # Load targets objects used in tests --------------------------------------
  
  targets::tar_load(DF, envir = .GlobalEnv)
  targets::tar_load(df_SBS, envir = .GlobalEnv)
  


  # Run all the tests ------------------------------------------------------

  test_dir(path = here::here("tests/testthat/"), env = .GlobalEnv, stop_on_failure = TRUE)

}


# Creating automatic CHECKS
  # GorkaFunctions::createtest_df(DF, num_variables_to_test = 10)
  # GorkaFunctions::createtest_df(df_SBS, num_variables_to_test = 2)
