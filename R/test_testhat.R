##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

##' @return
##' @author gorkang
##' @export
test_testhat <- function(...) {

  # Load targets objects used in tests --------------------------------------
  
  argnames <- sys.call()
  arguments = lapply(argnames[-1], as.character) %>% unlist()
  targets::tar_load(all_of(arguments), envir = .GlobalEnv)
  
  
  # Print
  num_tests = list.files("tests/testthat/") %>% length()
  cat(crayon::blue(crayon::underline(paste0("\n\nRunning ", num_tests, " tests:"))), " \n")
  

  # Run all the tests ------------------------------------------------------

  testthat::test_dir(path = here::here("tests/testthat/"), env = .GlobalEnv, stop_on_failure = TRUE, reporter = StopReporter)#ProgressReporter
  
}



# Creating automatic CHECKS
  # library(tidyverse)
  # GorkaFunctions::createtest_df_snapshot(DF_clean, n_char_variables_to_test = 10, n_num_variables_to_test = 3)
  # GorkaFunctions::createtest_df_snapshot(df_SBS, n_char_variables_to_test = 1, n_num_variables_to_test = 10)
  # GorkaFunctions::createtest_df_snapshot(df_CRT7, n_char_variables_to_test = 1, n_num_variables_to_test = 10)
  # GorkaFunctions::createtest_df_snapshot(df_MagicalIdeation, n_char_variables_to_test = 1, n_num_variables_to_test = 10)
