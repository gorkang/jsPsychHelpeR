##' Calls all the tests in 'test/testthat'
##'
##' .. content for \details{} ..
##'
##' @title

##' @return
##' @author gorkang
##' @export
test_testhat <- function(...) { # input_files_automatic_tests_str

  # Load targets objects used in tests --------------------------------------
  
  argnames <- sys.call()
  arguments = lapply(argnames[-1], as.character) %>% unlist()
  
  # browser()
  # Load targets
  # targets::tar_load(all_of(input_files_automatic_tests_str), envir = .GlobalEnv)
  targets::tar_load(all_of(arguments), envir = .GlobalEnv)
  
  
  # Print
  num_tests = list.files("tests/testthat/", pattern = ".R") %>% length()
  cat(crayon::blue(crayon::underline(crayon::bold(paste0("\n\n[Running ", num_tests, " tests]:", paste(rep(" ", 60), collapse = " ")), " \n"))))
  # cat(crayon::blue(crayon::underline(paste(rep(" ", 48), collapse = " "))), " \n")

  
  # Check we have automatic tests for all tasks (each task should have a df_[SHORT_NAME_OF_TASK]) in _targets/outputs
  # automatic_tests_raw = list.files(path = "tests/testthat/", pattern="df_.*snapshot", full.names = FALSE, ignore.case = FALSE)
  # automatic_tests = gsub("test-(.*)_snapshot.R", "\\1", automatic_tests_raw)
  # missing_tests = input_files_automatic_tests_str[!input_files_automatic_tests_str %in% automatic_tests]
  # if (length(missing_tests) > 0)  cat(crayon::red(paste0("\n\n[WARNING]: Missing snapshot tests: ")), paste(missing_tests, collapse = ","), "\n")
  # 
  

  # Run all the tests ------------------------------------------------------

  testthat::test_dir(path = here::here("tests/testthat/"), env = .GlobalEnv, stop_on_failure = FALSE, reporter = ProgressReporter)# ProgressReporter StopReporter
  
  cat(crayon::blue(crayon::underline(crayon::bold(paste0("\n\n[END tests]:", paste(rep(" ", 65), collapse = " ")), " \n\n\n"))))
  
}



# Creating automatic CHECKS
# [TODO] This should be a helper function
  # Create tests and DELETE old tests?
  # Should tests take an actual snapshot of the object (.rds) dating it?

  # library(tidyverse)
  # GorkaFunctions::createtest_df_snapshot(DF_clean, n_char_variables_to_test = 9, n_num_variables_to_test = 3)
  # GorkaFunctions::createtest_df_snapshot(df_CRT7, n_char_variables_to_test = 1, n_num_variables_to_test = 10)
  # GorkaFunctions::createtest_df_snapshot(df_bRCOPE, n_char_variables_to_test = 15, n_num_variables_to_test = 19)
  ## NOT ADDED TO GET WARNING ABOUT TEST NOT EXISTING # GorkaFunctions::createtest_df_snapshot(df_MIS, n_char_variables_to_test = 15, n_num_variables_to_test = 19)