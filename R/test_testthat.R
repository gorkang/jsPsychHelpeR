#' test_testthat
#' 
#' Calls all the tests in 'test/testthat'
#'
#' @param ... .
#'
#' @return Calls tests
#' @export
test_testthat <- function(...) { # input_files_automatic_tests_str

  # DEBUG
  # arguments = c("df_AIM", "df_Consent")
  
  # Load targets objects used in tests --------------------------------------
  
  argnames <- sys.call()
  arguments = lapply(argnames[-1], as.character) %>% unlist()
  
  # Load targets
  # Since targets 1.3.0 cannot use tar_load() in a running pipeline
  1:length(arguments) |> 
    walk(~{
      if (file.exists(paste0("_targets/objects/", arguments[.x]))) assign(arguments[.x], readRDS(paste0("_targets/objects/", arguments[.x])), envir = .GlobalEnv)
    })
  
  
  # Print
  num_tests = list.files("tests/testthat/", pattern = ".R") %>% length()
  cat(cli::col_blue(cli::style_underline(cli::style_bold(paste0("\n\n[Running ", num_tests, " tests]:", paste(rep(" ", 60), collapse = " ")), " \n"))))

  # TODO: ADAPT to new test-snapshots.R
    # Check we have automatic tests for all tasks (each task should have a df_[SHORT_NAME_OF_TASK]) in _targets/outputs
    # automatic_tests_raw = list.files(path = "tests/testthat/", pattern="df_.*snapshot", full.names = FALSE, ignore.case = FALSE)
    # automatic_tests = gsub("test-(.*)_snapshot.R", "\\1", automatic_tests_raw)
    # missing_tests = input_files_automatic_tests_str[!input_files_automatic_tests_str %in% automatic_tests]
    # if (length(missing_tests) > 0)  cat(cli::col_red(paste0("\n\n[WARNING]: Missing snapshot tests: ")), paste(missing_tests, collapse = ","), "\n")


  # Run all the tests ------------------------------------------------------

  testthat::test_dir(path = here::here("tests/testthat/"), env = .GlobalEnv, stop_on_failure = FALSE, reporter = ProgressReporter)# ProgressReporter StopReporter

  cli::cli_h1("[END tests]")  
  
}
