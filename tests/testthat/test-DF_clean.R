testthat::test_that('Check if any of the items appear more or less than the other items for each given test', {

  # DEBUG
  # targets::tar_load(c(DF_clean))
  
  # testthat::local_edition(3)
  
  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "DF_clean"
  white_list = c("SASS_02", "SASS_03", "SDG_05", "SDG_07", "SDG_08",
                 "AIM_04", "AIM_05", "AIM_06", "AIM_07", "AIM_08", "AIM_09", "AIM_10") # Items where there is a conditional controlling if the question is shown
  cat(crayon::underline(crayon::yellow(paste0("\n\nRunning: ", crayon::silver(name_of_test, paste(rep(" ", 40), collapse = " ")),"\n\n"))))
  
  # Test --------------------------------------------------------------------
  
  DF_temp = 
    DF_clean %>% 
    group_by(experimento) %>% 
    count(trialid, name = "times_item_shown") %>% 
    filter(!trialid %in% white_list) %>% 
    count(times_item_shown) %>% 
    count() # How many times an experiment appears?
    
  DF_clean_offender_tests =
    DF_temp %>% 
    filter(n > 1) %>% 
    pull(experimento)
  
  checks_DF = 
    DF_clean %>% 
    group_by(experimento) %>% 
    count(trialid) %>% 
    filter(experimento %in% DF_clean_offender_tests)
  
  
  # Warning and log ---------------------------------------------------------
  
  if (length(DF_clean_offender_tests) > 0) {
    
    write_csv(checks_DF, here::here(paste0("outputs/tests_outputs/test-", name_of_test, ".csv")))
    
    cat(crayon::red("\nERROR in", paste0("test-", name_of_test), "\n"),
        crayon::red("  - Some of the items appear more than others in the same tests:"), DF_clean_offender_tests, "\n",
        crayon::green("  - # of Issues: "), crayon::red(length(DF_clean_offender_tests)), "\n",
        crayon::silver("  - DF with details stored in:", paste0("'outputs/tests_outputs/test-", name_of_test, ".csv'"), "\n\n"))
    
  }
  
  # Actual expectation -------------------------------------------------------------
  
  testthat::expect_gt(nrow(DF_temp), 1) # Checks that we have some rows in the DF
  testthat::expect_length(DF_clean_offender_tests, 0)
  
})
