testthat::test_that('Check if any of the items appear more or less than the other items for each given test', {

  # DEBUG
  # targets::tar_load(c(DF_clean))
  
  # testthat::local_edition(3)
  
  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "DF_clean"
  
  # In general, these SHOULD be IDQ_09_1, IDQ_11_1, IDQ_13_1 ???
  white_list = c("AIM_04", "AIM_05", "AIM_06", "AIM_07", "AIM_08", "AIM_09", "AIM_10",
                 "DEMOGR24_12", "DEMOGR24_14",
                 "IDQ_10", "IDQ_12", "IDQ_14",
                 "INFCONS_01", "INFCONS_02", "INFCONS_03", "INFCONS_04", "INFCONS_05", "INFCONS_06", "INFCONS_07", "INFCONS_08", "INFCONS_09", "INFCONS_10", "INFCONS_11", "INFCONS_12", "INFCONS_13", "INFCONS_14", "INFCONS_15", "INFCONS_16", "INFCONS_17", "INFCONS_18", "INFCONS_19", "INFCONS_20", "INFCONS_21", "INFCONS_22", 
                 "OTRASRELIG_07", "OTRASRELIG_09",
                 "PRFBM_02", "PRFBM_03", "PRFBMpost_02", "PRFBMpost_03",
                 "PVC_001_1",
                 "Report_001_1", "Report_001_2",
                 "SASS_02", "SASS_03", "SDG_05", "SDG_07", "SDG_08"
                 ) # Items where there is a conditional controlling if the question is shown
  cat(cli::style_underline(cli::col_yellow(paste0("\n\nRunning: ", cli::col_silver(name_of_test, paste(rep(" ", 40), collapse = " ")),"\n\n"))))
  
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
    
    cat(cli::col_red("\nERROR in", paste0("test-", name_of_test), "\n"),
        cli::col_red("  - Some of the items appear more than others in the same tests:"), DF_clean_offender_tests, "\n",
        cli::col_green("  - # of Issues: "), cli::col_red(length(DF_clean_offender_tests)), "\n",
        cli::col_silver("  - DF with details stored in:", paste0("'outputs/tests_outputs/test-", name_of_test, ".csv'"), "\n\n"))
    
  }
  
  # Actual expectation -------------------------------------------------------------
  
  testthat::expect_gt(nrow(DF_temp), 1) # Checks that we have some rows in the DF
  testthat::expect_length(DF_clean_offender_tests, 0)
  
})
