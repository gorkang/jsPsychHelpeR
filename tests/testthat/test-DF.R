testthat::test_that('Check if any of the items appear more or less than the other items for each given test', {

  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "DF"
  
  
  # Test --------------------------------------------------------------------
  
  offender_tests =
    DF %>% 
    group_by(experimento) %>% 
    count(trialid, name = "times_item_shown") %>% 
    count(times_item_shown) %>% 
    count() %>% # How many times an experiment appears?
    filter(n > 1) %>% 
    pull(experimento)
  
  
  checks_DF = DF %>% 
    group_by(experimento) %>% 
    count(trialid) %>% 
    filter(experimento %in% offender_tests)
  
  
  # Warning and log ---------------------------------------------------------
  
  if (length(offender_tests) > 0) {
    
    write_csv(checks_DF, here::here("output/tests_outputs/test-DF.csv"))
    
    cat(crayon::red("\nERROR in", paste0("test-", name_of_test), "\n"),
        crayon::red("  - Some of the items appear more than others in the same tests:"), offender_tests, "\n",
        crayon::green("  - # of Issues: "), crayon::red(length(offender_tests)), "\n",
        crayon::silver("  - DF with details stored in:", paste0("'output/tests_outputs/test-", name_of_test, ".csv'"), "\n\n"))
    
  }
  
  # Actual expectation -------------------------------------------------------------
  
  testthat::expect_length(offender_tests, 0)

})
