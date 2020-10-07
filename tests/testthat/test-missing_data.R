testthat::test_that('Tests if the participant is missing test results', {

  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "missing_data"
  
  
  # Test --------------------------------------------------------------------
  
  all_scales = grep("^((?!_[0-9]{1,2}_).)*_PROC$", names(df_joined), value = TRUE, perl = TRUE)
  
  missing_DF = df_joined %>% 
    select(id, all_scales) %>% 
    mutate(NAs_id = rowSums(is.na(select(., matches(all_scales))))) %>% 
    select(id, NAs_id, everything())
  
  missing_ids = missing_DF %>% 
    filter(NAs_id > 0) %>% 
    arrange(id) %>% 
    pull(id)
  
  
  # Warning and log ---------------------------------------------------------
  
  if (length(missing_ids) > 0) {
    
    write_csv(missing_DF, here::here(paste0("output/tests_outputs/test-", name_of_test, ".csv")))
    
    cat(crayon::red("\n", crayon::underline("ERROR"), "in", paste0("test-", name_of_test), "\n"),
        crayon::red("  - Some of the participants are", crayon::underline("missing test results:")), missing_ids, "\n",
        crayon::green("  - # of Issues: "), crayon::red(length(missing_ids)), "\n",
        crayon::silver("  - DF with details stored in:", paste0("'output/tests_outputs/test-", name_of_test, ".csv'"), "\n\n"))
    
  }
  
  # Actual expectation -------------------------------------------------------------
  
  testthat::expect_length(missing_ids, 0)

})
