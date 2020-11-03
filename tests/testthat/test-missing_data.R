testthat::test_that('Tests if the participant is missing test results', {

  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "missing_data"
  cat(crayon::underline(crayon::yellow(paste0("\n\nRunning: ", crayon::silver(name_of_test, paste(rep(" ", 40), collapse = " ")),"\n\n"))))
  
  
  # Test --------------------------------------------------------------------
  
  # Selects all STDt, STDd, DIRt and DIRd scales
  # all_scales also used in "R/prepare_df_analysis.R"
  all_scales = grep(".*_DIRt$|.*_STDt$|.*_DIRd$|.*STDd$", names(DF_joined), value = TRUE, perl = TRUE)
  
  
  missing_DF = DF_joined %>% 
    select(id, all_of(all_scales)) %>% 
    mutate(NAs_id = rowSums(is.na(select(., matches(all_scales))))) %>% 
    mutate(
      missing_VARS = case_when(
        NAs_id > 0 ~ paste(colnames(.)[!complete.cases(t(.))],  collapse = ", "),
        TRUE ~ "")) %>% 
    select(id, NAs_id, missing_VARS, colnames(.)[!complete.cases(t(.))])
  
  missing_ids = missing_DF %>% 
    filter(NAs_id > 0) %>% 
    arrange(id) %>% 
    pull(id)

  # WORKS WITH MULTIPLE MISSING TESTS?  
  # missing_DF %>% 
  #   filter(NAs_id > 0) %>% 
  #   arrange(id) %>% 
  #   group_by(missing_VARS) %>% 
  #   summarise(participants = paste(id, collapse = ", "), .groups = "drop") %>% 
  #   mutate(MESSAGE = paste0(missing_VARS, ": ", participants)) %>% 
  #   pull(MESSAGE)
  
  # NEEDS TO WORK FOR EACH ROW!
  # missing_DF = 
  #   DF_joined %>% 
  #   # select(id, all_of(all_scales)) %>% 
  #   mutate(NAs_id = rowSums(is.na(select(., everything())))) %>% 
  #   rowwise() %>% 
  #   mutate(
  #     missing_VARS = 
  #       case_when(
  #         NAs_id > 0 ~ paste(colnames(.)[!complete.cases(t(.))],  collapse = ", "),
  #         TRUE ~ "")) %>% 
  #   select(id, NAs_id, missing_VARS, colnames(.)[!complete.cases(t(.))])
  
  
  # Warning and log ---------------------------------------------------------
  
  if (length(missing_ids) > 0) {
    
    write_csv(missing_DF, here::here(paste0("output/tests_outputs/test-", name_of_test, ".csv")))
    
    cat(crayon::red("\n", crayon::underline("ERROR"), "in", paste0("test-", name_of_test), "\n"),
        crayon::red("  - Some of the participants are", crayon::underline("missing test results:")), missing_ids, "\n",
        crayon::green("  - # of Issues: "), crayon::red(length(missing_ids)), "\n",
        crayon::silver("  - DF with details stored in:", paste0("'output/tests_outputs/test-", name_of_test, ".csv'"), "\n\n"))
    
  }
  
  # Actual expectations -------------------------------------------------------------
  
  testthat::expect_gt(ncol(missing_DF), 2) # Checks that we have some columns in the DF
  testthat::expect_length(missing_ids, 0) # Check that there are no id's with missing data

})
