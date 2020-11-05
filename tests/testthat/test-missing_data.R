testthat::test_that('Tests if the participant is missing test results', {

  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "missing_data"
  cat(crayon::underline(crayon::yellow(paste0("\n\nRunning: ", crayon::silver(name_of_test, paste(rep(" ", 40), collapse = " ")),"\n\n"))))
  
  
  # Test: missing data in standardized vars --------------------------------------------------------------
  
  # Selects all STDt, STDd, DIRt and DIRd scales
  # all_scales also used in "R/prepare_df_analysis.R"
  all_scales = grep(".*_DIRt$|.*_STDt$|.*_DIRd$|.*STDd$", names(DF_joined), value = TRUE, perl = TRUE)
  
  
  missing_DF_temp = DF_joined %>% 
    select(id, all_of(all_scales)) %>% 
    mutate(NAs_id = rowSums(is.na(select(., matches(all_scales)))))
  
  # Get names
  DF_missing_vars = missing_DF_temp %>% 
    mutate(across(-id, is.na)) %>%  # replace all NA with TRUE and else FALSE
    pivot_longer(-id, names_to = "var") %>%  # pivot longer
    filter(value == TRUE) %>% 
    group_by(id) %>%    # group by the ID
    summarise(Missing_Variables = toString(var), .groups = "keep")
  
  missing_DF = missing_DF_temp %>% 
    left_join(DF_missing_vars, by = "id") %>% 
    select(id, NAs_id, Missing_Variables, colnames(.)[!complete.cases(t(.))]) %>% 
    filter(NAs_id > 0)
  
  missing_vars = missing_DF %>% select(-id, -NAs_id, -Missing_Variables) %>% names(.)
  
  missing_ids = missing_DF_temp %>% 
    filter(NAs_id > 0) %>% 
    arrange(id) %>% 
    pull(id)
  
  
  

  # Test: missings in any item ----------------------------------------------

  missingall_DF_temp = DF_joined %>% 
    select(-all_of(all_scales)) %>% 
    mutate(NAs_id = rowSums(is.na(select(., -matches(all_scales)))))
  
  # Get names
  DF_missingall_vars = missingall_DF_temp %>% 
    mutate(across(-id, is.na)) %>%  # replace all NA with TRUE and else FALSE
    pivot_longer(-id, names_to = "var") %>%  # pivot longer
    filter(value == TRUE) %>% 
    group_by(id) %>%    # group by the ID
    summarise(Missing_Variables = toString(var), .groups = "keep")
  
  missingall_DF = missingall_DF_temp %>% 
    left_join(DF_missingall_vars, by = "id") %>% 
    select(id, NAs_id, Missing_Variables, colnames(.)[!complete.cases(t(.))]) %>% 
    filter(NAs_id > 0)
  
  # Warning and log ---------------------------------------------------------
  
  if (length(missing_ids) > 0) {
    
    write_csv(missing_DF, here::here(paste0("output/tests_outputs/test-", name_of_test, ".csv")))
    write_csv(missingall_DF, here::here(paste0("output/tests_outputs/test-", name_of_test, "_ALL.csv")))
    
    cat(crayon::red("\n", crayon::underline("ERROR"), "in", paste0("test-", name_of_test), "\n"),
        crayon::red("  - Some of the participants are", crayon::underline("missing test results:")), missing_ids, "\n",
        crayon::red("  - In the following vars:"), missing_vars, "\n",
        
        crayon::green("  - # of Issues: "), crayon::red(length(missing_ids)), "\n",
        crayon::silver("  - DF with details stored in:", paste0("'output/tests_outputs/test-", name_of_test, ".csv'"), "\n\n"))
    
  }
  
  # Actual expectations -------------------------------------------------------------
  
  testthat::expect_gt(ncol(missing_DF), 2) # Checks that we have some columns in the DF
  testthat::expect_length(missing_ids, 0) # Check that there are no id's with missing data

})
