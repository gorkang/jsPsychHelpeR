testthat::test_that('Check if the trialid question_text are unique', {
  
  # DEBUG
  # targets::tar_load(c(DF_clean))
  
  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "trialid_question"
  # cat(cli::style_underline(cli::col_yellow(paste0("\n\nRunning: ", cli::col_silver(name_of_test, paste(rep(" ", 40), collapse = " ")),"\n\n"))))

  
  # Test --------------------------------------------------------------------
  
  DF_trialid = 
    DF_clean %>% 
    # dplyr::mutate(question_text = 
    #         dplyr::case_when(
    #            is.na(question_text) ~ stimulus,
    #            is.na(stimulus) ~ question_text,
    #            TRUE ~ NA_character_
    #          )) %>% 
    dplyr::group_by(experiment) %>% 
    dplyr::count(trialid, stimulus) %>% 
    dplyr::group_by(trialid, stimulus) %>% 
    dplyr::count() %>% 
   tidyr::drop_na(trialid) 
  
  non_unique_trialid = 
    DF_trialid %>% 
    dplyr::filter(n > 1) %>%  
    dplyr::pull(trialid)
  

  # Warning and log ---------------------------------------------------------
  
  if (length(non_unique_trialid) > 0) {
    
    data.table::fwrite(non_unique_trialid %>% tibble::as_tibble(), here::here(paste0("outputs/tests_outputs/test-", name_of_test, ".csv")))
    
    cat(cli::col_red("\nERROR in", paste0("test-", name_of_test), "\n"),
        cli::col_red("  - Some of the trialid's are not unique:"), non_unique_trialid, "\n",
        cli::col_green("  - # of Issues: "), cli::col_red(length(non_unique_trialid)), "\n",
        cli::col_silver("  - DF with details stored in:", paste0("'outputs/tests_outputs/test-", name_of_test, ".csv'"), "\n\n"))
  }
  

  # Actual expectation -------------------------------------------------------------
  
  testthat::expect_gt(nrow(DF_trialid), 2) # Checks that we have some rows in the DF
  testthat::expect_length(non_unique_trialid, 0)

})
