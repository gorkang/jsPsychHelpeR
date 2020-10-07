testthat::test_that('Check if the trialid question_text are unique', {

  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "trialid_question"


  # Test --------------------------------------------------------------------
  
  non_unique_trialid = 
    DF %>% 
    mutate(question_text = 
             case_when(
               is.na(question_text) ~ stimulus,
               is.na(stimulus) ~ question_text,
               TRUE ~ NA_character_
             )) %>% 
    group_by(experimento) %>% 
    count(trialid, question_text) %>% 
    group_by(trialid, question_text) %>% 
    count() %>% 
    drop_na(trialid) %>% 
    filter(n > 1) %>%  
    pull(trialid)
  

  # Warning and log ---------------------------------------------------------
  
  if (length(non_unique_trialid) > 0) {
    
    write_csv(non_unique_trialid %>% as_tibble(), here::here(paste0("output/tests_outputs/test-", name_of_test, ".csv")))
    
    cat(crayon::red("\nERROR in", paste0("test-", name_of_test), "\n"),
        crayon::red("  - Some of the trialid's are not unique:"), non_unique_trialid, "\n",
        crayon::green("  - # of Issues: "), crayon::red(length(non_unique_trialid)), "\n",
        crayon::silver("  - DF with details stored in:", paste0("'output/tests_outputs/test-", name_of_test, ".csv'"), "\n\n"))
  }
  

  # Actual expectation -------------------------------------------------------------
  
  testthat::expect_length(non_unique_trialid, 0)

})
