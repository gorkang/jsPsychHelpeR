testthat::test_that('Check DF_joined', {

  # DEBUG
  # targets::tar_load(c(DF_joined, DICCIONARY_tasks))
  
  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "DF_joined"
  cat(crayon::underline(crayon::yellow(paste0("\n\nRunning: ", crayon::silver(name_of_test, paste(rep(" ", 40), collapse = " ")),"\n\n"))))


  # TEST 1: NON canonical names --------------------------------------------------------------------
  
  expected_names = grep("^.*_RAW$|^.*_DIR$|^.*_DIRt$|^.*_DIRd$|^.*_RELt$|^.*_RELd$|^.*_STDt$|^.*_STDd|^.*DIR_NA$|^.*RAW_NA$", names(DF_joined %>% select(-id)), ignore.case = FALSE, fixed = FALSE, value = TRUE)
  non_canonical_names = names(DF_joined %>% select(-id))[!names(DF_joined %>% select(-id)) %in% expected_names]

  if (length(non_canonical_names) > 0) cat(crayon::red(paste0("\nERROR: DF_joined contains non-standard columns: ", crayon::silver(paste(non_canonical_names, collapse = ", ")), "\n\n")))

  
  
  # TEST 2: missing tests ---------------------------------------------------

  # Tasks in DF_clean that don't pass to DF_joined
  white_list = c("Consent", "Bank")
  
  # Targets
  existing_targets_raw = list.files(path = here::here("_targets/objects/"), pattern = "df_.*", full.names = FALSE, ignore.case = FALSE)
  existing_targets = gsub("df_", "", existing_targets_raw)
  
  # Diccionary
  tasks_in_diccionary = 
    DICCIONARY_tasks %>% 
    distinct(names = `short_name: from trialid`) %>% 
    filter(!names %in% white_list) %>% 
    pull()
  
  # DF_joined
  tasks_joined = names(DF_joined) %>% as_tibble() %>%
    filter(grepl("DIR_NA", value)) %>% 
    mutate(name = gsub("_DIR_NA", "", value)) %>% pull(name)
  
  
  
  # Tasks in diccionary (DF_clean) not in DF_joined
    missing_from_DF_joined = tasks_in_diccionary[!tasks_in_diccionary %in% tasks_joined]
    if (length(missing_from_DF_joined) != 0) cat(crayon::yellow(paste0("\n[WARNING]: Tasks in DF_clean missing from DF_joined: ", crayon::silver(paste(missing_from_DF_joined, collapse = ", ")), "\n\n")))
  
  # Existing targets that have not been joined
    # - Can be because the name is different (SHOULD CORRECT)
    # - Or because we did not include them in create_joined()
    targets_joined = existing_targets[!existing_targets %in% tasks_joined]
    if (length(existing_targets[!existing_targets %in% tasks_joined]) != 0) cat(crayon::yellow(paste0("\n[WARNING]: Tasks with _targets missing from DF_joined:"), crayon::silver(paste(existing_targets[!existing_targets %in% tasks_joined], collapse = ", "))), "\n",
                                                                                "In '_targets.R' file, '_Prepare tasks' section: Target names should be '", crayon::underline("df_[short_name_scale_str]"), "'\n",
                                                                                paste0("For example: 'tar_target(df_", crayon::bgGreen("SBS") ,", prepare_SBS(DF_clean,  name_scale_str = 'Supernatural_Belief_Scale', short_name_scale_str = '", crayon::bgGreen("SBS") ,"'))'\n"),
                                                                                crayon::silver("  - existing_targets:", paste(existing_targets, collapse = ", "), "\n",
                                                                                               "  - tasks_joined:", paste(tasks_joined, collapse = ", "), "\n\n"))


  # TEST 3: Number of 9999 values  ------------------------------------------------------------------

    DF_999 = DF_joined %>% 
      pivot_longer(2:ncol(.), values_transform = list(value = as.character)) %>% 
      filter(value == "9999") %>% # Solved the issue below with DIR == 9999 ~ DIR,?
      # filter(grepl("999", value)) %>% # We reverse items after transforming to dir... sometimes the 9999 gets transform to -9993 or others,,,
      distinct(name, value, .keep_all = FALSE)
    
    
    
  # Warning and log ---------------------------------------------------------
  
  if (length(missing_from_DF_joined) > 0 | length(targets_joined) > 0) {
    
    df_missing = missing_from_DF_joined %>% 
      as_tibble() %>% 
      mutate(what = "In diccionary, missing in join") %>%
      bind_rows(
        targets_joined %>% 
          as_tibble() %>% 
          mutate(what = "In targets, missing in join")
        )
    
    write_csv(df_missing, here::here(paste0("outputs/tests_outputs/test-", name_of_test, ".csv")))
    
    cat(crayon::red("\nERROR in", paste0("test-", name_of_test), "\n"),
        crayon::red("  - Some tasks are in DF_clean but not in DF_joined:"), "", "\n",
        crayon::green("  - # of Issues: "), crayon::red(nrow(df_missing)), "\n",
        crayon::silver("  - DF with details stored in:", paste0("'outputs/tests_outputs/test-", name_of_test, ".csv'"), "\n\n"))
  }
  

  # Actual expectation -------------------------------------------------------------
  
  testthat::expect_length(non_canonical_names, 0)
  testthat::expect_length(missing_from_DF_joined, 0)
  testthat::expect_length(targets_joined, 0)

  testthat::expect_equal(DF_999 %>% nrow(),
                         0,
                         label = paste0("Number of 9999 values (errors from RAW to DIR) [", paste(DF_999 %>% pull(name), collapse = ", "), "] ")
  )

})
