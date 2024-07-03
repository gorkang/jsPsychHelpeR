testthat::test_that('Check all input files have the essential columns', {
  
  # Name of test (should reflect the name of the file) ----------------------
  name_of_test = "input_files"
  
  # Read all input_files
  input_files = list_input_files(pid_target)
  
  

  # Data preparation --------------------------------------------------------

  # Read all 
  DF_all = jsPsychHelpeR::read_csv_or_zip(input_files)
  
  DF_final = 
    DF_all |> 
    group_by(filename) |> 
    summarise(across(everything(), ~ sum(is.na(.))))

  
  # Test 1 --------------------------------------------------------------------
  
  essential_columns = c("procedure", "trialid", "rt", "response", "stimulus")
  
  all_filenames = DF_final |> distinct(filename)
  
  DF_filtered_names = DF_final |> 
    pivot_longer(2:ncol(DF_final)) |> 
    filter(name %in% essential_columns) 
  
  all_filenames_with_some_essential_columns = DF_filtered_names |> distinct(filename)
  
  
  # All files have at least some of the essential columns
  testthat::expect_identical(
    nrow(all_filenames), 
    nrow(all_filenames_with_some_essential_columns), 
    info = "Some of the files have NONE of the essential columns"
    )
  
  

  # Test 2 ------------------------------------------------------------------

  DF_files_missing_some_essential_columns = 
    DF_filtered_names |> 
    count(filename) |> 
    filter(n != length(essential_columns))
    
  testthat::expect_equal(
    nrow(DF_files_missing_some_essential_columns), 
    0, 
    info = "Some of the files are missing essential columns"
    )
  
  # Store file with files missing essential columns
  if (nrow(DF_files_missing_some_essential_columns) > 0) data.table::fwrite(DF_files_missing_some_essential_columns, here::here(paste0("outputs/tests_outputs/test-", name_of_test, "_missing_essential.csv")))

  

  # Test 3 -------------------------------------------------------------------
  
  # We expect the csv to be: project_experimento_version_datetime_id.csv
  # If there is a .csv in the id field is because there was a mistake separating the filename
  extra_pieces_in_csv = any(grepl("_", all_filenames |>  parse_filename() |> pull(id)))
  
  testthat::expect_identical(
    extra_pieces_in_csv, FALSE, 
    info = "We expect the csv to be: project_experimento_version_datetime_id.csv, but this has some extra bits separated by '_'"
    )
  
  # Warning and log ---------------------------------------------------------
  
  # if (length(names_missing_columns) > 0) {
  #   
  #   data.table::fwrite(check_input_files_DF, here::here(paste0("outputs/tests_outputs/test-", name_of_test, ".csv")))
  #   
  #   cat(cli::col_red("\nERROR in", paste0("test-", name_of_test), "\n"),
  #       cli::col_red("  - Some of the essential columns are not in all input_files:"), names_missing_columns, "\n",
  #       cli::col_green("  - # of Issues: "), cli::col_red(nrow(check_input_files_DF)), "\n",
  #       cli::col_silver("  - DF with details stored in:", paste0("'outputs/tests_outputs/test-", name_of_test, ".csv'"), "\n\n"))
  #   
  # }
  

})
