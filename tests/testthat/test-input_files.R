testthat::test_that('Check all input files have the essential columns', {
  
  # DEBUG
  # targets::tar_load(c(input_files))
  
  
  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "input_files"
  cat(cli::style_underline(cli::col_yellow(paste0("\n\nRunning: ", cli::col_silver(name_of_test, paste(rep(" ", 40), collapse = " ")),"\n\n"))))
  
  
  # Test --------------------------------------------------------------------
  
  # Store column names of files
  read_check <- function(file_name, workers) {
    DF = data.table::fread(here::here(file_name), encoding = 'UTF-8', nThread = 1)
    names(DF) %>% as_tibble() %>% mutate(filename = file_name)
  }
  
  all_csvs = all(grepl("\\.csv", input_files))
  all_zips = all(grepl("\\.zip", input_files))
  
  # Read file/s
  if (all_csvs) {
    
    # We read ONE file for each task. Otherwise, with big protocols it takes ages
    one_of_each_file = 
      tibble(filename = basename(input_files)) %>% 
      parse_filename() |> 
      group_by(experiment) %>% 
      sample_n(1) %>% 
      pull(filename)
    
    # Construimos DF global
    DF_final = purrr::map_dfr(paste0(dirname(input_files), "/", one_of_each_file), read_check, workers = workers) %>% 
      parse_filename()

  # Essential columns
  essential_columns = c("procedure", "trialid", "rt", "responses", "stimulus")
  whitelist_tasks = ""#c("Consent", "Goodbye")
  
  names_missing_columns = 
    DF_final %>% 
    filter(!experiment %in% whitelist_tasks) %>% 
    # count(experiment, value) 
    count(value) %>% # Todos los campos deberian aparecer length(number_of_files) veces
    filter(value %in% essential_columns) %>%
    filter(n != max(n)) %>% 
    pull(value)
  
  
  # Buscamos en que archivos NO aparecen determinadas columnas
  wich_files_missing_columns <- function(name_missing_column, DF_final) {
    
    # cat(cli::col_green("\n - Missing column: "), name_missing_column)
    DF_final %>% 
      group_by(filename) %>% 
      # filter(!"responses" %in% value) %>%
      filter(!name_missing_column %in% value) %>%
      
      ungroup() %>% 
      distinct(filename) %>% 
      pull(filename)
    
  }
  
  if (length(names_missing_columns) > 0) {
    check_input_files_DF = 
      1:length(names_missing_columns) %>% 
      map_df(~ wich_files_missing_columns(name_missing_column = names_missing_columns[.x], DF_final) %>% as_tibble() %>% mutate(column_missing = names_missing_columns[.x]) %>% rename(file = value))
  }
  
  

  # TEST2 -------------------------------------------------------------------
  
  # We expect the csv to be: project_experimento_version_datetime_id.csv
  # If there is a .csv in the id field is because there was a mistake separating the filename
   extra_pieces_in_csv = any(grepl("_", DF_final$id))
  
  
  
  # Warning and log ---------------------------------------------------------
  
  if (length(names_missing_columns) > 0) {
    
    write_csv(check_input_files_DF, here::here(paste0("outputs/tests_outputs/test-", name_of_test, ".csv")))
    
    cat(cli::col_red("\nERROR in", paste0("test-", name_of_test), "\n"),
        cli::col_red("  - Some of the essential columns are not in all input_files:"), names_missing_columns, "\n",
        cli::col_green("  - # of Issues: "), cli::col_red(nrow(check_input_files_DF)), "\n",
        cli::col_silver("  - DF with details stored in:", paste0("'outputs/tests_outputs/test-", name_of_test, ".csv'"), "\n\n"))
    
  }
  
  
  # Actual expectation -------------------------------------------------------------
  DF_columns_files = DF_final %>% count(value)
  testthat::expect_gt(nrow(DF_columns_files), 1) # Checks that we have some rows in the DF
  testthat::expect_length(names_missing_columns, 0) # No missing critical columns
  testthat::expect_identical(extra_pieces_in_csv, FALSE, info = "We expect the csv to be: project_experimento_version_datetime_id.csv, but this has some extra bits separated by '_'")

  } else if (all_zips) {
    
    cli::cli_alert_info("This test does not run when input is a zip file")    
    
  } else {
    cli::cli_abort("Something wrong in read_data(). Are input files all csv files or a single zip file?")
  }
  
  
})
