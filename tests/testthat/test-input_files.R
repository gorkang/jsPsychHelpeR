testthat::test_that('Check all input files have the same columns', {

  # DEBUG
  # targets::tar_load(c(input_files))
  
  
  # Name of test (should reflect the name of the file) ----------------------
  
  name_of_test = "input_files"
  workers = 1
  cat(crayon::underline(crayon::yellow(paste0("\n\nRunning: ", crayon::silver(name_of_test, paste(rep(" ", 40), collapse = " ")),"\n\n"))))
  
  
  # Test --------------------------------------------------------------------
  
  # Funcion para almacenar los nombres de columna de todos los archivos
  read_check <- function(file_name, workers) {
    DF = data.table::fread(here::here(file_name), encoding = 'UTF-8', nThread = workers)
    names(DF) %>% as_tibble() %>% mutate(name_file = file_name)
  }
  
  # We read ONE file for each task. Otherwise, with big protocols it takes ages
  one_of_each_file = 
    input_files %>% as_tibble() %>% 
    separate(value, sep = "_", into = c("c1", "c2", "c3", "c4", "c5"), remove = FALSE) %>% 
    group_by(c2) %>% 
    sample_n(1) %>% 
    pull(value)
  
  # Construimos DF global
  DF_final = purrr::map_dfr(one_of_each_file, read_check, workers = workers)
  
  # Numero de archivos
  number_of_files = length(one_of_each_file)
  
  # DF_final %>% write_csv("columns_files.csv")
  
  # Todos los campos deberian aparecer length(files) veces
  DF_columns_files = DF_final %>% count(value)
  
  names_missing_columns = DF_columns_files %>% 
    filter(n != number_of_files) %>% pull(value)
  
  
  # Buscamos en que archivos NO aparecen determinadas columnas
  
  wich_files_missing_columns <- function(name_missing_column, DF_final) {
    
    # cat(crayon::green("\n - Missing column: "), name_missing_column)
    DF_final %>% 
      group_by(name_file) %>% 
      # filter(!"responses" %in% value) %>%
      filter(!name_missing_column %in% value) %>%
      
      ungroup() %>% 
      distinct(name_file) %>% 
      pull(name_file)
    
  }
  
  if (length(names_missing_columns) > 0) {
  check_input_files_DF = 
    1:length(names_missing_columns) %>% 
    map_df(~ wich_files_missing_columns(name_missing_column = names_missing_columns[.x], DF_final) %>% as_tibble() %>% mutate(column_missing = names_missing_columns[.x]) %>% rename(file = value))
  }
  
  # Warning and log ---------------------------------------------------------
  
  if (length(names_missing_columns) > 0) {
    
    write_csv(check_input_files_DF, here::here(paste0("outputs/tests_outputs/test-", name_of_test, ".csv")))
    
    cat(crayon::red("\nERROR in", paste0("test-", name_of_test), "\n"),
        crayon::red("  - Some of the columns are not in all input_files:"), names_missing_columns, "\n",
        crayon::green("  - # of Issues: "), crayon::red(nrow(check_input_files_DF)), "\n",
        crayon::silver("  - DF with details stored in:", paste0("'outputs/tests_outputs/test-", name_of_test, ".csv'"), "\n\n"))
    
  }
  
  
  # Actual expectation -------------------------------------------------------------
  
  testthat::expect_gt(nrow(DF_columns_files), 1) # Checks that we have some rows in the DF
  testthat::expect_length(names_missing_columns, 0)

})
