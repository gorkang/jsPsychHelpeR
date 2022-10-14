extract_tables <- function(list_credentials = NULL, serial_parallel = "serial") {

  # source("admin/mysql_helper_functions.R")
  
  # Credentials
    # TODO: protect credentials!!!
    #sshpass:  -e            Password is passed as env-var "SSHPASS"
  # list_credentials = source(".vault/.credentials")

  if (!is.null(list_credentials)) {
  
  # Open ssh tunnel ---------------------------------------------------------
  
    cli::cli_h1("Openning ssh tunnel")  
    ssh_tunnel = check_start_ssh_tunnel(list_credentials)
    # ssh_tunnel$result$kill()
    # ssh_tunnel$result$get_result()
  
    
  # Extract tables ----------------------------------------------------------
  
  cli::cli_h1("Extracting tables")  
    
  if (serial_parallel == "serial") {
    
    tictoc::tic()
    
    con = openDBconnection(list_credentials)
    
    tables = c("experimental_condition", "user", "user_condition", "user_task", "task", "protocol")
    cli::cli_progress_bar("Extracting tables", total = length(tables), .envir = .GlobalEnv)
    # purrr::walk(tables, get_table_safely, con = con)
    LIST_tables = 
      1:length(tables) |> 
      purrr::map(~ get_table(table_name = tables[.x], con = con)) |> 
      setNames(tables)
    
    DBI::dbDisconnect(con)
    
    tictoc::toc()
    
  } else if (serial_parallel == "parallel") {
    
    # Each parallel job need its own mySQL connection (see inside get_table_parallel())

    tictoc::tic()
    
    # con = openDBconnection(list_credentials)
    
      tables = c("experimental_condition", "user", "user_condition", "user_task", "task", "protocol")
      options(future.rng.onMisuse = "ignore")
      future::plan(future::multisession, workers = length(tables))
      
      LIST_tables = 
        1:length(tables) |> 
        furrr::future_map(~ get_table_parallel(table_name = tables[.x], list_credentials = list_credentials)) |> 
        setNames(tables)
      
    tictoc::toc()

  }

  # Clean up ----------------------------------------------------------
  ssh_tunnel$result$kill()
  # DBI::dbDisconnect(con)

  
  # Output ------------------------------------------------------------------
  return(LIST_tables)
  } else {
    cli::cli_alert_info("Enter password")
  } 
}
