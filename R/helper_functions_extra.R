#' create_formulas
#' It creates the standardized code necessary for the dimensions or total scores calculation in the prepare_TASK scripts
#' PROBABLY NOT NEEDED, as we SHOULD do all this automatically in get_dimensions_google_doc()
#' Maybe useful when there are some non-standard elements to the calculations? e.g. See prepare_CS
#'
#' @param type One of: c("DIRd", "STDd", "RELd", "DIRt", "RELt", "STDt")
#' @param functions One of: c("sum"|"mean")
#' @param dimensions character vector with the dimensions names
#'
#' @return prints the formula for the prepare_TASK() function
#' @export
#'
#' @examples  create_formulas(type = "DIRd", functions = "sum", dimensions = c("ONE"))
create_formulas <- function(type, functions = "sum", dimensions = NULL) {

  if (functions == "sum") {
    functions_str = "rowSums"
  } else if (functions == "mean") {
    functions_str = "rowMeans"
  } else {
    cli::cli_abort(paste0("'functions' should be one of: ", paste(c("sum", "mean"), collapse = ", ")))  
  }
  
  
  if (type %in% c("DIRd", "STDd", "RELd")) {
    
    if (!is.null(dimensions)) {
      
      if (functions == "sum") {
        # OLD: !!names_list$names_dimensions_DIR[1] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))), na.rm = TRUE), 
        # NEW: !!names_list$name_DIRd[1] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR"))), na.rm = TRUE),  
        cat('', paste0('!!names_list$name_', type, '[', 1:length(dimensions), '] := rowSums(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[', 1:length(dimensions), ']], "_DIR"))), na.rm = TRUE),\n'))
      } else if (functions == "mean") {
        cat('', paste0('!!names_list$name_', type, '[', 1:length(dimensions), '] := rowMeans(across(all_of(paste0(short_name_scale_str, "_", items_dimensions[[', 1:length(dimensions), ']], "_DIR"))), na.rm = TRUE),\n'))
      }
      
    } else {
      cli::cli_abort(paste0("'dimensions' should be a character vector similar to c('dimension1', 'dimension2')"))  
      
    }
    
  } else if (type %in% c("DIRt", "RELt", "STDt")) {
    
    if (functions == "sum") {
      cat(paste0('!!name_DIRt := rowSums(across(all_of(paste0(short_name_scale_str, "_", all_items, "_DIR"))), na.rm = TRUE)'))
    } else if (functions == "mean") {
      cat(paste0('!!name_DIRt := rowMeans(across(all_of(paste0(short_name_scale_str, "_", all_items, "_DIR"))), na.rm = TRUE)'))
    }
    
  } else {
    cli::cli_abort(paste0("'type' should be one of: ", paste(c("DIRd", "STDd", "RELd", "DIRt", "RELt", "STDt"), collapse = ", ")))
  }
  
  
}



#' debug_function
#'
#' Loads the parameters used in the functions present in _targets.R to make debugging easier
#'
#' @param name_function name of the function (in _targets.R) for which you want to load the parameters
#'
#' @return Loads in the Global environment the parameters of the function
#' @export
debug_function <- function(name_function) {

  # SEE jsPsychAdmin::get_parameters_of_function()
  # REMEMBER: See jsPsychMonkeys::debug_function()
  
  # DEBUG
  # name_function = "prepare_CRS"

  # Function to tar_load or assign the parameters
  load_parameters <- function(parameters_function_separated, NUM) {
    # NUM = 2
    if (length(parameters_function_separated[[NUM]]) == 1) {
      targets::tar_load(parameters_function_separated[[NUM]], envir = .GlobalEnv)
    } else if (length(parameters_function_separated[[NUM]]) == 2) {
      # assign(parameters_function_separated[[NUM]][1], parameters_function_separated[[NUM]][2], envir = .GlobalEnv)
      assign(parameters_function_separated[[NUM]][1], parameters_function_separated[[NUM]][2], inherits = TRUE)
    }
  }


  # Makes possible to use prepare_TASK or "prepare_TASK"
  if (substitute(name_function) != "name_function") name_function = substitute(name_function) #if (!interactive()) is so substitute do not overwrite name_function when in interactive mode

  # Parses _targets.R
  # TODO: BETTER WAY? (lots of messages)
  # DF_function = tar_manifest() |> filter(grepl(name_function, command)) |> pull(command) |> last()
  
  code <- parse("_targets.R")
  if (file.exists("targets/targets_main.R")) code <- c(code, parse("targets/targets_main.R"))
  # code <- parse("_targets.R")

  # Finds the chunk where name_function is, and cleans the "\"
  text_targets = grep(name_function, code, value = TRUE) %>% gsub("[^A-Za-z0-9\\(\\),_= ]", "", .)

  # Gets and separates then parameters of the function
  parameters_function_raw = gsub(paste0(".*", name_function, "\\((.*?)).*"), "\\1", text_targets) %>% gsub(" ", "", .)

  if (length(parameters_function_raw) > 0) {

    parameters_function_separated = strsplit(parameters_function_raw, ",") %>% unlist() %>% strsplit(., "=")

    # For each of the parameters, applies the load_parameters() function
    TEMP = seq_along(parameters_function_separated) %>% purrr::map(~ load_parameters(parameters_function_separated, NUM = .x))
    cat(cli::col_green("Loaded: "), gsub(",", ", ", parameters_function_raw), "\n")

  } else {

    cat(cli::col_red(paste0("'", name_function, "'", "not found in _targets.R")), "\n")

  }
}




#' Create a new prepare_TASK.R file using prepare_TEMPLATE.R and replacing TEMPLATE by the short name of the new task
#' All new task should be created using this template and then adapted as needed.
#'
#' @param short_name_task short name of the task
#' @param overwrite If the prepare_TASK is already present, should overwrite? FALSE / TRUE
#' @param get_info_googledoc If TRUE, uses `get_dimensions_googledoc()` to get the 
#' task correction information from the jsPsychR GoogleSheets, giving you 
#' pre-prepared code for the different sections  FALSE / TRUE
#' @param destination By default is "R_tasks"
#'
#' @return Creates a R_tasks/prepare_TASK.R using the prepare_TEMPLATE.R
#' @export
create_new_task <- function(short_name_task, overwrite = FALSE, get_info_googledoc = FALSE, destination = "R_tasks") {

  # Create file ---
  new_task_file = paste0(destination, "/prepare_", short_name_task ,".R")

  if (!file.exists(new_task_file) | overwrite == TRUE) {
    
    # Check prepare_TEMPLATE.R exists
    template_file = "R_tasks/prepare_TEMPLATE.R"
    if (!file.exists(template_file)) cli::cli_abort("`{template_file}` does NOT exist! You can download from {.url https://raw.githubusercontent.com/gorkang/jsPsychHelpeR/master/R_tasks/prepare_TEMPLATE.R}")
    
    # Copy template
    cli::cli_alert_info(c("\nCreating new file: ", cli::col_silver(new_task_file), "\n"))
    file.copy(template_file, new_task_file, overwrite = overwrite)
    

    # Replace lines ---
    x <- readLines(new_task_file)
    y <- gsub( "TEMPLATE", short_name_task, x )
    cat(y, file = new_task_file, sep = "\n")

  } else {
    cat(cli::col_yellow("\nFile ", cli::col_silver(new_task_file), " already exists. Not overwriting\n"))
  }


  # get_dimensions_googledoc ---

    if (get_info_googledoc == TRUE) {
      get_dimensions_googledoc(short_name_text = short_name_task)
    }
  

  # Openfile
  rstudioapi::navigateToFile(new_task_file)
  
  
  # OUTPUT ---

    # Line to add in _targets.R
  short_name_old = short_name_task

  # Output messages ---
  cli::cli_h1("ADD the following lines to _targets.R")
  cat(paste0('tar_target(df_', short_name_task, ', prepare_', short_name_task, '(DF_clean, short_name_scale_str = "', short_name_old, '")),\n'))
  cat(cli::col_green("\nDON'T FORGET TO ADD", cli::col_silver(paste0("df_", short_name_task)), "to create_joined() in _targets.R:", cli::col_silver("create_joined(..., ", paste0("df_", short_name_task, ")\n\n"))))

}


#' create_number_series
#' Create a number series from a vector of numbers including individual numbers 
#' and intervals such as c(1, "3-8", 11, "22-24")
#' 
#' @param numbers_RAW A vector of numbers or ranges'
#' @param digits number of digits for the trialid's (3 by default)
#'
#' @return A character vector with numbers preceded by a 0, if needed
#' @export
#'
#' @examples create_number_series(c(1, "3-8", 11, "22-24"))
create_number_series <- function(numbers_RAW, digits = 3) {
  
  # Get rid of spaces
  numbers_RAW_clean = gsub(" ", "", numbers_RAW) #gsub(" - ", "-", numbers_RAW) %>% 
  
  # Get chunks of numbers (separated by "," or ", ")
  numbers_chunks = stringi::stri_extract_all(str = gsub(",|, ", ",", numbers_RAW_clean), regex = "[0-9]{1,3},|[0-9]{1,3}-[0-9]{1,3}|[0-9]{1,3}$") %>% unlist() %>% gsub(",", "", .)
  
  # For each of the chunks in numero_item_dimension_o_sub_escala
  NUMBERS = 1:length(numbers_chunks) |> 
    purrr::map(~ 
          {
            # If there is a "-" create sequence
            if (grepl("-", numbers_chunks[.x])) {
              do.call(what = "seq", args = as.list(stringi::stri_extract_all(numbers_chunks[.x], regex = "[0-9]{1,3}", simplify = FALSE) %>% unlist() %>% as.numeric()))
            } else {
              as.numeric(numbers_chunks[.x])
            }
          }
    ) |>  unlist()
  
  NUMBERS_formatted = sprintf(paste0("%0", digits, "d"), NUMBERS)
  
  if (all(NUMBERS_formatted == "NA")) NUMBERS_formatted = "000"
  
  return(NUMBERS_formatted)
}


#' create_vector_items
#' Create an OR vector for the grepl() and other functions. From c(1,2) to "1|2"
#'
#' @param VECTOR numeric vector
#' @param collapse_string "|"
#'
#' @return A character with the numbers separated by |
#' @export
#'
#' @examples create_vector_items(VECTOR = c( 5, 9, 14, 16, 18))
create_vector_items <- function(VECTOR, collapse_string = "|", digits = 3) {
  
  cat(paste(sprintf(paste0("%0", digits, "d"), VECTOR), collapse = collapse_string))
}



#' number_items_tasks
#' Creates a tibble with information about how many RAW items each task in DF_joined has
#'
#' @param DF_joined DF
#'
#' @return A tibble
#' @export
number_items_tasks <- function(DF_joined) {
  
  items_RAW = 
    tibble::tibble(NAMES = names(DF_joined %>% dplyr::select(matches("_RAW")))) %>% 
    tidyr::separate(NAMES, into = c("test", "item", "type"), extra = "drop", fill = "right")

  items_n = 
    items_RAW %>% 
    dplyr::filter(type == "RAW") %>% 
    dplyr::count(test)
  
  items_info = 
    list(items_RAW = items_RAW,
         items_n = items_n)
  
  return(items_info)
  
}


# update_data() Deprecated by get_zip()



#' Check the project tasks and compare with the csv results to see if there are results or tasks missing
#'
#' @param participants Number of participants
#' @param folder_protocol Folder where the protocol tasks are
#' @param folder_results Folder where the results are
#'
#' @return Message indicating if there are missing data
#' @export
check_project_and_results <- function(participants, folder_protocol, folder_results) {
  
  # DEBUG
  # participants = 5
  # folder_results = "data/999/"
  # folder_protocol = "~/Downloads/protocol999/tasks"
  
  files_protocol = dir(folder_protocol)
  files_results = dir(folder_results)
  
  # If it's a single zip, read files inside # TODO: make more eficient
  files_results = read_csv_or_zip(paste0(folder_results, "/", files_results), only_list = TRUE)
  
  # library(dplyr)
  if (length(files_protocol) / (length(files_results)/participants) == 1)  {
    cat("OK, one task per participant")
    # dir("../jsPsychMaker/canonical_protocol/tasks")
  } else {
    cat(participants, "participants\n")
    cat(length(files_protocol), "tasks\n")
    cat(length(files_results)/participants, "files per participant\n")
  }
  
  experiments_results = 
    tibble::tibble(filename = files_results) %>% 
    parse_filename() |> 
    dplyr::distinct(experiment) %>% 
    dplyr::pull(experiment)
  
  tasks_from_results = paste0(experiments_results, ".js")
  
  missing_experiments = files_protocol[!files_protocol %in% tasks_from_results]
  missing_results = tasks_from_results[!tasks_from_results %in% files_protocol]
  
  if (length(missing_experiments) > 0) cat("Missing task files for: ", missing_experiments)
  if (length(missing_results) > 0) cat("Missing results for: ", gsub("\\.js", "", missing_results))
  
}


#' zip_files
#' zip files of a folder
#'
#' @param folder_files folder to zip
#' @param zip_name name of output zip
#' @param remove_files remove source files TRUE / FALSE
#' @param all_messages show all messages FALSE / TRUE
#'
#' @return NULL
#' @export
zip_files <- function(folder_files, zip_name, remove_files = FALSE, all_messages = TRUE) {
  
  if (remove_files == TRUE){
    if (!grepl("^/tmp/", folder_files)) cli::cli_abort("folder_files should be a subfolder in `/tmp/` when remove_files = TRUE")  
  }
  
  # WIP: Now we append the files if the zip exists
    # If some files were deleted from source but not a previous zip, they remain there

  project_folder = getwd()

  # Make folder_files absolute so list.files will work
  folder_files = normalizePath(folder_files)
  zip_name = paste0(normalizePath(dirname(zip_name)), "/", basename(zip_name))
  
  # Set Temp folder as working folder so the files in zip WONT have the temp path
  setwd(folder_files)
  
  # List all files to zip (exclude zip_name)
  FILES_ZIP_raw = list.files(folder_files, recursive = TRUE, full.names = FALSE, all.files = TRUE, include.dirs = TRUE)
  FILES_ZIP = FILES_ZIP_raw[!grepl(basename(zip_name), FILES_ZIP_raw)]
  
  # Create safely version so an error won't avoid resetting the project's wd
  zip_safely = purrr::safely(utils::zip)
  
  if (length(FILES_ZIP) == 0) {
    if (all_messages == TRUE) cli::cli_alert_danger("NO files found")
  } else {
    # ZIP zilently (flags = "-q")
    RESULT = zip_safely(zipfile = zip_name, files = FILES_ZIP, flags = "-q")
    # Show error
    if (!is.null(RESULT$error)) {
      cli::cli_text(RESULT$error)
    } else {
      cli::cli_alert_success("ZIPPED {length(FILES_ZIP)} files to {fs::path_tidy(gsub(project_folder, '', zip_name))}")
    }
  }
  # Remove temp dir and content
  if (remove_files == TRUE) {
    # file.remove(FILES_ZIP)
    unlink(folder_files, recursive = TRUE)
    if (all_messages == TRUE) cli::cli_alert_success("REMOVED {length(FILES_ZIP)} source files FROM {folder_files}")
  } else {
    if (all_messages == TRUE) cli::cli_alert_info("Will NOT REMOVE {length(FILES_ZIP)} source files FROM {folder_files}")
  }
  
  # Reset the project's WD
  setwd(project_folder)
  
}



#' Get and zip the data, or a full jsPsychMakeR protocol without the data, to keep it as a backup
#' 
#' We use this internal function to save the data of the Running protocols 
#' (according to the Google sheet "Codebook protocolos") to a folder SHARED with 
#' the project's PI's
#'
#' @param pid project id
#' @param what Should be one of c("data", "protocol")
#' @param where Where to leave the zip file with the data. Leave empty to save to `SHARED-data/pid/`
#' @param credentials_file Path to .credentials file. Usually: `.vault/.credentials`
#' @param dont_ask TRUE / FALSE
#' @param ignore_existing If TRUE, does not overwrite existing files even if they are newer. Good for .data/, Bad for rest
#' @param all_messages Show all rsync messages? TRUE / FALSE
#' @param tempdir_location You can choose a tempdir_location (for example, to extract contents of an existing zip and sync only new files)
#' @param file_name Name of zip file. If NULL, the default, will apply simple rules to create name.
#'
#' @return A zip file
#' @export
get_zip <-
  function(pid,
           what,
           where = NULL,
           credentials_file = NULL,
           dont_ask = TRUE,
           ignore_existing = FALSE,
           all_messages = FALSE,
           tempdir_location = NULL,
           file_name = NULL) {
    
  # DEBUG
  # jsPsychAdmin::get_parameters_of_function("jsPsychHelpeR::get_zip()")
  # pid = "999"
  # what = "data"
  # credentials_file = ".vault/.credentials"
  
  # TODO: If no data, do not download!
  
  # Check what is one of the available options
  # what = rlang::arg_match(what)
  if (!exists("what")) cli::cli_abort("parameter `what` missing. Should be `data` or `protocol`" )
  if (!what %in% c("data", "protocol")) cli::cli_abort("`what` should be `data` or `protocol`" )
  
  # Get project's folder to be able ro reset it after zipping
  project_folder = getwd()
  
  # Save version of name (if it is in a subfolder, replace / by _)
  # REVIEW: Works in Windows?
  pid_safe = stringr::str_replace_all(string = pid, pattern = "/", replacement = "_")
  
  
  if (what == "data") {
    
    download_folder = "../SHARED-data/"
    if (!is.null(where)) download_folder = where
    
    server_folder = paste0(pid, "/.data")
    exclude_csv = FALSE
    
    zip_name = here::here(paste0(download_folder, pid, "/", pid_safe, ".zip"))
    if (!is.null(file_name)) zip_name = here::here(paste0(download_folder, pid, "/", file_name))
    
    dir.create(dirname(zip_name), recursive = TRUE, showWarnings = FALSE)
    
  } else if (what == "protocol") {
    
    download_folder = "/data/protocol_"
    if (!is.null(where)) download_folder = where
    
    server_folder = pid
    exclude_csv = TRUE
    
    zip_name = paste0(download_folder, pid_safe, ".zip")
    if (!is.null(file_name)) zip_name = here::here(paste0(download_folder, file_name))
    
  }
  
  
  # Create temp dir to download the protocol
  if (is.null(tempdir_location)) tempdir_location = paste0(tempdir(check = TRUE), "/get_zip/")
  
  OUT = sync_server_local(server_folder = server_folder, 
                          local_folder = tempdir_location,
                          direction = "server_to_local", 
                          only_test = FALSE, 
                          exclude_csv = exclude_csv,
                          delete_nonexistent = TRUE,
                          ignore_existing = ignore_existing,
                          dont_ask = dont_ask, 
                          all_messages = all_messages, 
                          credentials_file = credentials_file)
  
  if (length(OUT) > 0) cli::cli_alert_info(OUT)
        
  # ZIP ---
  zip_files(folder_files = tempdir_location, 
            zip_name = zip_name, 
            remove_files = TRUE, 
            all_messages = all_messages)
  
}



#' create_docker_container
#' Creates a Docker container named gorkang/jspsychhelper:pidPID
#'
#' @param PID project id
#' @param username username
#'
#' @return NULL
#' @export
create_docker_container <- function(PID = 999, username = "gorkang") {
  
  cli::cli_h1("Building container for pid {PID}")
  
  # Read template
  # FROM admin/
  # template = readLines("admin/Dockerfile_TEMPLATE") 
  
  # From jsPsychHelpeR package
  # path <- callr::r(func = find.package, args =  list(package = "jsPsychHelpeR", lib.loc = NULL, quiet = TRUE))
  # Dockerfile_location = list.files(path, recursive = TRUE, pattern = "Dockerfile_TEMPLATE", full.names = TRUE)
  Dockerfile_location = system.file("templates", "Dockerfile_TEMPLATE", package = "jsPsychHelpeR")
  template = readLines(Dockerfile_location)
  
  # Replace PID
  final_file = gsub("999", PID, template)
  
  # Create final file
  cat(final_file, file = "Dockerfile", sep = "\n")
  
  # system(paste0("docker build -t pid", PID, " ."))
  system(paste0("docker build -t ", username, "/jspsychhelper:pid", PID, " ."))
  
  
}


#' clean_renv_cache
#' Gets rid of cache packages not in lib
#'
#' @return Cleans the renv cache
#' @export
clean_renv_cache <- function(lib_location = "renv/lib/R-4.3/x86_64-pc-linux-gnu/", cache_location = "renv/cache/v5/R-4.3/x86_64-pc-linux-gnu")  {
  
  LIB = list.files(lib_location)
  CACHE = list.files(cache_location, full.names = TRUE) |> tibble::as_tibble() |> dplyr::mutate(name = basename(value))
  
  DELETE = CACHE |> dplyr::filter(!CACHE$name %in% LIB)
  # LIB[!LIB %in% CACHE$name]
  
  cli::cli_alert_info("Will delete {nrow(DELETE)} unused packages from cache: {DELETE$name}")
  
  unlink(DELETE$value, recursive = TRUE)
  
}

activate_deactivate_renv <- function(activate_deactivate = "activate") {
  
  renv_activate = readLines(".Rprofile")
  line_renv_activate = which(grepl('source\\(\"renv/activate.R\"\\)', renv_activate))
  if (length(line_renv_activate) != 1) cli::cli_abort('Did not find source("renv/activate.R") in .Rprofile')
  
  # If source("renv/activate.R") is commented out, uncomment
  if (activate_deactivate == "activate"){
    
      renv_activate[line_renv_activate] = 'source("renv/activate.R")'
      writeLines(renv_activate, con = ".Rprofile")

  } else if (activate_deactivate == "deactivate") {
    
    renv_activate[line_renv_activate] = '# source("renv/activate.R")'
    writeLines(renv_activate, con = ".Rprofile")

  }
  
  cli::cli_alert_info('source("renv/activate.R") {activate_deactivate}d. Restarting RStudio')
  rstudioapi::restartSession()
  
}




