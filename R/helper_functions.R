#' standardized_names
#' Standardized names for Direct scores, dimensions and scales
#' Creates names_list, which can be used in the prepare_TASK scripts to create 
#' columns with standardized names for direct scores, total scores, etc.
#' The OLD version created global variables, now we create a list with everything needed
#'
#' @param short_name_scale short name of scale
#' @param dimensions c("DIMENSION1", "DIMENSION2"): list of names for the dimensions
#' @param help_names TRUE prints a message with the instructions
#'
#' @return
#' @export
#'
#' @examples
standardized_names <- function(short_name_scale, dimensions = "", help_names = FALSE) {

  # Global sufix for direct scores
  .GlobalEnv$sufix_DIR = "_DIR"


  # CHECKS ---
  
  # dimensions is a character vector
  if (!(is.vector(dimensions) & is.character(dimensions[1]))) {
    ERROR = paste("\n`dimensions` needs to be a character vector. e.g. c('name1', 'name2')\n",
                  "Right now: `dimensions` = ",   paste0(dimensions), "\n")
    stop(ERROR)
    
    # dimensions names don't have spaces and other forbidden characters
  } else if (any(grepl(" |_", dimensions))) {
    ERROR = paste("\n`dimensions` can't have spaces or '_'. We recommend using CammelCase\n",
                  " - WRONG: c('name dimension', 'name_dimension2')\n",
                  " - RIGHT: c('namedimension', 'NameDimension2')\n",
                  "Right now: `dimensions` = ",   paste0(dimensions), "\n")
    stop(ERROR)
  }


  # Create names for dimensions, NAs and/or totals
  if (dimensions[1] != "") {
    
    # DEBUG
    # help_names = TRUE
    # short_name_scale = "XXX"
    # dimensions = c("dimension1", "dimension2")
    
    # Build strings for DIR, REL and STD
    names_list = list(name_DIRd = paste0(short_name_scale, "_", dimensions, "_DIRd"),
                      name_RELd = paste0(short_name_scale, "_", dimensions, "_RELd"),
                      name_STDd = paste0(short_name_scale, "_", dimensions, "_STDd"),
                      name_RAW_NA = paste0(short_name_scale, "_RAW_NA"),
                      name_DIR_NA = paste0(short_name_scale, "_DIR_NA"),
                      name_STD_NA = paste0(short_name_scale, "_STDt_NA"),
                      name_DIRt = paste0(short_name_scale, "_DIRt"),
                      name_RELt = paste0(short_name_scale, "_RELt"),
                      name_STDt = paste0(short_name_scale, "_STDt"))
    
  } else {
    
    # Build strings for DIR, REL and STD
    names_list = list(name_RAW_NA = paste0(short_name_scale, "_RAW_NA"),
                      name_DIR_NA = paste0(short_name_scale, "_DIR_NA"),
                      name_STD_NA = paste0(short_name_scale, "_STDt_NA"),
                      name_DIRt = paste0(short_name_scale, "_DIRt"),
                      name_RELt = paste0(short_name_scale, "_RELt"),
                      name_STDt = paste0(short_name_scale, "_STDt"))
    
  }
  
  # names_list$names_dimensions_DIR
  
  if (help_names == TRUE){
    cli::cli_alert_info(
      c('To create the code to calculate dimensions or total scores you can use:\n', 
        cli::code_highlight('create_formulas(type = "dimensions_DIR", functions = "sum", items_dimensions)'))
    )
  }
  
  # NEW SYSTEM OUTPUT
  return(names_list)
  
}




#' create_formulas
#' It creates the standardized code necessary for the dimensions or total scores calculation in the prepare_TASK scripts
#' PROBABLY NOT NEEDED, as we SHOULD do all this automatically in get_dimensions_google_doc()
#' Maybe useful when there are some non-standard elements to the calculations? e.g. See prepare_CS
#'
#' @param type One of: c("dimensions_DIR", "dimensions_STD", "dimensions_REL", "total_DIR", "total_REL", "total_STD")
#' @param functions One of: c("sum"|"mean")
#' @param dimensions character vector with the dimensions names
#'
#' @return
#' @export
#'
#' @examples
create_formulas <- function(type, functions = "sum", dimensions = NULL) {
  
  if (functions == "sum") {
    functions_str = "rowSums"
  } else if (functions == "mean") {
    functions_str = "rowMeans"
  } else {
    cli::cli_abort(paste0("'functions' should be one of: ", paste(c("sum", "mean"), collapse = ", ")))  
  }
  
  
  if (type %in% c("dimensions_DIR", "dimensions_STD", "dimensions_REL")) {
    
    if (!is.null(dimensions)) {
      
      if (functions == "sum") {
        # !!names_list$names_dimensions_DIR[1] := rowSums(select(., paste0(short_name_scale_str, "_", items_DIRd1, "_DIR")), na.rm = TRUE), 
        cat('', paste0('!!names_list$names_', type, '[', 1:length(dimensions), '] := rowSums(select(., paste0(short_name_scale_str, "_", items_DIRd', 1:length(dimensions), ', "_DIR")), na.rm = TRUE),\n'))
      } else if (functions == "mean") {
        # !!names_list$names_dimensions_DIR[1] := rowSums(select(., paste0(short_name_scale_str, "_", items_DIRd1, "_DIR")), na.rm = TRUE), 
        cat('', paste0('!!names_list$names_', type, '[', 1:length(dimensions), '] := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd', 1:length(dimensions), ', "_DIR")), na.rm = TRUE),\n'))
      }
      
    } else {
      cli::cli_abort(paste0("'dimensions' should be a character vector similar to c('dimension1', 'dimension2')"))  
      
    }
    
  } else if (type %in% c("total_DIR", "total_REL", "total_STD")) {
    
    if (functions == "sum") {
      cat(paste0('!!name_DIRt := rowSums(select(., paste0(short_name_scale_str, "_", all_items, "_DIR")), na.rm = TRUE)'))
    } else if (functions == "mean") {
      cat(paste0('!!name_DIRt := rowMeans(select(., paste0(short_name_scale_str, "_", all_items, "_DIR")), na.rm = TRUE)'))
    }
    
  } else {
    cli::cli_abort(paste0("'type' should be one of: ", paste(c("dimensions_DIR", "dimensions_STD", "dimensions_REL", "total_DIR", "total_REL", "total_STD"), collapse = ", ")))
  }
  
  
}




##' Checks if the NAs of the RAW calculation are the same as the ones from the PROC calculation
##'
##' Important to catch errors when transforming data from RAW to PROCESSED
##'
##' @title check_NAs
##' @param DF_joined
##' @return
##' @author gorkang
##' @export
check_NAs <- function(DF_joined) {

  DF_CHECK_NA = DF_joined %>%
    select(ends_with("_NA"))

  if (ncol(DF_CHECK_NA) == 2) {

    # Check we have the same number of NAs in RAW and PROC DFs
    if (!identical(DF_CHECK_NA[[1]], DF_CHECK_NA[[2]])) cli::cli_abort(c("Missing data when processing RAW responses:", "-{colnames(DF_CHECK_NA)} have different values"))

    # [REWIEW]: Other ways to check equality
    # all(DF_CHECK_NA[1] == DF_CHECK_NA[2])
    # all.equal(DF_CHECK_NA[[1]], DF_CHECK_NA[[2]])

  } else {

    cli::cli_alert_danger("\n  - Can't perform NA check, DF does not have RAW_NA and DIR_NA columns\n")

  }

}




##' Create long DF for a specific task (short_name_scale)
##'
##'
##'
##' @title create_raw_long
##'
##' @param short_name_scale
##' @param numeric_responses
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
create_raw_long <- function(DF_clean, short_name_scale, numeric_responses = FALSE, is_experiment = FALSE, help_prepare = FALSE) {

  # DEBUG
  # short_name_scale = "FONDECYT"
  # is_experiment = TRUE

  experimental_conditions = "NOTHING_SHOULD_MATCH_THIS"
  if (is_experiment == TRUE) experimental_conditions = "condition_"
  
  DF_output = 
    DF_clean %>%
    
      # OLD system
      # filter(grepl(paste0(short_name_scale, "_[0-9]"), trialid)) %>%
    
      # NEW system
      filter(experimento == short_name_scale) %>%
    
      select(id, experimento, rt, trialid, stimulus, response, starts_with(eval(experimental_conditions), ignore.case = FALSE)) %>%
      mutate(response =
               if (numeric_responses == TRUE) {
                 as.numeric(response)
               } else {
                 as.character(response)
               }
      ) %>%
      drop_na(trialid) %>%
      rename(RAW = response) %>%
      arrange(trialid, id)
  
    # If is experiment, make sure condition_within is fine
    if (is_experiment == TRUE) {
      if (any(grepl("[ -]", DF_output$condition_within))) {
        
        condition_within_options = unique(DF_output$condition_within)
        
        cli_text(col_red("{symbol$cross} "), "Unholy names found in condition_within. Cleaning up. It can take a while...\n")
        
        # NEW WAY: create a diccionary and left_join
        DICCIONARY_within = DF_output %>% 
          distinct(condition_within) %>% 
          rowwise() %>% 
          mutate(condition_withinOK = paste(map_chr(str_split(condition_within, pattern = "_", simplify = TRUE), janitor::make_clean_names, case = "small_camel"), collapse = "_"))
        
        # Show changes
        cli_li(DICCIONARY_within %>% transmute(DIFF = paste0(condition_within, " -> ",  condition_withinOK)) %>% pull(DIFF))
        
        DF_output = 
          DF_output %>% 
          left_join(DICCIONARY_within, by = "condition_within") %>% 
          mutate(condition_within = condition_withinOK) %>% select(-condition_withinOK)
        
        # Very slow
        # DF_output = 
        #   DF_output %>% 
        #   rowwise() %>% 
        #   mutate(condition_within = paste(map_chr(str_split(condition_within, pattern = "_", simplify = TRUE), janitor::make_clean_names, case = "small_camel"), collapse = "_"))
      }
    }
  
  
  if (nrow(DF_output) == 0) stop("No trialid's matching '", short_name_scale, "_[0-9]' found in DF_clean")
  
  if (help_prepare == TRUE) prepare_helper(DF_output, show_trialid_questiontext = TRUE)
  
  return(DF_output)
}




#' debug_function
#'
#' Loads the parameters used in the functions present in _targets.R to make debugging easier
#'
#' @param name_function
#'
#' @return
#' @export
#'
#' @examples
debug_function <- function(name_function) {

  # DEBUG
  # name_function = "prepare_CRS"

  # Function to tar_load or assign the parameters
  load_parameters <- function(parameters_function_separated, NUM) {
    if (length(parameters_function_separated[[NUM]]) == 1) {
      targets::tar_load(parameters_function_separated[[NUM]], envir = .GlobalEnv)
    } else if (length(parameters_function_separated[[NUM]]) == 2) {
      assign(parameters_function_separated[[NUM]][1], parameters_function_separated[[NUM]][2], envir = .GlobalEnv)
    }
  }


  # Makes possible to use prepare_TASK or "prepare_TASK"
  if (substitute(name_function) != "name_function") name_function = substitute(name_function) #if (!interactive()) is so substitute do not overwrite name_function when in interactive mode

  # Parses _targets.R
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
    TEMP = seq_along(parameters_function_separated) %>% map(~ load_parameters(parameters_function_separated, NUM = .x))
    cat(cli::col_green("Loaded: "), gsub(",", ", ", parameters_function_raw), "\n")

  } else {

    cat(cli::col_red(paste0("'", name_function, "'", "not found in _targets.R")), "\n")

  }
}



##' Save files
##'
##'
##'
##' @title create_raw_wide
##' @param DF
##' @param short_name_scale
##' @param is_scale = TRUE
##' @param is_sensitive = TRUE
##' @return
##' @author gorkang
##' @export
save_files <- function(DF, short_name_scale, is_scale = TRUE, is_sensitive = FALSE, output_formats = c("csv", "csv2")) {

  # Select path based on nature of the data
  if (is_sensitive == TRUE) {
    data_path = ".vault/outputs/data/"
  } else {
    data_path = "outputs/data/"
  }
  

  implemented_formats = c("csv", "csv2", "rds")
  if (!all(output_formats %in% implemented_formats)) cli::cli_abort("output_formats should be one of {.code {implemented_formats}}")
  
  # Save data. Use "df_" if it's a scale otherwise use "DF_"
  if (is_scale == TRUE) {

    if ("csv" %in% output_formats) write_csv(DF, here::here(paste0(data_path, "df_", short_name_scale , ".csv")))
    if ("csv2" %in% output_formats) write_csv2(DF, here::here(paste0(data_path, "df_", short_name_scale , "_sp.csv")))
    if ("rds" %in% output_formats) write_rds(DF, here::here(paste0(data_path, "df_", short_name_scale , ".rds")))

  } else {

    if ("csv" %in% output_formats) write_csv(DF, here::here(paste0(data_path, "DF_", short_name_scale , ".csv")))
    if ("csv2" %in% output_formats) write_csv2(DF, here::here(paste0(data_path, "DF_", short_name_scale , "_sp.csv")))
    if ("rds" %in% output_formats) write_rds(DF, here::here(paste0(data_path, "DF_", short_name_scale , ".rds")))

  }

}



#' helper to correct tasks
#'
#' @param DF_long_RAW
#' @param show_trialid_questiontext [TRUE/FALSE]
#'
#' @return
#' @export
#'
#' @examples
prepare_helper <- function(DF_long_RAW, show_trialid_questiontext = FALSE) {

  # DF_long_RAW created by create_raw_long()
  # DF_long_RAW = DF_output
  
  # Items
  vector_items = DF_long_RAW %>% distinct(trialid) %>% arrange(trialid) %>% pull(trialid)

  # Questions
  DF_question = DF_long_RAW %>% distinct(trialid, stimulus) #%>%  print(n = Inf)

  # Responses
  vector_responses = DF_long_RAW %>% distinct(RAW) %>% pull(RAW)
  if (is.numeric(vector_responses)) {
    vector_responses = str_sort(vector_responses, numeric = TRUE)
  } else {
    vector_responses = str_sort(vector_responses, numeric = FALSE)
  }
  DF_trialid_alternativeresponses = DF_long_RAW %>% count(trialid, RAW) %>% count(trialid, name = "alternative responses")
  DF_check_responses = DF_trialid_alternativeresponses %>% count(`alternative responses`, name = "number of items") %>% arrange(desc(`number of items`))
  
  DF_responses =
    DF_long_RAW %>%
    count(trialid, RAW) %>%
    group_by(trialid) %>%
    summarise(N = n(),
              Responses = paste(RAW, collapse = ", "),
              .groups = "drop") %>%
    group_by(Responses) %>%
    summarise(N = n(),
              trialid = paste(trialid, collapse = ", "),
              .groups = "drop") 

  if (show_trialid_questiontext == TRUE) {
    cli::cli_h1("trialid and stimulus")
    # cli::cli_alert_info("Showing trialid and stimulus for all the items: ")
    DF_question %>% print(n = Inf)
  }

  if (nrow(DF_check_responses) > 1) {
    cli::cli_h1("Responses per item")
    cat("\n", cli::col_blue(nrow(DF_check_responses), " Different number of responses per item: \n"))
    print(DF_check_responses)
  }
  
  cli::cli_h1("Items and responses")
  cat(cli::col_blue("\n", length(vector_items), " Items:"), cli::col_silver(paste0("'", vector_items[c(1,length(vector_items))], "'", collapse = " to ")), "\n")
  cat(cli::col_blue("\n", length(vector_responses), " distinct Responses:"), cli::col_silver("See Viewer pane..."), "\n")

  # Output the DT table (need to print because this function is called inside another function)
  DF_responses %>% DT::datatable() %>% print()

}



#' create_new_task
#' Create a new prepare_TASK.R file from prepare_TEMPLATE.R replacing TEMPLATE by the short name of the new task
#'
#' @param short_name_task 
#' @param overwrite 
#'
#' @return
#' @export
#'
#' @examples
create_new_task <- function(short_name_task, overwrite = FALSE, get_info_googledoc = FALSE, destination = "R_tasks") {

  # DEBUG
  # short_name_task = "CS"
  # overwrite = FALSE
  # get_info_googledoc = TRUE
  
  # Needed in case someone launches the function directly (%>%)
  library(dplyr)

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
#' @param numbers_RAW 
#'
#' @return
#' @export
#'
#' @examples
create_number_series <- function(numbers_RAW) {
  
  # numbers_RAW = c(1, "3-8", 11, "22-24")
  
  # Get rid of spaces
  numbers_RAW_clean = gsub(" ", "", numbers_RAW) #gsub(" - ", "-", numbers_RAW) %>% 
  
  # Get chunks of numbers (separated by "," or ", ")
  numbers_chunks = stringi::stri_extract_all(str = gsub(",|, ", ",", numbers_RAW_clean), regex = "[0-9]{1,3},|[0-9]{1,3}-[0-9]{1,3}|[0-9]{1,3}$") %>% unlist() %>% gsub(",", "", .)
  
  # For each of the chunks in numero_item_dimension_o_sub_escala
  NUMBERS = 1:length(numbers_chunks) %>% 
    purrr::map(~ 
          {
            # If there is a "-" create sequence
            if (grepl("-", numbers_chunks[.x])) {
              do.call(what = "seq", args = as.list(stringi::stri_extract_all(numbers_chunks[.x], regex = "[0-9]{1,3}", simplify = FALSE) %>% unlist() %>% as.numeric()))
            } else {
              as.numeric(numbers_chunks[.x])
            }
          }
    ) %>% unlist()
  
  NUMBERS_formatted = sprintf("%02d", NUMBERS)
  
  if (all(NUMBERS_formatted == "NA")) NUMBERS_formatted = "000"
  
  return(NUMBERS_formatted)
}


#' create_vector_items
#' Create an OR vector for the grepl() and other functions. From c(1,2) to "1|2"
#'
#' @param VECTOR 
#'
#' @return
#' @export
#'
#' @examples
create_vector_items <- function(VECTOR, collapse_string = "|") {
  # VECTOR = c( 5, 9, 14, 16, 18)
  cat(paste(sprintf("%02d", VECTOR), collapse = collapse_string))
}


#' Create _targets.R file from a protocol folder
#'
#' @param pid_protocol 
#' @param folder_data 
#' @param folder_tasks 
#'
#' @return
#' @export
#'
#' @examples
create_targets_file <- function(pid_protocol = 0, folder_data = NULL, folder_tasks = NULL) {

  # DEBUG
  # folder_tasks = "/home/emrys/Downloads/COURSE/gorkang-jsPsychMaker-d94788a/canonical_protocol/tasks/"
  # folder_data = "data/999"
  # folder_tasks = NULL
  # pid_protocol = "999"

  suppressPackageStartupMessages(library(dplyr))
  
  if (file.exists("_targets_automatic_file.R")) {
    cli::cli_alert_info("Deleting OLD _targets_automatic_file.R")
    file.remove("_targets_automatic_file.R")
  }
  
  
  # If both parameters have info, choose folder_data
  if (!is.null(folder_data) & !is.null(folder_tasks)) {
    cli::cli_alert_info("Both 'folder_data' and 'folder_tasks' have information. Using 'folder_data'\n")
    folder_tasks = NULL
  } 
  
  # List js files or parse data files
  if (is.null(folder_data) & !is.null(folder_tasks)) {
    
    files = gsub(".js", "", basename(list.files(folder_tasks, recursive = FALSE, pattern = ".js")))
    
  } else if (!is.null(folder_data) & is.null(folder_tasks)) {
    
    input_files = list.files(path = folder_data, pattern = "*.csv|*.zip", full.names = TRUE)

    # Extract all unique tasks
    files = read_csv_or_zip(input_files) %>% distinct(procedure) %>% pull(procedure)

  }
  
  if (length(files) > 0) {
    
    # Read template
    template = readLines("targets/_targets_TEMPLATE.R")
    
    # Prepare targets section and joins section
    targets = paste0("   tar_target(df_", files, ", prepare_", files, "(DF_clean, short_name_scale_str = '", files,"')),\n") %>% paste(., collapse = "")
    joins = paste0("\t\t\t\t\t\t\t df_", files, ",\n") %>% paste(., collapse = "") %>% gsub(",\n$", "", .)
  
    # Replace targets and joins sections 
    final_targets = gsub("#TARGETS_HERE", targets, template)
    final_joins = gsub("#JOINS_HERE", joins, final_targets)
    
    # Find and replace pid_target
    line_pid_target = which(grepl("pid_target = '999'", final_joins))
    if (length(line_pid_target) == 0) cli::cli_abort("Can´t find `pid_target = '999'` in _targets_TEMPLATE.R") # Check we find the string to replace
    final_joins[line_pid_target] = paste0("\tpid_target = '", pid_protocol, "'")
    final_file = final_joins
    # final_file = gsub("pid_target = '999'", paste0("pid_target = '", pid_protocol, "'"), final_joins) # OLD method
    # cat(final_joins, sep = "\n")
  
    # Create final file
    cat(final_file, file = "_targets_automatic_file.R", sep = "\n")
    
  } else {
  
    cli::cli_alert_danger("0 tasks found for protocol '{pid_protocol}'. NOT creating `_targets.R` file")  

  }
  
  # If previous step was successful
  if (file.exists("_targets_automatic_file.R")) {

    response_prompt = menu(choices = c("YES", "No"), 
                           title = cli_message(var_used = files,
                                               info = "{cli::style_bold((cli::col_yellow('Overwrite')))} file '_targets.R' to include the following {length(files)} tasks?", 
                                               details = "{.pkg {files}}")
                           )
    
    if (response_prompt == 1) {
      
      # Create Backup file
      if (file.exists("_targets.R")) file.rename(from = "_targets.R", to = "_targets_old.R")
      
      # RENAME _targets_automatic_file.R as _targets.R. _targets_automatic_file.R was created in the previous step
      file.rename(from = "_targets_automatic_file.R", to = "_targets.R")
      
      # DELETE UNUSED tasks
      TASKS_TO_DELETE_raw = 
        list.files("R_tasks/") %>% 
        as_tibble() %>% 
        mutate(task = gsub("prepare_(.*)\\.R", "\\1", value)) %>% 
        filter(!task %in% files & !grepl("\\.csv", value)) %>% 
        pull(value) 
      
      if(length(TASKS_TO_DELETE_raw) > 0) {
        
        TASKS_TO_DELETE = TASKS_TO_DELETE_raw %>% paste0("R_tasks/", .)
        
        delete_prompt = menu(choices = c("Yes", "NO"), 
                             title = cli_message(var_used = TASKS_TO_DELETE,
                                                 h1_title = "Clean up",
                                                 info = "{cli::style_bold((cli::col_red('DELETE')))} the following {length(TASKS_TO_DELETE)} unused tasks from 'R_tasks/'?:",
                                                 details = "{.pkg {basename(TASKS_TO_DELETE)}}")
        )
        
        if (delete_prompt == 1) {
          cli::cli_alert_info("Zipping unused tasks to create backup")
          zip(zipfile = "outputs/backup/deleted_tasks.zip", files = TASKS_TO_DELETE, flags = "-q") # Silent flags = "-q"
          file.remove(TASKS_TO_DELETE)
          cli::cli_alert_success(paste0("Deleted ", length(TASKS_TO_DELETE), " unused tasks. Backup in `outputs/backup/deleted_tasks.zip`"))
        }
      }
      
      # END Messages
      cli_message(h1_title = "All done", 
                  success = "NEW '_targets.R' created",
                  details = cli::col_grey("Use the following commands to start the data preparation:"),
                  list = c("Visualize pipeline: {.code targets::tar_visnetwork()}",
                           "Delete cache: {.code targets::tar_destroy()}", 
                           "Start data preparation: {.code targets::tar_make()}")
                  )
    
    } else {
      cli::cli_alert_warning("OK, nothing done\n")
    }
    
  }

}


#' auto_reliability
#'
#' Checks Cronbach's alpha and select variables based on a min r.drop criteria
#'
#' @param DF
#' @param items
#' @param min_rdrop
#'
#' @return
#' @export
#'
#' @examples
auto_reliability = function(DF, short_name_scale = short_name_scale_str, items = NULL, min_rdrop = 0.2) {

  # DEBUG
  # DF = DF_wide_RAW
  # short_name_scale = short_name_scale_str
  # items =  items_DIRd1
  # min_rdrop = 0.2
  # items =  NULL
  

  # Internal functions
  alpha_table <- function(DF) {psych::alpha(DF, check.keys = TRUE)$item.stats %>% as_tibble(rownames = "nitem")}
  safely_alpha_table = safely(alpha_table)
  alpha_raw <- function(DF) {psych::alpha(DF, check.keys = TRUE)$total$raw_alpha}
  safely_alpha_raw = safely(alpha_raw)


  # Create items_selection vector
  if (is.null(items)) {
    items_selection = DF %>% select(matches("DIR$")) %>% names(.)
  } else {
    items_selection = paste0(short_name_scale, "_", items, "_DIR")
  }

  
  # Get selected variables
  temp_clean_RAW = DF %>%
    dplyr::select(all_of(items_selection))

  temp_clean = temp_clean_RAW %>%
    select_if(~ !any(is.na(.))) # Need to delete columns with any NA

  
  # CHECK variance
  variance_WARNING = ifelse(all(1:ncol(temp_clean) %>% map_dbl(~ sd(temp_clean[.x] %>% unlist)) == 0), "No variance", NA) 
  if (!is.na(variance_WARNING)) cli::cli_alert_danger("NO variance in items (sd = 0): \n{paste(items_selection, collapse = '; ')}")
  
  
  # deleted_items_NAs
  deleted_items_NAs = paste(names(temp_clean_RAW)[!names(temp_clean_RAW) %in% names(temp_clean)])
  if (length(deleted_items_NAs) > 0) cat(cli::col_red("x DELETED VARS (have NA's): ", paste(deleted_items_NAs, collapse = ", "), "\n"))

  # Filter items where r.drop < min_rdrop
  delete_items_raw = suppressMessages(safely_alpha_table(temp_clean))
  if (!is.null(delete_items_raw$result)) {
    delete_items = delete_items_raw$result %>% select(nitem, r.drop) %>% filter(r.drop <= min_rdrop) %>% pull(nitem)
    delete_items_warnings = delete_items_raw$warnings
  } else {
    delete_items_warnings = NULL
  }

  if (exists("delete_items") == FALSE) delete_items = 0
  if (length(delete_items) == 0) {

    temp_clean_alpha_raw = suppressMessages(safely_alpha_raw(temp_clean)$result)
    if (!is.null(temp_clean_alpha_raw)) {
      alpha_initial = round(temp_clean_alpha_raw, 3)
    } else {
      alpha_initial = NULL
    }
    alpha_final = NULL
    delete_items = NULL
    keep_items = names(temp_clean)

    cat(cli::col_green("No items with r.drop <= ", min_rdrop), "|| alpha: ", alpha_initial, "\n")

  } else {

    # Select items that won't be deleted
    keep_items = names(temp_clean[,!(names(temp_clean) %in% delete_items)])

    # Temp DF with selected items
    temp_seleccionados = temp_clean %>% dplyr::select(all_of(keep_items))

    # Alphas
    temp_alpha_initial_raw = suppressMessages(safely_alpha_raw(temp_clean)$result)
    if (!is.null(temp_alpha_initial_raw)) {
      alpha_initial = round(temp_alpha_initial_raw, 3)
    } else {
      alpha_initial = NULL
    }
    
    temp_alpha_final_raw = suppressMessages(safely_alpha_raw(temp_seleccionados)$result)
    if (!is.null(temp_alpha_final_raw)) {
      alpha_final = round(temp_alpha_final_raw, 3)
    } else {
      alpha_final = NULL
    }
    
    cat(cli::col_yellow("Filtered ", paste0(length(delete_items), "/", ncol(temp_clean_RAW)), " items with r.drop <= ", min_rdrop), "|| alpha: ", alpha_initial , "->", alpha_final, "\n")


  }

  item_selection_string = gsub(".*([0-9]{2,3}).*", "\\1", keep_items)

  reliability_output =
    list(min_rdrop = min_rdrop,
         alpha_initial = alpha_initial,
         alpha_final = alpha_final,
         delete_items = delete_items,
         keep_items = keep_items,
         delete_items_warnings = delete_items_warnings,
         item_selection_string = item_selection_string)

  write_rds(reliability_output, paste0("outputs/reliability/", short_name_scale, ".rds"))

  return(reliability_output)

}


#' show_progress_pid
#'
#' Show progress of data collection of a project
#'
#' @param pid
#' @param files_vector
#' @param last_task
#' @param goal
#'
#' @return
#' @export
#'
#' @examples
show_progress_pid <- function(pid = 3, files_vector, last_task = "Goodbye", goal = 100, DEBUG = FALSE) {
  
  # DEBUG
  # pid = 3
  # files_vector = files_3
  # last_task = "Goodbye"
  # goal = 100
  # DEBUG = TRUE
  
  # Prepare data ---
  
  # Read files in csv or zip
  DF_files = read_csv_or_zip(files_vector) |> 
    tidyr::separate(col = filename,
                    into = c("project", "experimento", "version", "datetime", "id"),
                    sep = c("_"), remove = FALSE) %>%
    mutate(id = gsub("(*.)\\.csv", "\\1", id))
  
  
  DF_progress =
    DF_files %>%
    filter(experimento == last_task) %>%#"COVIDCONTROL"
    distinct(id, .keep_all = TRUE) %>%
    distinct(filename, datetime) %>%
    mutate(fecha_registro = as.Date(datetime)) %>%
    count(fecha_registro, name = "numero_registros") %>%
    ungroup() %>%
    mutate(suma_total = cumsum(numero_registros),
           days_since_start = as.integer(fecha_registro - min(fecha_registro)),
           rate = round(suma_total / days_since_start, 2)) %>% 
    arrange(desc(fecha_registro))
  
  
  # Plot ---
  days = difftime(max(DF_progress$fecha_registro), min(DF_progress$fecha_registro), units = "days")
  if (days > 20) {
    date_breaks = "weeks"
  } else {
    date_breaks = "days"
  }
  
  
  PLOT_progress =
    DF_progress %>%
    ggplot(aes(fecha_registro, suma_total)) +
    geom_line() +
    geom_bar(aes(fecha_registro, numero_registros), stat = "identity", alpha = .5) +
    geom_point() +
    geom_hline(yintercept = goal, linetype = "dashed", color = "grey") +
    theme_minimal(base_size = 16) +
    scale_x_date(date_breaks = date_breaks, guide = guide_axis(angle = 90)) +
    scale_y_continuous(n.breaks = 10) +
    labs(title = paste0("Protocolo " , pid, " completado"),
         subtitle = paste0("Ultimo dato: ", as.Date(max(DF_files$datetime))),
         caption = paste0("Last test used: ", last_task, ". Goal: ", goal, " participants"), x = "")
  
  
  # Calculate output vars
  suma_total_ahora = max(DF_progress$suma_total)
  suma_total_yesterday = DF_progress %>% filter(fecha_registro <= Sys.Date() - 1) %>% pull(suma_total) %>% first()
  suma_total_lastweek = DF_progress %>% filter(fecha_registro <= Sys.Date() - 7) %>% pull(suma_total) %>% first()
  if (is.na(suma_total_lastweek)) suma_total_lastweek = 0
  DIFF_yesterday = suma_total_ahora - suma_total_yesterday
  DIFF_lastweek = suma_total_ahora - suma_total_lastweek
  DAYS = as.integer(max(DF_progress$fecha_registro) - min(DF_progress$fecha_registro))
  rate_per_day = round(suma_total_ahora/DAYS, 1)
  rate_yesterday = round(DIFF_yesterday / 1, 1)
  rate_lastweek = round(DIFF_lastweek / 7, 1)
  days_to_goal = round((goal - suma_total_ahora)/rate_per_day, 0)
  days_to_goal_yesterdayrate = round((goal - suma_total_ahora)/rate_yesterday, 0)
  days_to_goal_lastweekrate = round((goal - suma_total_ahora)/rate_lastweek, 0)
  
  
  TABLE = tibble(last_n_days = c(1, 7, DAYS),
                 new_participants = c(DIFF_yesterday, DIFF_lastweek, suma_total_ahora),
                 participants_x_day = c(rate_yesterday, rate_lastweek , rate_per_day),
                 days_to_goal = c(days_to_goal_yesterdayrate, days_to_goal_lastweekrate, days_to_goal))  
  
  
  # OUTPUT ---
  
  if (DEBUG == TRUE) {
    
    cat(cli::col_green(
      paste0("Data collection started in ", min(DF_progress$fecha_registro), ". Our goal is ", goal, " participants.\n",
             "In the last [1 / 7 / ", DAYS, "] days we got [+", DIFF_yesterday, " / +", DIFF_lastweek, " / +", suma_total_ahora, "] new people.\n",
             "In the last [1 / 7 / ", DAYS, "] days we got [", rate_yesterday, " / ", rate_lastweek ," / ", rate_per_day, "] people per day.\n",
             "Will take [", days_to_goal_yesterdayrate, " / ", days_to_goal_lastweekrate ," / ", days_to_goal, "] more days to reach the goal.")))
    
  }
  
  OUTPUT = list(TABLE = TABLE, 
                DF_files = DF_files,
                DF_progress = DF_progress,
                PLOT_progress = PLOT_progress)
  
  return(OUTPUT)
  
}




#' separate_responses
#' 
#' Creates one line per response 
#'
#' @param DF 
#'
#' @return
#' @export
#'
#' @examples
separate_responses <- function(DF) {
  
  # DEBUG
  # DF = DF_clean_raw #%>% filter(experimento == "MLQ")
  
  # How many different N of responses we have
  different_N = DF %>% 
    # mutate(N_responses = str_count(response, '\\"".*?"":""')) %>% 
    mutate(N_responses = str_count(response, '\\"".*?"":')) %>% 
    pull(N_responses) %>% unique()
  
  # Internal function to separate   
  separate_N <- function(N) {
    
    # N = 4
    
    # When there is no response recorded (e.g. INFCONS)
    if (is.na(N)) {
      
      DF %>% 
        # mutate(N_responses = str_count(response, '\\"".*?"":""')) %>% 
        mutate(N_responses = str_count(response, '\\"".*?"":')) %>% 
        filter(is.na(N_responses)) %>% 
        mutate(question = "Q0",
               response_raw = response)
      
      # N == 0 should not exist in canonical jsPsych
    } else if (N == 0) {
      
      DF %>% 
        # mutate(N_responses = str_count(response, '\\"".*?"":""')) %>% 
        mutate(N_responses = str_count(response, '\\"".*?"":')) %>% 
        filter(N_responses == N) %>% 
        mutate(question = "Q0",
               response_raw = response)
      
      
      # For any number of responses
    } else {
      
      pattern_names = 1:N %>% map(~ c(paste0("qid", .x), paste0("resp", .x))) %>% unlist()
      # pattern_template_raw = '\\""(.*?)"":\\""(.*?)""'
      pattern_template_raw = '\\""(.*?)"":(.*)'
      pattern_template = paste(rep(pattern_template_raw, N), collapse = ",")
      
      DF_temp = 
        DF %>% 
        # mutate(N_responses = str_count(response, '\\"".*?"":""')) %>% 
        mutate(N_responses = str_count(response, '\\"".*?"":')) %>% 
        filter(N_responses == N) %>% 
        tidyr::extract(response, pattern_names, regex = pattern_template, remove = FALSE) %>% 
        rename(response_raw = response) 
      
      names_qid = DF_temp %>% select(matches("qid[0-9]{1,3}")) %>% names()
      names_resp = DF_temp %>% select(matches("resp[0-9]{1,3}")) %>% names()
      values_qid = DF_temp %>% select(matches("qid[0-9]{1,3}")) %>% pivot_longer(everything()) %>% distinct(value) %>% pull(value)
      
      1:length(names_qid) %>% 
        map_df(~ DF_temp %>% pivot_wider(names_from = names_qid[.x], values_from = names_resp[.x])) %>% 
        select(-matches("qid[0-9]{1,3}"), -matches("resp[0-9]{1,3}")) %>% 
        pivot_longer(all_of(values_qid), names_to = "question", values_to = "response") %>%
        drop_na(response) %>% 
        mutate(response = gsub('\\"|}', '', response))
      
      
    }
  }
  
  # Apply separate_N() to all the different_N's 
  DF_out = 
    map_df(different_N, separate_N) %>% 
    mutate(trialid = 
             case_when(
               question == "Q0" ~ trialid,
               question == "" ~ trialid,
               is.na(question) ~ trialid,
               TRUE ~ paste0(trialid, "_", question)
             ))
  
  return(DF_out)
  
}


#' number_items_tasks
#' Creates a tibble with information about how many RAW items each task in DF_joined has
#'
#' @param DF_joined 
#'
#' @return
#' @export
#'
#' @examples
number_items_tasks <- function(DF_joined) {
  
  items_RAW = 
    tibble(NAMES = names(DF_joined %>% select(matches("_RAW")))) %>% 
    separate(NAMES, into = c("test", "item", "type"), extra = "drop", fill = "right")

  items_n = 
    items_RAW %>% 
    filter(type == "RAW") %>% 
    count(test)
  
  items_info = 
    list(items_RAW = items_RAW,
         items_n = items_n)
  
  return(items_info)
  
}



#' update_data
#' Download 'data/id_protocol' folder from server using rsync
#'
#' @param id_protocol 
#' @param sensitive_tasks 
#' @param folder_to_download ["data" | "script"] to download data or full script
#'
#' @return
#' @export
#'
#' @examples
update_data <- function(id_protocol, sensitive_tasks = c(""), folder_to_download = "data") {
  
  # DEBUG
  # id_protocol = 22
  # sensitive_tasks = c("DEMOGR")

  # CHECKS --
  
  credentials_exist = file.exists(".vault/.credentials")
  SSHPASS = Sys.which("sshpass") # Check if sshpass is installed
  RSYNC = Sys.which("rsync") # Check if rsync is installed
  
  if (!dir.exists(paste0(getwd(), '/', folder_to_download, '/' , id_protocol, '/'))) dir.create(paste0(getwd(), '/', folder_to_download, '/' , id_protocol, '/'))
  # if (!dir.exists(paste0("data/", id_protocol, "/"))) stop("CAN'T find the destination folder: 'data/", id_protocol, "'")
  
  if (!credentials_exist) {
    cli::cli_abort(
      c("The file '.vault/.credentials' does NOT exist. Follow the steps in: ", 
        'rstudioapi::navigateToFile("setup/setup_server_credentials.R")'))
  }
   
  if (SSHPASS != "" & RSYNC != "") { 
    
    if (folder_to_download == "data") {
      server_folder_to_download = ".data"
    }
    
      # Download files --
    
      cli::cli_alert_info("Synching files from pid {id_protocol} to 'data/{id_protocol}'")
      
      WD = gsub(" ", "\\ ", getwd(), fixed = TRUE) # Replace " " in path to avoid error
      list_credentials = source(".vault/.credentials")
      result = system(paste0('sshpass -p ', list_credentials$value$password, 
                    ' rsync -av --rsh=ssh ', 
                    # From
                    list_credentials$value$user, "@", list_credentials$value$IP, ":", list_credentials$value$main_FOLDER, id_protocol, '/', server_folder_to_download, '/ ', 
                    # To
                    WD, '/', folder_to_download, '/' , id_protocol, '/'), 
                    intern = TRUE)
      
      if (length(result) == 4) {
        cli::cli_alert_info("All files already in 'data/{id_protocol}'")
      } else {
        cli::cli_alert_success("Downloaded {length(result) - 5} files to 'data/{id_protocol}'")
        
        destination_folder = paste0(WD, '/', folder_to_download, '/' , id_protocol, '/')
        files_destination = list.files(destination_folder)
        cli::cli_alert_info("{length(files_destination)} files in 'data/{id_protocol}'")
      }
      
    
      # Move sensitive files to .vault/data --
    
      if (sensitive_tasks != "") {
        # MOVE sensitive data to .vault
        data_folder = paste0("data/", id_protocol)
        sensitive_files = list.files(data_folder, pattern = paste(sensitive_tasks, collapse = "|"), full.names = TRUE)
        
        destination_folder = paste0(".vault/data")
        destination_names = gsub(data_folder, destination_folder, sensitive_files)
        file.rename(from = sensitive_files, to = destination_names)
        
        cli::cli_alert_success("Moved {length(destination_names)} files matching '{paste(sensitive_tasks, collapse = '|')}' to '{destination_folder}'")
        
      }
  } else {
    
    cli::cli_alert_danger("'sshpass' or 'rsync' not installed. Can't use `update_data()`")
    cli::cli_alert_info("You need to manually download the files to '{paste0('data/', pid, '/')}'")
    
  }
  
}

#' create_codebook
#' Extracts information from the prepare_TASK.R files to create a codebook
#'
#' @param number 
#'
#' @return
#' @export
#'
#' @examples
create_codebook <- function(tasks, number) {
  
  # TODO ---
  # - Add totals
  
  
  # DEBUG
  # number = 51
  
  DF = readLines(tasks[number])
  
  DF_clean = DF[!grepl("^[ \t\\#]{1,100}\\#", DF)] # Get rid of comments
  
  lines_DIRd = DF_clean[grepl("!!name_DIRd", DF_clean)]
  
  # Task
  name_task = gsub("prepare_|\\.R", "", basename(tasks)[number])
  
  # Descriptions
  description_task = gsub('.*description_task \\= \\"(.*)\\"', "\\1", DF_clean[grepl("description_task", DF_clean)])
  description_dimensions = stringr::str_extract_all(DF_clean[grepl("description_dimensions =", DF_clean)], '"[\\w_ ]{1,50}"') %>% purrr::compact()
  
  # Total
  function_DIRt = gsub(".*!!name_DIRt := (\\w{1,10}).*", "\\1", DF_clean[grepl("!!name_DIRt", DF_clean)]) %>% head(1) # REVIEW INFCONS
  
  # Dimensions
  names_dimensions = stringr::str_extract_all(DF_clean[grepl("names_dimensions =", DF_clean)], '"[\\w_]{1,50}"') %>% purrr::compact() %>% unlist() %>% gsub('\"', "",.)
  # num_dimensions = stringr::str_extract_all(DF_clean[grepl("items_DIRd[0-9] =", DF_clean)], "items_DIRd[0-9]{1,2}")
  items_dimensions = stringr::str_extract_all(DF_clean[grepl("items_DIRd[0-9] =", DF_clean)], "[0-9]{2,3}_[\\w]{1,20}|[0-9]{2,3}") %>% purrr::compact()
  functions_DIRd = gsub(".*!!name_DIRd[0-9] := (\\w{1,10}).*", "\\1", lines_DIRd)
  
  items_reversed = stringr::str_extract_all(DF_clean[grepl("items_to_reverse =", DF_clean)], '"[\\w_]{1,50}"') %>% purrr::compact() %>% unlist() %>% gsub('\"', "",.) %>% unique() %>% paste(., collapse = ", ")
  items_ignored = stringr::str_extract_all(DF_clean[grepl("items_to_ignore =", DF_clean)], '"[\\w_]{1,50}"') %>% purrr::compact() %>% unlist() %>% gsub('\"', "",.) %>% unique() %>% paste(., collapse = ", ")
  
  
  
  # Dimensions OLD STRUCTURE
  old_names_dimensions =  stringr::str_extract_all(DF_clean[grepl("dimensions =", DF_clean)], '"[\\w_]{1,50}"') %>% purrr::compact() %>% unlist() %>% gsub('\"', "",.)
  old_items_dimensions = stringr::str_extract_all(lines_DIRd, "[0-9]{2,3}_[\\w]{1,20}|[0-9]{2,3}") %>% purrr::compact()
  
  
  # CHECK
  if (length(function_DIRt) == 0) function_DIRt = ""
  if (length(functions_DIRd) == 0) functions_DIRd = ""
  if (length(names_dimensions) == 0) names_dimensions = ""
  if (length(items_dimensions) == 0) items_dimensions = ""
  if (length(items_reversed) == 0) items_reversed = ""
  if (length(items_ignored) == 0) items_ignored = ""
  if (length(old_names_dimensions) == 0) old_names_dimensions = ""
  if (length(old_items_dimensions) == 0) old_items_dimensions = ""
  
  if (length(description_task) == 0) description_task = ""
  if (length(description_dimensions) == 0) description_dimensions = ""
  
  
  if (names_dimensions[1] == "") names_dimensions = old_names_dimensions
  if (items_dimensions[1] == "") items_dimensions = old_items_dimensions
  
  # Build tibble
  DF_output = 
    tibble::tibble(
      task = name_task,
      names_dimensions = unlist(names_dimensions),
      items_dimensions = items_dimensions,
      items_reversed = items_reversed,
      items_ignored = items_ignored,
      functions = functions_DIRd,
      description = description_dimensions %>% unlist(),
    ) %>%
    # For each of the dimensions
    rowwise() %>% 
    mutate(items_dimensions = paste(items_dimensions, collapse = ", "))
  
  if (nrow(DF_output) == 1 & sum(DF_output != "") == 1) {
    DF_output %>% mutate(names_dimensions = "TOTAL", items_dimensions = "ALL minus ignored", functions = function_DIRt)
  } else {
    DF_output %>% 
      bind_rows(tibble(task = name_task, names_dimensions = "TOTAL", items_dimensions = "ALL minus ignored", functions = function_DIRt)) %>% 
      mutate(description = 
               case_when(
                 !is.na(description) ~ description,
                 names_dimensions == "TOTAL" ~ description_task,
                 TRUE ~ NA_character_
               ))
  }
  
}

# tasks = list.files("R_tasks", pattern = "\\.R", full.names = TRUE)
# 1:length(tasks) %>%
#   map(~
#         {
#           cli::cli_h1(.x, "\n")
#           create_codebook(tasks, .x)
#           })




#' check_project_and_results
#' Check the project tasks and compare with the csv results to see if there are results or tasks missing
#'
#' @param participants 
#' @param folder_protocol 
#' @param folder_results 
#'
#' @return
#' @export
#'
#' @examples
check_project_and_results <- function(participants, folder_protocol, folder_results) {
  
  # DEBUG
  # participants = 5
  
  files_protocol = dir(folder_protocol)
  files_results = dir(folder_results)
  
  if (length(files_protocol) / (length(files_results)/participants) == 1)  {
    cat("OK, one task per participant")
    # dir("../jsPsychMaker/canonical_protocol/tasks")
  } else {
    cat(length(files_protocol), "tasks\n")
    cat(length(files_results)/participants, "files per participant\n")
  }
  
  experiments_results = tibble(filename = files_results) %>% tidyr::separate(col = filename,
                                                                             into = c("project", "experimento", "version", "datetime", "id"),
                                                                             sep = c("_"), remove = FALSE) %>%
    mutate(id = gsub("(*.)\\.csv", "\\1", id)) %>% distinct(experimento) %>% 
    pull(experimento)
  
  missing_experiments = files_protocol[!files_protocol %in% paste0(experiments_results, ".js")]
  
  cat("Missing data for: ", missing_experiments)
  
  
}




#' read_csv_or_zip
#' Read input files, adapting to multiple csv's or a single zip
#'
#' @param input_files multiple csv's or a single zip
#' @param workers how many cores to use
#'
#' @return
#' @export
#'
#' @examples
read_csv_or_zip <- function(input_files, workers = 1) {
  
  length_files = length(input_files)
  input_folder = unique(dirname(input_files))
  all_csvs = all(grepl("\\.csv", input_files))
  all_zips = all(grepl("\\.zip", input_files))
  
  
  # Read file/s
  if (all_csvs) {
    
    # TEST and remove empty files (size < 100 bytes)
    empty_files = file.info(input_files) %>% as_tibble(rownames = "files") %>% filter(size < 100)
    if (nrow(empty_files) > 0) cli::cli_alert_warning("There are {length(empty_files)} empty input files (size < 100 bytes)")
    input_files = input_files[!input_files %in% empty_files$files]
    
    DF_raw_read = purrr::map_dfr(input_files %>% set_names(basename(.)), data.table::fread, .id = "filename", colClasses = 'character', encoding = 'UTF-8', nThread = as.numeric(workers)) %>% as_tibble()
    # colClasses = c(response = "character")
    
  } else if (length_files == 1 & all_zips) {
    
    # Unzips to temp folder, reads files and deletes temp folder
    DF_raw_read = read_zips(input_files)  
    
  } else {
    # Other issues
    if (length_files == 0) cli::cli_abort("NO files in '{input_folder}'")
    if (length_files != 1 & all_zips) cli::cli_abort("Multiple ZIP files detected in '{input_folder}'")
    if (length_files != 0 & !all_zips & !all_csvs) cli::cli_abort("Multiple types of files detected in '{input_folder}'")
    cli::cli_abort("Something wrong in read_csv_or_zip(). Are input files all csv files or a single zip file?")
  }
  
  return(DF_raw_read)
  
}



#' read_zips
#' Function to unzip and read csv files
#'
#' @param input_files 
#' @param workers 
#' @param unzip_dir 
#' @param silent 
#' @param do_cleanup 
#'
#' @return
#' @export
#'
#' @examples
read_zips = function(input_files, workers = 1, unzip_dir = file.path(dirname(input_files), sprintf("csvtemp_%s", sub(".zip", "", basename(input_files)))), silent = FALSE, do_cleanup = TRUE){
  
  # DEBUG
  # workers = 1
  # unzip_dir = file.path(dirname(input_files), sprintf("csvtemp_%s", sub(".zip", "", basename(input_files))))
  # silent = FALSE
  # do_cleanup = TRUE
  
  dir.create(unzip_dir, showWarnings = FALSE)
  
  # unzip zips OR tar.xz
  if (tools::file_ext(input_files) == "zip") {
    unzip(input_files, overwrite = TRUE, exdir = unzip_dir)
  } else if (tools::file_ext(input_files) %in% c("tar", "xz")) {
    untar(input_files, exdir = unzip_dir) 
  } else {
    cat(paste0("The file extension is '.", tools::file_ext(input_files), "' but needs to be either '.zip' or '.tar.xz'\n"))
  }
  
  # Read only the csv's inside the zip
  fns = list.files(path = unzip_dir, pattern = "*.csv", full.names = TRUE, recursive = TRUE) # Recursive because sometimes the csv's will be inside a folder with the pid, but maybe not always?
  # %>% setNames(file.path(.), .) 
  
  # TEST and remove empty files (size < 100 bytes)
  empty_files = file.info(fns) %>% as_tibble(rownames = "files") %>% filter(size < 100)
  if (nrow(empty_files) > 0) cli::cli_alert_warning("There are {nrow(empty_files)} empty input files (size < 100 bytes)")
  fns = fns[!fns %in% empty_files$files]
  
  
  if (!all(tools::file_ext(fns) == "csv")) cli::cli_abort("The zip file should only contain CSV files")
  
  # Read files
  res = purrr::map_dfr(fns %>% set_names(basename(.)), data.table::fread, .id = "filename", colClasses = 'character', encoding = 'UTF-8', fill = TRUE, nThread = as.numeric(workers)) %>% as_tibble()
  
  # Delete files
  if (do_cleanup) unlink(unzip_dir, recursive = TRUE)
  
  return(res)
}




#' zip_files
#' zip files of a folder
#'
#' @param folder_files 
#' @param zip_name 
#' @param remove_files 
#'
#' @return
#' @export
#'
#' @examples
zip_files <- function(folder_files, zip_name, remove_files = FALSE) {
  
  project_folder = getwd()
  
  # Set Temp folder as working folder so the files in zip WONT have the temp path
  setwd(folder_files)
  
  # List all files to zip (exclude zip_name)
  FILES_ZIP_raw = list.files(folder_files, recursive = TRUE, full.names = FALSE, all.files = TRUE, include.dirs = TRUE)
  FILES_ZIP = FILES_ZIP_raw[!grepl(basename(zip_name), FILES_ZIP_raw)]
  
  # Create safely version so an error won't avoid resetting the project's wd
  zip_safely = purrr::safely(zip)
  
  if (length(FILES_ZIP) == 0) {
    cli::cli_alert_danger("NO files found")
  } else {
    # ZIP zilently (flags = "-q")
    RESULT = zip_safely(zipfile = zip_name, files = FILES_ZIP, flags = "-q")
    # Show error
    if (!is.null(RESULT$error)) {
      cli::cli_text(RESULT$error)
    } else {
      cli::cli_alert_success("ZIPED protocol files to {gsub(project_folder, '', zip_name)}")
    }
  }
  # Remove temp dir and content
  if (remove_files == TRUE) {
    file.remove(FILES_ZIP)
    cli::cli_alert_success("REMOVED {length(FILES_ZIP)} source files")
  } else {
    cli::cli_alert_info("Will NOT REMOVE {length(FILES_ZIP)} source files")
  }
  
  # Reset the project's WD
  setwd(project_folder)
  
}



#' get_zip
#' Get and zip a the data or a full jsPsychMakeR protocol without the data to keep it as a backup
#'
#' @param pid 
#' @param what data/protocol
#'
#' @return
#' @export
#'
#' @examples
get_zip <- function(pid, what, where = NULL, list_credentials = NULL) {
  
  # DEBUG
  # pid = "230"
  # what = "data"
  # where = NULL
  # list_credentials = source(".vault/.credentials")
  # setwd("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychHelpeR/")
  
  # TODO: If no data, do not download!
  
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
    dir.create(dirname(zip_name), recursive = TRUE, showWarnings = FALSE)
    
  } else if (what == "protocol") {
    
    download_folder = "/data/protocol_"
    if (!is.null(where)) download_folder = where
    
    server_folder = pid
    exclude_csv = TRUE
    zip_name = paste0(project_folder, download_folder, pid_safe, ".zip")
    
  }
  
  
  # Create temp dir to download the protocol
  TEMP_DIR = tempdir(check = TRUE)
  
  sync_server_local(server_folder = server_folder, 
                    local_folder = TEMP_DIR,
                    direction = "server_to_local", 
                    only_test = FALSE, 
                    exclude_csv = exclude_csv,
                    delete_nonexistent = TRUE,
                    dont_ask = TRUE, 
                    all_messages = FALSE, 
                    list_credentials = list_credentials)
  
  # ZIP ---
  zip_files(folder_files = TEMP_DIR, 
            zip_name = zip_name, 
            remove_files = TRUE)
  
}




#' check_trialids
#'
#'Checks that trialid's of an experiment in a folder follow the standard expected rules
#' @param local_folder_tasks 
#'
#' @return
#' @export
#'
#' @examples
check_trialids <- function(local_folder_protocol, show_all_messages = FALSE) {
  
  # DEBUG
  # local_folder_protocol = "/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/CSCN-server/protocols/999"
  
  suppressMessages(suppressWarnings(library(dplyr)))
  suppressMessages(suppressWarnings(library(purrr)))
  suppressMessages(suppressWarnings(library(readr)))
  
  # cli::cli_h1("Checking /{basename(local_folder_protocol)}")
  
  scripts = dir(path = paste0(local_folder_protocol, "/tasks"), pattern = ".js", recursive = TRUE, full.names = TRUE)
  if (length(scripts) == 0) {
    
    if (show_all_messages == TRUE) {
      cli::cli_h1("Checking /{basename(local_folder_protocol)}")
      cli::cli_alert_danger("Can't find anything in {local_folder_protocol}")
    }
    
    
  } else {
    
    find_trialids <- function(file_name) {
      
      # DEBUG
      # file_name = scripts[51]
      
      script = read_file(file_name) 
      # expres = ".*?trialid: '(.*?)'.*?"
      # trialid = gsub(expres, "\\1; \n", script) %>% gsub("^(.*; \n).*", "\\1", .) %>% gsub(";", "", .) %>% gsub(" number \n", "", .)
      expres = ".*?trialid: (.*?),.*?"
      trialid = 
        gsub(expres, "\\1; \n", script) %>% 
        gsub("^(.*; \n).*", "\\1", .) %>% 
        gsub(";", "", .) %>% 
        gsub(" number \n", "", .) %>% 
        gsub("'", "", .) %>% # Get rid of '
        gsub('"', '', .) %>% # Get rid of " 
        gsub("  ", " ", .) # Get rid of "  "
      
      if (grepl("This document was made with test_maker", trialid)) trialid = ""
      strsplit(trialid, " \n")[[1]] %>% as_tibble() %>% 
        mutate(file = file_name) %>% 
        rename(trialid = value) %>% 
        filter(!grepl("^Instructions|^Instructions_[0-9]{2}|^Fullscreen|jsPsych.timelineVariable", trialid))
      
    }
    
    
    DF_all_trialids = map_df(scripts, find_trialids)
    
    rule_check_trialids = "^[a-zA-Z0-9]{1,100}_[0-9]{2,3}$|^[a-zA-Z0-9]{1,100}_[0-9]{2,3}_[0-9]{1,3}$|^[a-zA-Z0-9]{1,100}_[0-9]{2,3}_if$|^[a-zA-Z0-9]{1,100}_[0-9]{2,3}_[0-9]{1,3}_if$" # NAME_001, NAMEexperiment_001_1
    # rule_check_trialids = "NAMEtest_01\NAMEtest_01_1\NAMEtest_01_if|NAMEtest_01_1_if" 
    
    # DF_problematic_trialids = 
    #   DF_all_trialids %>% 
    #   filter(!grepl(rule_check_trialids, trialid)) %>% 
    #   mutate(experiment = basename(file)) %>% 
    #   select(-file)
    
    DF_problematic_trialids =
      DF_all_trialids %>% 
      separate(trialid, into = c("task", "num", "subnum"), sep = "_", extra = "merge", fill = "right", remove = FALSE) %>% 
      mutate(experiment = gsub(".js", "", basename(file))) %>% 
      filter(
        !(
          # shortname_itemNumber_otherStuff
          task == experiment & # Task name == experiment
            (grepl("[0-9]{2,3}", num) | grepl("\\+ pad|\\+ String", num)) & # itemNumber hardcoded or automatically generated 
            (is.na(subnum) | subnum == "if" | grepl("[0-9]{1}", subnum) | grepl("if_[0-9]{1}", subnum) | grepl("\\+ num", subnum))
        )
      ) %>% 
      
      # filter(!grepl(rule_check_trialids, trialid)) %>% 
      # mutate(experiment = basename(file)) %>% 
      select(-file)
    
    if (nrow(DF_problematic_trialids) > 0) {
      
      cli::cli_h1("Checking /{basename(local_folder_protocol)}")
      cat(cli::col_red(nrow(DF_problematic_trialids), " ISSUES:\n"), 
          "- experiment:", paste(DF_problematic_trialids %>% pull(experiment) %>% unique(.), collapse = ", "), "\n",
          "- trialid:   ", paste(DF_problematic_trialids %>% pull(trialid) %>% unique(.), collapse = ", "), "\n")
      
    } else {
      
      if (show_all_messages == TRUE) {
        cli::cli_h1("Checking /{basename(local_folder_protocol)}")
        cli::cli_alert_success("All trialid's look great!\n")
      }
      
    }
    
  }
}



#' cli_message
#' Create standardize cli messages
#'
#' @param var_used if using a {variable}, need to include it here
#' @param h1_title 
#' @param success
#' @param danger 
#' @param details 
#' @param info 
#' @param list 
#'
#' @return
#' @export
#'
#' @examples
cli_message <- function(var_used = NULL, h1_title = NULL, info = NULL, success = NULL, danger = NULL, details = NULL, list = NULL) {
  
  # Prepare var_used to be used internally
  # TODO: Should map() through var_used to be able to use multiple vars
  if (!is.null(var_used)) {
    names_vars = deparse(substitute(var_used))
    vars_list = list()
    vars_list[[names_vars]] = var_used
    list2env(vars_list, environment())
  }

  number_non_nulls = sum(c(!is.null(h1_title),  !is.null(info), !is.null(success), !is.null(danger), !is.null(details), !is.null(list)))

  cli::cli(
    {
      # If it's a lonely message, create space with previous Console content
      if (number_non_nulls == 1) {
        cli::cli_par()
        cli::cli_text("")
        cli::cli_end()
      }
      
      if (!is.null(h1_title)) {
        cli::cli_par()
        cli::cli_h1(h1_title)
        cli::cli_end()
      }
      if (!is.null(danger)) {
        cli::cli_par()
        cli::cli_alert_danger(danger)
        cli::cli_end()
      }
      if (!is.null(info)) {
        cli::cli_par()
        cli::cli_alert_info(info)
        cli::cli_end()
      }
      if (!is.null(success)) {
        cli::cli_par()
        cli::cli_alert_success(success)
        cli::cli_end()
      }
      if (!is.null(details)) {
        cli::cli_text(details)
      }
      if (!is.null(list)) {
        cli::cli_li(list)
      }  
    }
  )
  
}



#' set_permissions_google_drive
#'
#' @param pid pid of project
#' @param email_IP email of Principal Researcher to give reading permissions to data in google drive
#'
#' @return
#' @export
#'
#' @examples
set_permissions_google_drive <- function(pid, email_IP) {
  
  googledrive::drive_auth("gorkang@gmail.com")
  
  ADMIN_emails = c("gorkang@gmail.com", "herman.valencia.13@sansano.usm.cl")
  
  # If email_IP is not that of an admin
  if (!email_IP %in% ADMIN_emails) {
    
    # Get all folders in SHARED-data
    SHARED_data_folder = googledrive::drive_ls(googledrive::as_id("1ZNiCILmpq_ZvjX0mkyhXM0M3IHJijJdL"), recursive = FALSE, type = "folder")
    
    # Get id of folder == pid
    ID = SHARED_data_folder |> dplyr::filter(name == pid) |> dplyr::pull(id)
    
    # Present permissions
    permissions_ID = ID |> googledrive::drive_reveal("permissions")
    list_permissions = permissions_ID$drive_resource[[1]]$permissions
    DF_permissions = 1:length(list_permissions) |> 
      purrr::map_df(~{
        tibble::tibble(email = list_permissions[[.x]]$emailAddress,
               role = list_permissions[[.x]]$role)
        })
    
    # IF email_IP does not already have permissions
    if (!email_IP %in% DF_permissions$email) {
      
      # Change permissions for email_IP
      ID |> 
        googledrive::drive_share(
          role = "reader",
          type = "user",
          emailAddress = email_IP,
          emailMessage = paste0("La carpeta de datos del proyecto ", pid, " ha sido compartida contigo. Si es un error o tienes alguna duda, avisa a gorkang@gmail.com")
        )
      cli::cli_alert_success("Granted View permissions to {email_IP}")
      
    # email_IP already has permissions  
    } else {
      cli::cli_alert_info("{email_IP} already has permissions")
    }
    
  # Admins
  } else {
    cli::cli_alert_info("{email_IP} is an ADMIN and already has permissions")
  }

}
