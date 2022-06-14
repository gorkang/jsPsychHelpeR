##' Standardized names for Direct scores, dimensions and scales
##'
##'
##'
##' @param short_name_scale short name of scale
##' @param dimensions c("DIMENSION1", "DIMENSION2"): list of names for the dimensions
##' @param help_names TRUE prints a message with the instructions
##'
##' @title standardized_names
##' @return
##' @author gorkang
##' @export
standardized_names <- function(short_name_scale, dimensions = "", help_names = FALSE) {

  # Global sufix for direct scores
  .GlobalEnv$sufix_DIR = "_DIR"


  # CHECKS ---

  # Character vector
  if (!(is.vector(dimensions) & is.character(dimensions[1]))) {
    cat(
      crayon::red("ERROR: `dimensions` needs to be a character vector. e.g. c('name1', 'name2')\n"),
      "       Right now: `dimensions` = ",   paste0(dimensions), "\n")
    stop()

    # Spaces and other forbiden characters
  } else if (any(grepl(" |_", dimensions))) {
    cat(
      crayon::red("ERROR: `dimensions` can't have spaces and '_'.\n",
                  "       WRONG: c('name dimension', 'name_dimension2')\n",
                  "       RIGHT: c('namedimension', 'NameDimension2')\n"),
      "       Right now: `dimensions` = ",   paste0(dimensions), "\n")
    stop()
  }


  # Dimensions names
  # For each of the values in 'dimensions' will create a name_DIRd[n] and name_STDd[n]
  if (dimensions[1] != "") {

    # DEBUG
    # help_names = TRUE
    # short_name_scale = "XXX"
    # dimensions = c("dimension1", "dimension2")

    # Build strings for DIR
    names_dimensions_DIR = paste0(short_name_scale, "_", dimensions, "_DIRd")
    names_variables_DIR = paste0("name_DIRd", 1:length(names_dimensions_DIR))

    # Build strings for REL
    names_dimensions_REL = paste0(short_name_scale, "_", dimensions, "_RELd")
    names_variables_REL = paste0("name_RELd", 1:length(names_dimensions_REL))

    # Build strings for STD
    names_dimensions_STD = paste0(short_name_scale, "_", dimensions, "_STDd")
    names_variables_STD = paste0("name_STDd", 1:length(names_dimensions_STD))

    # Creates variables in Global Environment
    map2(names_variables_DIR, names_dimensions_DIR, assign, envir = .GlobalEnv)
    map2(names_variables_REL, names_dimensions_REL, assign, envir = .GlobalEnv)
    map2(names_variables_STD, names_dimensions_STD, assign, envir = .GlobalEnv)

    # Message with details ---
    if (help_names == TRUE) cat(crayon::red(crayon::underline("REMEMBER:\n\n")))
    if (help_names == TRUE) cat("",
                                crayon::magenta(crayon::underline("Dimensions\n")),
                                crayon::green(

                                  paste0("- For the DIRect scores of the dimension '", paste0(names_dimensions_DIR), "'",
                                         " use the name '", names_variables_DIR, ",",
                                         crayon::silver(paste0(" FOR EXAMPLE: !!", names_variables_DIR, " := rowSums(...)'\n"))),

                                  paste0("- For the STDard scores of the dimension '", paste0(names_dimensions_STD), "'",
                                         " use the name '", names_variables_STD, ",",
                                         crayon::silver(paste0(" FOR EXAMPLE: !!", names_variables_STD, " := rowSums(...)'\n"))),

                                  paste0("- For the RELiability scores of the dimension'", paste0(names_dimensions_STD), "'",
                                         " use the name '", names_variables_REL, ",",
                                         crayon::silver(paste0(" FOR EXAMPLE: !!", names_variables_REL, " := rowSums(...)'\n"))),
                                  "\n"
                                ))
  }


  # NAs for RAW and DIR items
  .GlobalEnv$name_RAW_NA = paste0(short_name_scale, "_RAW_NA")
  .GlobalEnv$name_DIR_NA = paste0(short_name_scale, "_DIR_NA")

  # Direct scores totals
  .GlobalEnv$name_DIRt = paste0(short_name_scale, "_DIRt")

  # RELiability total scores
  .GlobalEnv$name_RELt = paste0(short_name_scale, "_RELt")

  # Standardized scores totals
  .GlobalEnv$name_STDt_NA = paste0(short_name_scale, "_STDt_NA")
  .GlobalEnv$name_STDt = paste0(short_name_scale, "_STDt")



  # Message with details ---
  if (help_names == TRUE) cat("", crayon::green(

    crayon::magenta(crayon::underline("Total scores\n")),

    paste0("- For the DIRect total score of '",.GlobalEnv$name_DIRt, "'",
           " use the name 'name_DIRt'",
           crayon::silver(paste0(" FOR EXAMPLE: !!name_DIRt := rowSums(...)'\n"))),

    paste0("- For the RELiability total score of '",.GlobalEnv$name_RELt, "'",
           " use the name 'name_RELt'",
           crayon::silver(paste0(" FOR EXAMPLE: !!name_RELt := rowSums(...)'\n"))),

    paste0("- For the STDardized total score of '",.GlobalEnv$name_STDt, "'",
           " use the name 'name_STDt'",
           crayon::silver(paste0(" FOR EXAMPLE: !!name_STDt := rowSums(...)'\n")))
  ), "\n")

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
    if (!identical(DF_CHECK_NA[[1]], DF_CHECK_NA[[2]])) stop("Missing data when processing RAW responses")

    # [REWIEW]: Other ways to check equality
    # all(DF_CHECK_NA[1] == DF_CHECK_NA[2])
    # all.equal(DF_CHECK_NA[[1]], DF_CHECK_NA[[2]])

  } else {

    cat(crayon::blue("\n  - Can't perform NA check, DF does not have RAW_NA and DIR_NA columns\n"))

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
create_raw_long <- function(DF_clean, short_name_scale, numeric_responses = FALSE, is_experiment = FALSE) {

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
    cat(crayon::green("Loaded: "), gsub(",", ", ", parameters_function_raw), "\n")

  } else {

    cat(crayon::red(paste0("'", name_function, "'", "not found in _targets.R")), "\n")

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
save_files <- function(DF, short_name_scale, is_scale = TRUE, is_sensitive = FALSE) {

  # Select path based on nature of the data
  if (is_sensitive == TRUE) {
    data_path = ".vault/outputs/data/"
  } else {
    data_path = "outputs/data/"
  }

  # Save data. Use "df_" if it's a scale otherwise use "DF_"
  if (is_scale == TRUE) {

    write_csv(DF, here::here(paste0(data_path, "df_", short_name_scale , ".csv")))
    write_rds(DF, here::here(paste0(data_path, "df_", short_name_scale , ".rds")))

  } else {

    write_csv(DF, here::here(paste0(data_path, "DF_", short_name_scale , ".csv")))
    write_rds(DF, here::here(paste0(data_path, "DF_", short_name_scale , ".rds")))

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
  
  # Items
  vector_items = DF_long_RAW %>% distinct(trialid) %>% arrange(trialid) %>% pull(trialid)

  # Questions
  DF_question = DF_long_RAW %>% distinct(trialid, stimulus) #%>%  print(n = Inf)

  # Responses
  vector_responses = DF_long_RAW %>% distinct(RAW) %>% pull(RAW)
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
              .groups = "drop") %>%
    DT::datatable()

  cat(crayon::blue("\n", length(vector_items), "Items: "), crayon::silver(paste("'", vector_items[c(1,length(vector_items))], "'", collapse = " to ")), "\n")
  cat(crayon::blue("\n", length(vector_responses), "Responses:\n"), crayon::silver(paste(vector_responses, collapse = "\n ")), "\n")

  if (show_trialid_questiontext == TRUE) {
    cat("\n", crayon::blue("Showing trialid and stimulus for all the items: "), "\n")
    DF_question %>% print(n = Inf)
  }

  if (nrow(DF_check_responses) > 1) {
    cat("\n", crayon::blue(nrow(DF_check_responses), "Different number of responses per item: \n"))
    print(DF_check_responses)
    DF_responses
  }

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
create_new_task <- function(short_name_task, overwrite = FALSE) {

  # DEBUG
  # short_name_task = "PSS"
  # overwrite = FALSE

  # Create file ---
  new_task_file = paste0("R_tasks/prepare_", short_name_task ,".R")

  if (!file.exists(new_task_file) | overwrite == TRUE) {
    cat(crayon::green("\nCreating new file: ", crayon::silver(new_task_file), "\n"))
    file.copy("R_tasks/prepare_TEMPLATE.R", new_task_file, overwrite = overwrite)


    # Replace lines ---
    x <- readLines(new_task_file)
    y <- gsub( "TEMPLATE", short_name_task, x )
    cat(y, file = new_task_file, sep = "\n")

  } else {
    cat(crayon::yellow("\nFile ", crayon::silver(new_task_file), " already exists. Not overwriting\n"))
  }

  # Line to add in _targets.R
  short_name_old = short_name_task


  # Output messages ---
  cat(crayon::green("\nLine for _targets.R: \n"))
  cat(paste0('tar_target(df_', short_name_task, ', prepare_', short_name_task, '(DF_clean, short_name_scale_str = "', short_name_old, '")),\n'))
  cat(crayon::green("\nDON'T FORGET TO ADD", crayon::silver(paste0("df_", short_name_task)), "to create_joined() in _targets.R:", crayon::silver("create_joined(..., ", paste0("df_", short_name_task, ")\n\n"))))

}


# Create an OR vector for the grepl() and other functions. From c(1,2) to "1|2"
create_vector_items <- function(VECTOR) {
  # VECTOR = c( 5, 9, 14, 16, 18)
  cat(paste(sprintf("%02d", VECTOR), collapse = "|"))
}


#' Create _targets.R file from a protocol folder
#'
#' @param folder
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
  # pid_protocol = 999

  suppressPackageStartupMessages(library(dplyr))
  
  if (file.exists("_targets_automatic_file.R")) file.remove("_targets_automatic_file.R")
  
  
  # If both parameters have info, choose folder_data
  if (!is.null(folder_data) & !is.null(folder_tasks)) {
    cli::cli_alert_info("Both 'folder_data' and 'folder_tasks' have information. Using 'folder_data'\n")
    folder_tasks = NULL
  } 
  
  # List js files or parse data files
  if (is.null(folder_data) & !is.null(folder_tasks)) {
    
    files = gsub(".js", "", basename(list.files(folder_tasks, recursive = FALSE, pattern = ".js")))
    
  } else if (!is.null(folder_data) & is.null(folder_tasks)) {
    
    input_files = list.files(folder_data, recursive = TRUE, full.names = TRUE) 
    all_csvs = all(grepl("\\.csv", input_files))
    all_zips = all(grepl("\\.zip", input_files))
    
    # If folder contains csv files
    if (all_csvs) {
      
      files = list.files(folder_data, recursive = TRUE) %>% 
        as_tibble() %>% 
        tidyr::separate(col = value, into = c("project", "experimento", "version", "datetime", "id"), sep = c("_"), remove = TRUE) %>% 
        distinct(experimento) %>% 
        pull(experimento)
      
    # If folder contains single zip
    } else if (all_zips) {
      
      # Unzips to temp folder, reads files and deletes temp folder
      files = read_zips(input_files) %>% distinct(procedure) %>% pull(procedure)
      
    } else {
      cli::cli_abort("Something wrong. {folder_data} should contain all csv files or a single zip file")
    }
    
  }
  
  if (length(files) > 0) {
    
    # Read template
    template = readLines("targets/_targets_TEMPLATE.R")
    
    # Prepare targets section and joins section
    targets = paste0("   tar_target(df_", files, ", prepare_", files, "(DF_clean, short_name_scale_str = '", files,"')),\n") %>% paste(., collapse = "")
    joins = paste0("\t\t\t\t\t\t\t df_", files, ",\n") %>% paste(., collapse = "") %>% gsub(",\n$", "", .)
  
    # Replace  
    final_targets = gsub("TARGETS_HERE", targets, template)
    final_joins = gsub("JOINS_HERE", joins, final_targets)
    final_file = gsub("pid_target = 999", paste0("pid_target = ", pid_protocol), final_joins)
    # cat(final_joins, sep = "\n")
  
    # Create final file
    cat(final_file, file = "_targets_automatic_file.R", sep = "\n")
    
  } else {
    
    cli_text(col_red("{symbol$cross} "), "0 tasks found for protocol '", pid_protocol, "'. NOT creating _targets.R file")
  
  }
  
  # If previous step was successful
  if (file.exists("_targets_automatic_file.R")) {

    response_prompt = menu(choices = c("Yes", "No"), 
                           title = 
                             cli::cli(
                               {
                                 cli::cli_par()
                                 cli::cli_alert_info("Overwrite file '_targets.R' to include the following tasks?")
                                 cli::cli_end()
                                 cli::cli_text("{.pkg {files}}")
                                }
                               )
                           )
    
    if (response_prompt == 1) {
      
      # Create Backup file
      if (file.exists("_targets.R")) file.rename(from = "_targets.R", to = "_targets_old.R")
      
      # RENAME _targets_automatic_file.R as _targets.R. _targets_automatic_file.R was created in the previous step
      file.rename(from = "_targets_automatic_file.R", to = "_targets.R")
      
      # DELETE UNUSED tasks
      TASKS_TO_DELETE = list.files("R_tasks/") %>% as_tibble() %>% mutate(task = gsub("prepare_(.*)\\.R", "\\1", value)) %>% filter(!task %in% files & !grepl("\\.csv", value)) %>% pull(value) %>% paste0("R_tasks/", .)
      
      delete_prompt = menu(choices = c("Yes", "No"), 
                           title = cli::cli(
                             {
                               cli::cli_par()
                               cli::cli_alert_info("{cli::col_red('Delete')} the following {length(TASKS_TO_DELETE)} unused tasks from 'R_tasks/'?:")
                               cli::cli_end()
                               cli::cli_text("{.pkg {basename(TASKS_TO_DELETE)}}")
                             }
                             )
                           )
      
      
      if (delete_prompt == 1) {
        cli::cli_alert_info("Zipping unused tasks to create backup")
        zip(zipfile = "outputs/backup/deleted_tasks.zip", files = TASKS_TO_DELETE)
        file.remove(TASKS_TO_DELETE)
        cli::cli_alert_warning(paste0("Deleted ", length(TASKS_TO_DELETE), " unused tasks. Backup in `outputs/backup/deleted_tasks.zip`"))
      }
      
      # END Messages
      
      cli::cli(
        {
          cli::cli_par()
          cli::cli_h1("All done\n\n")
          cli::cli_end()
    
          cli::cli_par()
          cli::cli_alert_info("NEW '_targets.R' created")
          cli::cli_end()
          
          cli::cli_text(cli::col_grey("Use the following commands to start the data preparation:"))
          cli::cli_li(c("Visualize pipeline: {.code targets::tar_visnetwork()}",
                        "Delete cache: {.code targets::tar_destroy()}", 
                        "Start data preparation: {.code targets::tar_make()}"))
          
        }
      )
    
    } else {
      cli::cli_alert_warning("OK, nothing done\n")
    }
    
  }
    
  # cat(crayon::green("\n_targets_automatic_file.R created."), crayon::yellow("Manually rename it as _targets.R\n"))

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
  if (!is.na(variance_WARNING)) cat(crayon::red("x NO variance. sd = 0"), "\n")
  
  
  
  # deleted_items_NAs
  deleted_items_NAs = paste(names(temp_clean_RAW)[!names(temp_clean_RAW) %in% names(temp_clean)])
  if (length(deleted_items_NAs) > 0) cat(crayon::red("x DELETED VARS (have NA's)", paste(deleted_items_NAs, collapse = ", "), "\n"))

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

    cat(crayon::green("No items with r.drop <= ", min_rdrop), "|| alpha: ", alpha_initial, "\n")

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
    
    cat(crayon::yellow("Filtered", paste0(length(delete_items), "/", ncol(temp_clean_RAW)), "items with r.drop <= ", min_rdrop), "|| alpha: ", alpha_initial , "->", alpha_final, "\n")


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


#' check_progress_pid
#' 
#' Get list of files for a project
#'
#' @param pid 
#'
#' @return
#' @export
#'
#' @examples
check_progress_pid <- function(pid = 3) {

  # remotes::install_github("skgrange/threadr")
  # sudo apt install sshpass

  # Get filenames data from server ---
  
  # CHECK .credentials file exists
  if (!file.exists(".vault/.credentials")) cat(crayon::red("The .vault/.credentials file does not exist. RUN: \n"), crayon::silver("rstudioapi::navigateToFile('setup/setup_server_credentials.R')\n"))
  
  list_credentials = source(".vault/.credentials")
  files_server = threadr::list_files_scp(host = list_credentials$value$IP,
                                directory_remote = paste0(list_credentials$value$main_FOLDER, pid, "/.data"), 
                                user = list_credentials$value$user,
                                password = list_credentials$value$password)
  
  files_csv = grep("csv", basename(files_server), value = TRUE)
  
  return(files_csv)
  
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
  # last_task = "COVIDCONTROL"
  # goal = 850
  # DEBUG = TRUE
  
  # Prepare data ---
  
  files_csv = files_vector
  
  DF_files =
    tibble(filename = files_csv) %>%
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
  
  PLOT_progress =
    DF_progress %>%
    ggplot(aes(fecha_registro, suma_total)) +
    geom_line() +
    geom_bar(aes(fecha_registro, numero_registros), stat = "identity", alpha = .5) +
    geom_point() +
    geom_hline(yintercept = goal, linetype = "dashed", color = "grey") +
    theme_minimal(base_size = 16) +
    scale_x_date(date_breaks = "1 day", guide = guide_axis(angle = 90)) +
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
    
    cat(crayon::green(
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
  # DF = DF_clean_raw
  
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
#'
#' @return
#' @export
#'
#' @examples
update_data <- function(id_protocol, sensitive_tasks = c("")) {
  
  # DEBUG
  # id_protocol = 999
  # sensitive_tasks = c("DEMOGR")

  # CHECKS ------------------------------------------------------------------
  
  credentials_exist = file.exists(".vault/.credentials")
  SSHPASS = Sys.which("sshpass") # Check if sshpass is installed
  RSYNC = Sys.which("rsync") # Check if rsync is installed
  if (!dir.exists(paste0(getwd(), '/data/' , id_protocol, '/'))) dir.create(paste0(getwd(), '/data/' , id_protocol, '/'))
  # if (!dir.exists(paste0("data/", id_protocol, "/"))) stop("CAN'T find the destination folder: 'data/", id_protocol, "'")
  
  if (!credentials_exist) {
    cli::cli_abort(
      c("The file '.vault/.credentials' does NOT exist. Follow the steps in: ", 
        'rstudioapi::navigateToFile("setup/setup_server_credentials.R")'))
  }
   
  
  # Download files ----------------------------------------------------------

  cli::cli_alert_info("Synching files from pid {id_protocol} to 'data/{id_protocol}'")
  
  WD = gsub(" ", "\\ ", getwd(), fixed = TRUE) # Replace " " in path to avoid error
  list_credentials = source(".vault/.credentials")
  result = system(paste0('sshpass -p ', list_credentials$value$password, 
                ' rsync -av --rsh=ssh ', 
                # From
                list_credentials$value$user, "@", list_credentials$value$IP, ":", list_credentials$value$main_FOLDER, id_protocol, '/.data/ ', 
                # To
                WD, '/data/' , id_protocol, '/'), 
                intern = TRUE)
  
  if (length(result) == 4) {
    cli::cli_alert_info("All files already in 'data/{id_protocol}'")
  } else {
    cli::cli_alert_success("Downloaded {length(result) - 5} files to 'data/{id_protocol}'")
  }
  

  # Move sensitive files to .vault/data -------------------------------------

  if (sensitive_tasks != "") {
    # MOVE sensitive data to .vault
    data_folder = paste0("data/", id_protocol)
    sensitive_files = list.files(data_folder, pattern = paste(sensitive_tasks, collapse = "|"), full.names = TRUE)
    
    destination_folder = paste0(".vault/data")
    destination_names = gsub(data_folder, destination_folder, sensitive_files)
    file.rename(from = sensitive_files, to = destination_names)
    
    cli::cli_alert_success("Moved {length(destination_names)} files matching '{paste(sensitive_tasks, collapse = '|')}' to '{destination_folder}'")
    
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
  
  # TODO -----------------------
  
  # - Add reversed items
  # - Add ignored items
  # - Add totals
  
  
  # DEBUG
  # number = 33
  
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
  names_dimensions = stringr::str_extract_all(DF_clean[grepl("names_dimensions =", DF_clean)], '"[\\w_]{1,50}"') %>% purrr::compact()
  # num_dimensions = stringr::str_extract_all(DF_clean[grepl("items_DIRd[0-9] =", DF_clean)], "items_DIRd[0-9]{1,2}")
  items_dimensions = stringr::str_extract_all(DF_clean[grepl("items_DIRd[0-9] =", DF_clean)], "[0-9]{2,3}_[\\w]{1,20}|[0-9]{2,3}") %>% purrr::compact()
  functions_DIRd = gsub(".*!!name_DIRd[0-9] := (\\w{1,10}).*", "\\1", lines_DIRd)
  
  # Dimensions OLD STRUCTURE
  old_names_dimensions =  stringr::str_extract_all(DF_clean[grepl("dimensions =", DF_clean)], '"[\\w_]{1,50}"') %>% purrr::compact()
  old_items_dimensions = stringr::str_extract_all(lines_DIRd, "[0-9]{2,3}_[\\w]{1,20}|[0-9]{2,3}") %>% purrr::compact()
  
  
  # CHECK
  if (length(function_DIRt) == 0) function_DIRt = ""
  if (length(functions_DIRd) == 0) functions_DIRd = ""
  if (length(names_dimensions) == 0) names_dimensions = ""
  if (length(items_dimensions) == 0) items_dimensions = ""
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
      functions = functions_DIRd,
      description = description_dimensions %>% unlist(),
    ) %>%
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
# 
# 
# 1:length(tasks) %>% 
#   map(~ 
#         { 
#           # cat(.x, "\n")
#           create_codebook(.x)
#           }
#     )




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
  
  fns = list.files(unzip_dir, recursive = TRUE) %>% setNames(file.path(unzip_dir, .), .)
  
  if (!all(tools::file_ext(fns) == "csv")) cli::cli_abort("The zip file should only contain CSV files")
  
  # Read files
  res = purrr::map_dfr(fns %>% set_names(basename(.)), data.table::fread, .id = "filename", colClasses = 'character', encoding = 'UTF-8', nThread = as.numeric(workers)) %>% as_tibble()
  
  # Delete files
  if (do_cleanup) unlink(unzip_dir, recursive = TRUE)
  
  return(res)
}
