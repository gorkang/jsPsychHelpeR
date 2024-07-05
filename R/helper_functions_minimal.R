#' setup_folders
#' Extract jsPsychHelpeR.zip and make sure we have all necessary folders in a specific location
#'
#' @param folder destination folder for the project
#' @param pid_data_folder project id data folder inside data/
#' @param extract_zip If TRUE, extracts jsPsychHelpeR.zip to folder
#'
#' @return NULL
#' @export
#'
#' @examples setup_folders(999, tempdir())
setup_folders <- function(pid_data_folder, folder, extract_zip = FALSE) {

  # TODO: ADD check about empty folder and ASK user if we should delete contents
  
  # Avoid spaces in folder path because other functions (e.g. update_data) won't work if there are spaces
  if (grepl(" ", folder)) cli::cli_abort("The folder path should NOT have spaces. You can replace {.code {folder}} for {.code {gsub(' ', '', folder)}}")
  
  if (extract_zip == TRUE) {
    # Make sure folder exists and extract jsPsychHelpeR.zip there
    if (!dir.exists(folder)) dir.create(folder)
    
    # Location of jsPsychHelpeR.zip
    if ("jsPsychHelpeR" %in% utils::installed.packages()) {
      jsPsychHelpeR_zip = system.file("templates", "jsPsychHelpeR.zip", package = "jsPsychHelpeR")
    } else {
      jsPsychHelpeR_zip = "inst/templates/jsPsychHelpeR.zip"
    }
    
    utils::unzip(jsPsychHelpeR_zip, exdir = folder)
    cli::cli_alert_success("jsPsychHelpeR project extracted to {.code {folder}}\n")
    
  }
  
  # Necessary folders
  necessary_folders = c(paste0("data/", pid_data_folder), # data/manual_correction
                        "outputs/backup", "outputs/data", "outputs/plots", "outputs/reliability", "outputs/reports", "outputs/tables", "outputs/tests_outputs", 
                        ".vault/data_vault", ".vault/Rmd", ".vault/outputs/data", ".vault/outputs/reports")
  
  if (all(necessary_folders %in% dir(folder, recursive = TRUE, include.dirs = TRUE, all.files = TRUE))) {
    cli::cli_alert_success("All the necessary folders are present\n")
  } else {
    invisible(purrr::map(paste0(folder, "/", necessary_folders), dir.create, recursive = TRUE, showWarnings = FALSE))
    system("chmod 700 -R .vault/")
    cli::cli_alert_success("Created necessary folders: {.pkg {necessary_folders}}\n")
  }
  
}


#' parse_filename
#' 
#' Parse input files to get the essential pieces encoded in the filename column
#'
#' @param DF DF including the column filename
#' @param separator by default "_"
#'
#' @return Same DF with extra columns "project", "experiment", "version", "datetime", "id"
#' @export
#'
#' @examples tibble::tibble(filename = "999_RTS_original_2023-02-13T222222_1.csv") |>  parse_filename()
parse_filename <- function(DF, separator = "_") {
  
  if(!"filename" %in% colnames(DF)) cli::cli_abort("The input DF must include a filename column")

  DF |> 
    # Make sure it works regardless of having a full path or not
    dplyr::mutate(input = filename,
           filename = basename(filename)) |> 
    
    # Separate chunks
    tidyr::separate(col = filename, 
             into = c("project", "experiment", "version", "datetime", "id"), 
             sep = separator, 
             remove = FALSE,
             extra = "merge") |> # OK when the id contains "_" See project 25
    
    # Clean id
    dplyr::mutate(id = gsub("(*.)\\.csv", "\\1", id))
  
}

#' standardized_names
#' 
#' Standardized names for Direct scores, dimensions and scales
#'
#' @param short_name_scale short name of scale
#' @param dimensions c("DIMENSION1", "DIMENSION2"): list of names for the dimensions
#' @param help_names TRUE prints a message with the instructions
#'
#' @return Creates names_list, which can be used in the prepare_TASK scripts to 
#' create columns with standardized names for direct scores, total scores, etc.
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
  

  if (help_names == TRUE){
    cli::cli_alert_info(
      c('To create the code to calculate dimensions or total scores you can use:\n', 
        cli::code_highlight('create_formulas(type = "dimensions_DIR", functions = "sum", items_dimensions)'))
    )
  }
  
  return(names_list)
  
}



#' check_NAs
#'
#' Checks if the NAs of the RAW columns are the same as the ones from the DIR columns
#' Important to catch errors when transforming data from RAW to DIR
#' 
#' @param DF_joined DF_joined
#'
#' @return NULL
check_NAs <- function(DF_joined) {

  DF_CHECK_NA = DF_joined %>%
   dplyr::select(dplyr::ends_with("_NA"))

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



#' create_raw_long
#' 
#' Create long DF for a specific task (short_name_scale). Also cleans the DF and if it's an experiment, makes sure names are OK.
#'
#' @param DF_clean DF_clean in a prepare_TASK
#' @param short_name_scale Short name of scale
#' @param numeric_responses TRUE / FALSE
#' @param is_experiment TRUE / FALSE
#' @param keep_time TRUE / FALSE keep datetime in raw_long
#' @param help_prepare TRUE / FALSE
#'
#' @return A long and clean version of DF_clean for a specific task
#' @export
create_raw_long <- function(DF_clean, short_name_scale, numeric_responses = FALSE, is_experiment = FALSE, keep_time = FALSE, help_prepare = FALSE) {
  
  experimental_conditions = "NOTHING_SHOULD_MATCH_THIS"
  if (is_experiment == TRUE) experimental_conditions = "condition_"
  
  # Columns selection
  columns_selected = c("id", "experiment", "rt", "trialid", "stimulus", "response")
  if (keep_time == TRUE) columns_selected = c(columns_selected, "datetime")
  
  
  DF_output = 
    DF_clean %>%
    dplyr::filter(experiment == short_name_scale) %>%
    dplyr::select(all_of(columns_selected), dplyr::starts_with(eval(experimental_conditions), ignore.case = FALSE)) |> 
    # dplyr::select(id, experiment, rt, trialid, stimulus, response, dplyr::starts_with(eval(experimental_conditions), ignore.case = FALSE)) %>%
    dplyr::mutate(response =
                    if (numeric_responses == TRUE) {
                      as.numeric(response)
                      } else {
                        as.character(response)
                      }
                  ) %>%
    tidyr::drop_na(trialid) %>%
    dplyr::rename(RAW = response) %>%
    dplyr::arrange(trialid, id)
  
    # If is experiment, make sure condition_within is fine
    if (is_experiment == TRUE) {
      if (any(grepl("[ -]", DF_output$condition_within))) {
        
        # condition_within_options = unique(DF_output$condition_within)
        
        cli::cli_text(col_red("{symbol$cross} "), "Unholy names found in condition_within. Cleaning up. It can take a while...\n")
        
        # Create a dictionary and left_join
        DICTIONARY_within = DF_output %>% 
          dplyr::distinct(condition_within) %>% 
          dplyr::rowwise() %>% 
          dplyr::mutate(condition_withinOK = paste(purrr::map_chr(stringr::str_split(condition_within, pattern = "_", simplify = TRUE), janitor::make_clean_names, case = "small_camel"), collapse = "_"))
        
        # Show changes
        cli::cli_li(DICTIONARY_within %>% dplyr::transmute(DIFF = paste0(condition_within, " -> ",  condition_withinOK)) %>% dplyr::pull(DIFF))
        
        DF_output = 
          DF_output %>% 
          dplyr::left_join(DICTIONARY_within, by = "condition_within") %>% 
          dplyr::mutate(condition_within = condition_withinOK) %>% dplyr::select(-condition_withinOK)
        
        }
    }
  
  
  if (nrow(DF_output) == 0) stop("No trialid's matching '", short_name_scale, "_[0-9]' found in DF_clean")
  
  if (help_prepare == TRUE) prepare_helper(DF_long_RAW = DF_output, show_trialid_questiontext = TRUE)
  
  return(DF_output)
}


#' Save files
#'
#' Save a DF in one or more output formats to output/data or .vault/outputs/data/
#'
#' @title create_raw_wide
#'
#' @param DF dataframe
#' @param short_name_scale short name of scale
#' @param is_scale = TRUE
#' @param output_formats One or more of the following: csv, csv2 or rds
#' @param is_sensitive = TRUE
#' @importFrom data.table fwrite
#' @importFrom cli cli_abort
#'
#' @return NULL
#' @export
save_files <- function(DF, short_name_scale, is_scale = TRUE, is_sensitive = FALSE, output_formats = c("csv", "csv2")) {

  # CHECK
  implemented_formats = c("csv", "csv2", "rds")
  if (!all(output_formats %in% implemented_formats)) cli::cli_abort("output_formats should be one of {.code {implemented_formats}}")
  
  # Select path based on nature of the data
  data_path = ifelse(is_sensitive == TRUE, ".vault/outputs/data/", "outputs/data/")
  
  # Use "df_" if it's a scale, otherwise use "DF_"
  prefix = ifelse(is_scale == TRUE, "df_", "DF_")
  
  # Save data
    if ("csv" %in% output_formats) data.table::fwrite(DF, here::here(paste0(data_path, prefix, short_name_scale , ".csv")))
    if ("csv2" %in% output_formats) data.table::fwrite(DF, here::here(paste0(data_path, prefix, short_name_scale , "_sp.csv")), sep = ";")
    if ("rds" %in% output_formats) saveRDS(DF, here::here(paste0(data_path, prefix, short_name_scale , ".rds")))

}



#' prepare_helper
#' 
#' Helper to correct tasks. Shows trialid's, responses, etc. to help correcting a task
#'
#' @param DF_long_RAW DF_long_RAW created by create_raw_long()
#' @param show_trialid_questiontext TRUE / FALSE
#'
#' @return A number of tables, DT, etc.
#' @export
prepare_helper <- function(DF_long_RAW, show_trialid_questiontext = FALSE) {

  # Items
  vector_items = DF_long_RAW %>% dplyr::distinct(trialid) %>% dplyr::arrange(trialid) %>% dplyr::pull(trialid)

  # Questions
  DF_question = DF_long_RAW %>% dplyr::distinct(trialid, stimulus) #%>%  print(n = Inf)

  # Responses
  vector_responses = DF_long_RAW %>% dplyr::distinct(RAW) %>% dplyr::pull(RAW)
  if (is.numeric(vector_responses)) {
    vector_responses = stringr::str_sort(vector_responses, numeric = TRUE)
  } else {
    vector_responses = stringr::str_sort(vector_responses, numeric = FALSE)
  }
  DF_trialid_alternativeresponses = DF_long_RAW %>% dplyr::count(trialid, RAW) %>% dplyr::count(trialid, name = "alternative responses")
  DF_check_responses = DF_trialid_alternativeresponses %>% dplyr::count(`alternative responses`, name = "number of items") %>% dplyr::arrange(desc(`number of items`))
  
  DF_responses =
    DF_long_RAW %>%
    dplyr::count(trialid, RAW) %>%
    dplyr::group_by(trialid) %>%
    dplyr::summarise(N = dplyr::n(),
              Responses = paste(RAW, collapse = ", "),
              .groups = "drop") %>%
    dplyr::group_by(Responses) %>%
    dplyr::summarise(N = dplyr::n(),
              trialid = paste(trialid, collapse = ", "),
              .groups = "drop") 

  
  # OUTPUTS ---

  # PRINT OUTPUT (used inside another function)
  count_responses(DF_long_RAW |> 
                   dplyr::select(id, trialid, RAW) |> 
                    tidyr::pivot_wider(names_from = trialid,
                                values_from = RAW)) |> 
    print()
  
  
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


#' Create _targets.R file from a protocol folder
#'
#' @param pid project id
#' @param folder protocol folder
#' @param dont_ask do everything without asking for user input
#'
#' @return Creates a _targets.R file
#' @export
#' @examples 
#' \dontrun{
#' create_targets_file(pid = 999, 
#'                     folder = "/home/emrys/Downloads/jsPsychHelpeRtest/",
#'                     dont_ask = FALSE)
#' }

create_targets_file <- function(pid = 0, folder, dont_ask = FALSE) {

  folder_data_helper = paste0(folder, "/data/", pid, "/")
  folder_data_vault = paste0(folder, "/.vault/data_vault/")
  
  # If old _targets_automatic_file.R exists, delete
  if (file.exists(paste0(folder, "/_targets_automatic_file.R"))) {
    cli::cli_alert_info("Deleting OLD _targets_automatic_file.R")
    file.remove(paste0(folder, "/_targets_automatic_file.R"))
  }
  
    # List files in data folder
    input_files_temp = list.files(path = folder_data_helper, pattern = "*.csv|*.zip", full.names = TRUE)
    input_files_temp_vault = list.files(path = folder_data_vault, pattern = "*.csv|*.zip", full.names = TRUE) 
    
    # If zip, read_csv_or_zip will read the zip and list files, If csv, will list after cleaning empty files
    input_files = read_csv_or_zip(input_files = input_files_temp, only_list = TRUE)
    input_files_vault = read_csv_or_zip(input_files = input_files_temp_vault, only_list = TRUE)
    
    # Get distinct tasks/experiments
    tasks = tibble::tibble(filename = c(input_files, input_files_vault)) |> 
      parse_filename() |> 
      dplyr::distinct(experiment) |> 
      dplyr::pull(experiment)

  
  if (length(tasks) > 0) {
    
    # Equivalent tasks DICC ---
    
      # Use dictionary to find the correct prepare function for translations or adaptations
      # This can be used when the correction logic DOES not change (e.g. BNTen -> BNT)
      DICC_equivalent_tasks = tibble::tibble(canonical = c("BNT"),
                                             alt = c("BNTen"))
      
      # Create vector with canonical names using dictionary
      tasks_canonical = 
        tibble::tibble(original = tasks) |>
        dplyr::left_join(DICC_equivalent_tasks, by = c("original" = "alt")) |> 
        dplyr::mutate(tasks = ifelse(!is.na(canonical), canonical, original)) |> 
        dplyr::pull(tasks)
      
      # END DICC ---
      
      
      all_files_all = list.files(paste0(folder, "/R_tasks/"))
      all_files = all_files_all[!all_files_all %in% "prepare_TEMPLATE.R"] # Do not remove prepare_TEMPLATE
      all_prepare_funs = all_files[grepl("\\.R$", all_files)]
      all_supporting_files = all_files[grepl("\\.csv$", all_files)]
      
      # Create named vectors to use names to filter but keep filenames
      names(all_prepare_funs) <- gsub("^prepare_(.*)\\.R$", "\\1", all_prepare_funs)
      names(all_supporting_files) <- gsub("^prepare_(.*)-.*\\.csv$", "\\1", all_supporting_files)
      
      # prepare_TASKS not found
      prepare_funs_NOT_found = tasks_canonical[!tasks_canonical %in% names(all_prepare_funs)]
      
      if (length(prepare_funs_NOT_found) > 0) {
        cli::cli_alert_warning("Did not find the prepare_FUN for {length(prepare_funs_NOT_found)} task: {prepare_funs_NOT_found}. 
                                -If it is a translation, add it to `DICC_equivalent_tasks` in `create_targets_file()`.
                                -If it is a new task, create the prepare_FUN with `create_new_task()`")
      }
    

    # Read template
    template = readLines(paste0(folder, "/inst/templates/_targets_TEMPLATE.R"))
    
    # Prepare targets section and joins section
    targets = paste0("   tar_target(df_", tasks, ", prepare_", tasks_canonical, "(DF_clean, short_name_scale_str = '", tasks,"', output_formats = output_formats)),\n") %>% paste(., collapse = "")
    joins = paste0("\t\t\t\t\t\t\t df_", tasks, ",\n") %>% paste(., collapse = "") %>% gsub(",\n$", "", .)
  
    # Replace targets and joins sections 
    final_targets = gsub("#TARGETS_HERE", targets, template)
    final_joins = gsub("#JOINS_HERE", joins, final_targets)
    
    # Find and replace pid_target
    line_pid_target = which(grepl("pid_target = '999'", final_joins))
    if (length(line_pid_target) == 0) cli::cli_abort("Can't find `pid_target = '999'` in _targets_TEMPLATE.R") # Check we find the string to replace
    final_joins[line_pid_target] = paste0("\tpid_target = '", pid, "'")
    final_file = final_joins
    # final_file = gsub("pid_target = '999'", paste0("pid_target = '", pid, "'"), final_joins) # OLD method
    # cat(final_joins, sep = "\n")
  
    # Create final file
    cat(final_file, file = paste0(folder, "/_targets_automatic_file.R"), sep = "\n")
    
  } else {
  
    cli::cli_abort("0 tasks found for protocol '{pid}'. Please make sure data is in {.code {folder_data_helper}}")  

  }
  
  # If previous step was successful
  if (file.exists(paste0(folder, "/_targets_automatic_file.R"))) {
    
    if (dont_ask == FALSE) {

    response_prompt = menu(choices = c("YES", "No"), 
                           title = cli_message(var_used = tasks,
                                               info = "{cli::style_bold((cli::col_yellow('Overwrite')))} file '_targets.R' to include the following {length(tasks)} tasks?", #in {.code {folder}}
                                               details = "{.pkg {tasks}}"))
    } else {
      
      cli_message(var_used = tasks,
                  info = "{cli::style_bold((cli::col_yellow('Overwriten')))} file '_targets.R' to include the following {length(tasks)} tasks", #in {.code {folder}} 
                  details = "{.pkg {tasks}}")
      response_prompt = 1
      
    }
    
    if (response_prompt == 1) {
      
      # Create Backup file
      if (file.exists(paste0(folder, "/_targets.R"))) file.rename(from = paste0(folder, "/_targets.R"), to = paste0(folder, "/_targets_old.R"))
      
      # RENAME _targets_automatic_file.R as _targets.R. _targets_automatic_file.R was created in the previous step
      file.rename(from = paste0(folder, "/_targets_automatic_file.R"), to = paste0(folder, "/_targets.R"))
      
      
      # DELETE UNUSED tasks and supporting folders
        tasks_TO_DELETE = all_prepare_funs[!names(all_prepare_funs) %in% tasks_canonical]
        supporting_files_TO_DELETE = all_supporting_files[!names(all_supporting_files) %in% tasks_canonical]

        # Join Tasks and supporting files        
        files_TO_DELETE = c(tasks_TO_DELETE, supporting_files_TO_DELETE) |> as.character()
        
        if(length(files_TO_DELETE) > 0) {
          
          # relative so zip works, absolute to delete 
          files_TO_DELETE_relative = files_TO_DELETE %>% paste0("R_tasks/", .)
          files_TO_DELETE_absolute = files_TO_DELETE %>% paste0(folder, "/R_tasks/", .)

          if (dont_ask == FALSE) {
            
            delete_prompt = menu(choices = c("Yes", "NO"),
                                 title = cli_message(var_used = files_TO_DELETE,
                                                     h2_title = "Clean up",
                                                     info = "{cli::style_bold((cli::col_red('DELETE')))} the following {length(files_TO_DELETE)} unused tasks and supporting files from 'R_tasks/'?:",
                                                     details = "{.pkg {basename(files_TO_DELETE)}}")
            )
            
          } else {
            cli_message(var_used = files_TO_DELETE,
                        h2_title = "Clean up",
                        info = "{cli::style_bold((cli::col_red('DELETED')))} the following {length(files_TO_DELETE)} unused tasks and supporting files from 'R_tasks/':",
                        details = "{.pkg {basename(files_TO_DELETE)}}")
            delete_prompt = 1
          }
          
          if (delete_prompt == 1) {
            
            
            cli::cli_alert_info("Zipping and removing unused tasks")
            
            # Need to pass relative paths to zip, so we setwd to the files and come back ---
              # TODO: zip_files() only works with full folder
              project_folder = getwd()
              setwd(folder) # Set Temp folder as working folder so the files in zip WONT have the temp path
              utils::zip(zipfile = paste0(folder, "/outputs/backup/deleted_tasks.zip"), files = files_TO_DELETE_relative, flags = "-q") # Silent flags = "-q"
              setwd(project_folder)
            # ---
            
            # Remove files
            file.remove(files_TO_DELETE_absolute)
            cli::cli_alert_success(paste0("Deleted ", length(files_TO_DELETE), " unused tasks. Backup in {.code {paste0(folder, '/outputs/backup/deleted_tasks.zip')}}"))
          }
        }
      
      # END Message
      cli_message(h2_title = "All done", 
                  success = "NEW '_targets.R' created"
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
#' @param DF dataframe
#' @param short_name_scale short name of scale
#' @param items items to check
#' @param min_rdrop rdrop threshold 
#'
#' @return Saves a DF and outputs information about the item selection
#' @export
auto_reliability = function(DF, short_name_scale = short_name_scale_str, items = NULL, min_rdrop = 0.2) {

  # DEBUG
  # DF = DF_wide_RAW
  # short_name_scale = short_name_scale_str
  # items =  items_dimensions[[1]]
  # min_rdrop = 0.2
  # items =  NULL
  

  # Internal functions
  alpha_table <- function(DF) {psych::alpha(DF, check.keys = TRUE)$item.stats %>% tibble::as_tibble(rownames = "nitem")}
  safely_alpha_table = safely(alpha_table)
  alpha_raw <- function(DF) {psych::alpha(DF, check.keys = TRUE)$total$raw_alpha}
  safely_alpha_raw = safely(alpha_raw)


  # Create items_selection vector
  if (is.null(items)) {
    items_selection = DF %>% dplyr::select(matches("DIR$")) %>% names(.)
  } else {
    items_selection = paste0(short_name_scale, "_", items, "_DIR")
  }

  
  # Get selected variables
  temp_clean_RAW = DF %>%
    dplyr::select(dplyr::all_of(items_selection))

  temp_clean = temp_clean_RAW %>%
    dplyr::select_if(~ !any(is.na(.))) # Need to delete columns with any NA

  
  # CHECK variance
  variance_WARNING = ifelse(all(1:ncol(temp_clean) %>% purrr::map_dbl(~ sd(temp_clean[.x] %>% unlist)) == 0), "No variance", NA) 
  if (!is.na(variance_WARNING)) cli::cli_alert_danger("NO variance in items (sd = 0): \n{paste(items_selection, collapse = '; ')}")
  
  
  # deleted_items_NAs
  deleted_items_NAs = paste(names(temp_clean_RAW)[!names(temp_clean_RAW) %in% names(temp_clean)])
  if (length(deleted_items_NAs) > 0) cat(cli::col_red("x DELETED VARS (have NA's): ", paste(deleted_items_NAs, collapse = ", "), "\n"))

  # Filter items where r.drop < min_rdrop
  delete_items_raw = suppressMessages(safely_alpha_table(temp_clean))
  if (!is.null(delete_items_raw$result)) {
    delete_items = delete_items_raw$result %>% dplyr::select(nitem, r.drop) %>% dplyr::filter(r.drop <= min_rdrop) %>% dplyr::pull(nitem)
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
    temp_seleccionados = temp_clean %>% dplyr::select(dplyr::all_of(keep_items))

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
  
  # Extract 3-digits trialid numbers. If not found, try 2 digits
  item_selection_string = gsub(".*([0-9]{3}).*", "\\1", keep_items)
  if (any(grepl("[A-Za-z]", item_selection_string))) item_selection_string = gsub(".*([0-9]{2}).*", "\\1", keep_items)
  if (any(grepl("[A-Za-z]", item_selection_string))) cli::cli_alert_danger("Could not extract numbers from: {item_selection_string}")
  
  
  reliability_output =
    list(min_rdrop = min_rdrop,
         alpha_initial = alpha_initial,
         alpha_final = alpha_final,
         delete_items = delete_items,
         keep_items = keep_items,
         delete_items_warnings = delete_items_warnings,
         item_selection_string = item_selection_string)

  saveRDS(reliability_output, paste0("outputs/reliability/", short_name_scale, ".rds"))
  
  return(reliability_output)

}


#' move_sensitive_tasks_to_vault
#' Move sensitive files to .vault/data_vault
#' 
#' @param pid project id
#' @param folder project location
#' @param sensitive_tasks short names of sensitive tasks
#'
#' @return NULL
#' @export
move_sensitive_tasks_to_vault <- function(pid, folder, sensitive_tasks = c("")) {
  
  data_folder = paste0(folder, '/data/' , pid, '/')
  
  # TODO: files should be inside .vault/data_vault/`pid`?
  
  if (sensitive_tasks != "") {
    # MOVE sensitive data to .vault
    sensitive_files = list.files(data_folder, pattern = paste(sensitive_tasks, collapse = "|"), full.names = TRUE)
    
    # destination_folder_sensitive = paste0(destination_folder, "../../.vault/data_vault")
    destination_names = gsub(paste0("data/", pid, "/"), ".vault/data_vault/", sensitive_files)
    file.rename(from = sensitive_files, to = destination_names)
    
    cli::cli_alert_success("Moved {length(destination_names)} files matching '{paste(sensitive_tasks, collapse = '|')}' to '{folder}/.vault/data_vault/'")
    
  }
  
}

#' show_progress_pid
#'
#' Show progress of data collection of a project
#'
#' @param pid project id
#' @param files_vector vector of files 
#' @param last_task short name of last task
#' @param goal number of participants
#' @param DEBUG TRUE / FALSE
#'
#' @return Table, plot, DF... about the progress of data collection
#' @export
show_progress_pid <- function(pid = 3, files_vector, last_task = "Goodbye", goal = 100, DEBUG = FALSE) {
  
  # DEBUG
  # pid = 3
  # files_vector = files_3
  # last_task = "Goodbye"
  # goal = 100
  # DEBUG = TRUE
  
  # Prepare data ---
  
  # Read files in csv or zip
  DF_files = tibble(filename = read_csv_or_zip(files_vector, only_list = TRUE)) |> 
    parse_filename()

  DF_progress =
    DF_files %>%
    dplyr::filter(experiment == last_task) %>%#"COVIDCONTROL"
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::distinct(filename, datetime) %>%
    dplyr::mutate(fecha_registro = as.Date(datetime)) %>%
    dplyr::count(fecha_registro, name = "numero_registros") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(suma_total = cumsum(numero_registros),
           days_since_start = as.integer(fecha_registro - min(fecha_registro)),
           rate = round(suma_total / days_since_start, 2)) %>% 
    dplyr::arrange(desc(fecha_registro))
  
  
  # Plot ---
  days = difftime(max(DF_progress$fecha_registro), min(DF_progress$fecha_registro), units = "days")
  if (days > 20) {
    date_breaks = "weeks"
  } else {
    date_breaks = "days"
  }
  
  # Nudge proportionally with a minimum of 5
  nudge_value = ifelse(goal/50 < 5, 5, goal/50)
  
  PLOT_progress =
    DF_progress %>%
    ggplot2::ggplot(ggplot2::aes(fecha_registro, suma_total)) +
    ggplot2::geom_line() +
    ggplot2::geom_bar(ggplot2::aes(fecha_registro, numero_registros), stat = "identity", alpha = .5) +
    ggplot2::geom_point() +
    ggplot2::geom_text(aes(label = suma_total), nudge_y = nudge_value) +
    ggplot2::geom_hline(yintercept = goal, linetype = "dashed", color = "grey") +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::scale_x_date(date_breaks = date_breaks, guide = ggplot2::guide_axis(angle = 90)) +
    ggplot2::scale_y_continuous(n.breaks = 10) +
    ggplot2::labs(title = paste0("Protocolo " , pid, " completado"),
         subtitle = paste0("Ultimo dato: ", as.Date(max(DF_files$datetime))),
         caption = paste0("Last test used: ", last_task, ". Goal: ", goal, " participants"), x = "")
  
  
  # Calculate output vars
  suma_total_ahora = max(DF_progress$suma_total)
  suma_total_yesterday = DF_progress %>% dplyr::filter(fecha_registro <= Sys.Date() - 1) %>% dplyr::pull(suma_total) %>% first()
  suma_total_lastweek = DF_progress %>% dplyr::filter(fecha_registro <= Sys.Date() - 7) %>% dplyr::pull(suma_total) %>% first()
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
  
  
  TABLE = tibble::tibble(last_n_days = c(1, 7, DAYS),
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
#' Creates one line per response. This function is critical to separate multiple
#' responses in different rows
#'
#' @param DF DF with a trialid and response columns
#'
#' @return The same DF cleaning up the response column. If a cell contains 
#' multiple responses, it will separate them in multiple lines.
#' @export
#'
#' @examples separate_responses(DF = tibble::tibble(trialid = "PRFBM_04", 
#' response = '{""Q1"":""Muy de acuerdo"",""Q2"":""Muy de acuerdo""}'))
separate_responses <- function(DF) {
  
  
  # How many different N of responses we have
  different_N = DF %>% 
    dplyr::mutate(N_responses = stringr::str_count(response, '\\"".*?"":')) %>% 
    dplyr::pull(N_responses) %>% unique()
  
  # Internal function to separate   
  separate_N <- function(N) {
    
    # When there is no response recorded (e.g. INFCONS)
    if (is.na(N)) {
      
      DF %>% 
        dplyr::mutate(N_responses = stringr::str_count(response, '\\"".*?"":')) %>% 
        dplyr::filter(is.na(N_responses)) %>% 
        dplyr::mutate(question = "Q0",
               response_raw = response)
      
      # N == 0 should not exist in canonical jsPsych
    } else if (N == 0) {
      
      DF %>% 
        dplyr::mutate(N_responses =  stringr::str_count(response, '\\"".*?"":')) %>% 
        dplyr::filter(N_responses == N) %>% 
        dplyr::mutate(question = "Q0",
               response_raw = response)
      
      
      # For any number of responses
    } else {
      
      pattern_names = 1:N %>% purrr::map(~ c(paste0("qid", .x), paste0("resp", .x))) %>% unlist()
      pattern_template_raw = '\\""(.*?)"":(.*)'
      pattern_template = paste(rep(pattern_template_raw, N), collapse = ",")
      
      DF_temp = 
        DF %>% 
        dplyr::mutate(N_responses =  stringr::str_count(response, '\\"".*?"":')) %>% 
        dplyr::filter(N_responses == N) %>% 
        tidyr::extract(response, pattern_names, regex = pattern_template, remove = FALSE) %>% 
        dplyr::rename(response_raw = response) 
      
      names_qid = DF_temp %>% dplyr::select(matches("qid[0-9]{1,3}")) %>% names()
      names_resp = DF_temp %>% dplyr::select(matches("resp[0-9]{1,3}")) %>% names()
      values_qid = DF_temp %>% dplyr::select(matches("qid[0-9]{1,3}")) %>% 
        tidyr::pivot_longer(dplyr::everything()) %>% dplyr::distinct(value) %>% dplyr::pull(value)
      
      1:length(names_qid) %>% 
        purrr::map_df(~ DF_temp %>% tidyr::pivot_wider(names_from = names_qid[.x], values_from = names_resp[.x])) %>% 
        dplyr::select(-matches("qid[0-9]{1,3}"), -matches("resp[0-9]{1,3}")) %>% 
        tidyr::pivot_longer(dplyr::all_of(values_qid), names_to = "question", values_to = "response") %>%
        tidyr::drop_na(response) %>% 
        dplyr::mutate(response = gsub('\\"|}', '', response))
      
      
    }
  }
  
  # Apply separate_N() to all the different_N's 
  DF_out = 
    purrr::map_df(different_N, separate_N) %>% 
    dplyr::mutate(trialid = 
            dplyr::case_when(
               question == "Q0" ~ trialid,
               question == "" ~ trialid,
               is.na(question) ~ trialid,
               TRUE ~ paste0(trialid, "_", question)
             ))
  
  return(DF_out)
  
}



#' create_codebook
#' Extracts information from the prepare_TASK.R files to create a codebook. Used in report_DF_clean.Rmd
#'
#' @param task A prepare_TASK.R file
#'
#' @return A DF with correction information of a prepare_TASK() function
#' @export
#'
#' @examples create_codebook(task = system.file("R_tasks", "prepare_AIM.R", package = "jsPsychHelpeR"))
create_codebook <- function(task) {
  
  # TODO: Add totals
  
  DF = readLines(task)
  
  DF_clean = DF[!grepl("^[ \t\\#]{1,100}\\#", DF)] # Get rid of comments
  
  lines_DIRd = DF_clean[grepl("!!name_DIRd", DF_clean)]
  
  # Task
  name_task = gsub("prepare_|\\.R", "", basename(task))
  
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
    dplyr::rowwise() %>% 
    dplyr::mutate(items_dimensions = paste(items_dimensions, collapse = ", "))
  
  if (nrow(DF_output) == 1 & sum(DF_output != "") == 1) {
    DF_output %>% dplyr::mutate(names_dimensions = "TOTAL", items_dimensions = "ALL minus ignored", functions = function_DIRt)
  } else {
    DF_output %>% 
      dplyr::bind_rows(tibble::tibble(task = name_task, names_dimensions = "TOTAL", items_dimensions = "ALL minus ignored", functions = function_DIRt)) %>% 
      dplyr::mutate(description = 
              dplyr::case_when(
                 !is.na(description) ~ description,
                 names_dimensions == "TOTAL" ~ description_task,
                 TRUE ~ NA_character_
               ))
  }
  
}



#' read_csv_or_zip
#' Read input files, adapting to multiple csv's or a single zip
#'
#' @param input_files multiple csv's or a single zip
#' @param workers Workers for data.table::fread
#' @param only_list Only list the files, but do not read them
#'
#' @return Either a list of files or a dataframe after reading the files
#' @export
#'
#' @examples read_csv_or_zip(system.file("extdata/", "999.zip", package = "jsPsychHelpeR"))
read_csv_or_zip <- function(input_files, workers = 1, only_list = FALSE) {

  length_files = length(input_files)
  input_folder = unique(dirname(input_files))
  all_csvs = all(grepl("\\.csv", input_files))
  all_zips = all(grepl("\\.zip", input_files))

  # Read file/s
  if (all_csvs) {
    
    # TEST and remove empty files (size < 100 bytes)
    empty_files = file.info(input_files) %>% tibble::as_tibble(rownames = "files") %>% dplyr::filter(size < 100)
    if (nrow(empty_files) > 0) cli::cli_alert_warning("There are {length(empty_files)} empty input files (size < 100 bytes)")
    input_files = input_files[!input_files %in% empty_files$files]
    
    if (only_list == TRUE) {
      DF_raw_read = input_files
    } else {
      DF_raw_read = purrr::map_dfr(input_files %>% purrr::set_names(basename(.)), data.table::fread, .id = "filename", colClasses = 'character', encoding = 'UTF-8', nThread = as.numeric(workers)) %>% tibble::as_tibble()
    }
    
    
  } else if (length_files == 1 & all_zips) {
    
    # Unzips to temp folder, reads files and deletes temp folder
    DF_raw_read = read_zips(input_files, only_list = only_list)
    
  } else {
    # Other issues
    if (length_files == 0) cli::cli_abort("NO files in '{input_folder}'")
    if (length_files != 1 & all_zips) cli::cli_abort("Multiple ZIP files detected in '{input_folder}'")
    if (length_files != 0 & !all_zips & !all_csvs) cli::cli_abort("Multiple types of files detected in '{input_folder}'. Make sure either a single zip or multiple csv files are present")
    cli::cli_abort("Something wrong in read_csv_or_zip(). Are input files all csv files or a single zip file?")
  }
  
  return(DF_raw_read)
  
}



#' read_zips
#' Function to unzip and read csv files. This is used in read_csv_or_zip()
#'
#' @param input_files A zip, tar or xz file
#' @param workers Workers for data.table::fread
#' @param unzip_dir Where to unzip (if do_cleanup == TRUE, the folder will be deleted)
#' @param silent Show feedback FALSE / TRUE
#' @param do_cleanup Delete temp folder TRUE / FALSE
#' @param only_list Only list the files, but do not read them
#'
#' @return Either a list of files or a dataframe after reading the files
#' @export
#'
#' @examples read_zips(system.file("extdata/", "999.zip", package = "jsPsychHelpeR"))
read_zips = function(input_files, workers = 1, unzip_dir = file.path(tempdir(), sprintf("csvtemp_%s", sub(".zip", "", basename(input_files)))), silent = FALSE, do_cleanup = TRUE, only_list = FALSE){
  
  dir.create(unzip_dir, showWarnings = FALSE)
  
  # unzip zips OR tar.xz
  if (tools::file_ext(input_files) == "zip") {
    utils::unzip(input_files, overwrite = TRUE, exdir = unzip_dir)
  } else if (tools::file_ext(input_files) %in% c("tar", "xz")) {
    utils::untar(input_files, exdir = unzip_dir) 
  } else {
    cli::cli_abort(paste0("The file extension is '.", tools::file_ext(input_files), "' but needs to be either '.zip' or '.tar.xz'\n"))
  }
  
  # Read only the csv's inside the zip
  fns = list.files(path = unzip_dir, pattern = "*.csv", full.names = TRUE, recursive = TRUE) # Recursive because sometimes the csv's will be inside a folder with the pid, but maybe not always?
  # %>% setNames(file.path(.), .) 
  
  # TEST and remove empty files (size < 100 bytes)
  empty_files = file.info(fns) %>% tibble::as_tibble(rownames = "files") %>% dplyr::filter(size < 100)
  if (nrow(empty_files) > 0) cli::cli_alert_warning("There are {nrow(empty_files)} empty input files (size < 100 bytes)")
  fns = fns[!fns %in% empty_files$files]
  
  
  if (!all(tools::file_ext(fns) == "csv")) cli::cli_abort("The zip file should only contain CSV files")
  
  # Read files
  if (only_list == TRUE) {
    res = fns
  } else {
    res = purrr::map_dfr(fns %>% purrr::set_names(basename(.)), data.table::fread, .id = "filename", colClasses = 'character', encoding = 'UTF-8', fill = TRUE, nThread = as.numeric(workers)) %>% tibble::as_tibble()  
  }
  
  
  # Delete files
  if (do_cleanup) unlink(unzip_dir, recursive = TRUE)
  
  return(res)
}



#' cli_message
#' Create standardize cli messages
#'
#' @param var_used if using a {variable}, need to include it here
#' @param h1_title title
#' @param h2_title subtitle
#' @param success cli_success
#' @param danger cli_danger
#' @param details details
#' @param info  info
#' @param list list of elements
#'
#' @return A cli message
#' @export
#'
#' @examples cli_message(h1_title = "title")
cli_message <- function(var_used = NULL, h1_title = NULL,  h2_title = NULL, info = NULL, success = NULL, danger = NULL, details = NULL, list = NULL) {
  
  # Prepare var_used to be used internally
    # Otherwise, we can't use vars in the messages: e.g. info = "{var_not_in_var_used}" will give an error
  # TODO: Should purrr::map() through var_used to be able to use multiple vars
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
      if (!is.null(h2_title)) {
        cli::cli_par()
        cli::cli_h2(h2_title)
        cli::cli_end()
      }
      if (!is.null(danger)) {
        cli::cli_par()
        cli::cli_alert_danger(danger)
        cli::cli_end()
      }
      if (!is.null(success)) {
        cli::cli_par()
        cli::cli_alert_success(success)
        cli::cli_end()
      }
      if (!is.null(info)) {
        cli::cli_par()
        cli::cli_alert_info(info)
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




#' count_responses
#'
#' @param DF .
#' @param max_length_strings .
#'
#' @return Plots and tables
#' @export
count_responses <- function(DF, max_length_strings = 50) { #, n_unique = 20
  
  # Bar plot ---
  
  DF_out = 
    DF |> 
    tidyr::pivot_longer(2:ncol(DF), values_transform = list(value = as.character)) |> 
    dplyr::count(value) |> 
    dplyr::mutate(value = as.character(ifelse(is.na(value), "Unknown", value))) |>
    dplyr::mutate(value = as.character(ifelse(value == "", "Empty", value))) |> 
    dplyr::mutate(value = stringr::str_trunc(value, max_length_strings)) |> 
    
    dplyr::mutate(PCT = (n/(ncol(DF) * nrow(DF))) * 100) |> 
    dplyr::arrange(desc(n))
  
  PLOT =
    DF_out |> 
    ggplot2::ggplot(ggplot2::aes(value, n, fill = value)) +
    ggplot2::geom_bar(stat = "identity", na.rm = FALSE, show.legend = FALSE) +
    # ggplot2::scale_x_discrete() +
    ggplot2::theme_minimal() +
    scale_fill_manual(values = c(Unknown = "darkred", Empty = "darkred")) +
    ggplot2::scale_x_discrete(labels = factor(gtools::mixedsort(unique(DF_out$value)))) +
    # WARNING: Vectorized input to `element_text()` is not officially supported.
    ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,
                                     color = ifelse(factor(gtools::mixedsort(unique(DF_out$value))) %in% c("Unknown", "Empty"), "darkred", "black"))) +
    ggplot2::labs(title = "Number of unique values across all variables")
  
  
  # Heatmap ---
  
  DF_wide_temp =
    DF |> 
    tidyr::pivot_longer(2:ncol(DF), values_transform = list(value = as.character)) |> 
    dplyr::count(name, value) |> 
    dplyr::mutate(value = as.character(ifelse(is.na(value), "Unknown", value))) |> 
    dplyr::mutate(value = as.character(ifelse(value == "", "Empty", value))) |> 
    dplyr::mutate(value = stringr::str_trunc(value, max_length_strings))
  
  
  DF_wide = 
    DF_wide_temp |> 
    dplyr::mutate(value = factor(DF_wide_temp$value, levels = gtools::mixedsort(unique(as.character(DF_wide_temp$value))))) 
  # dplyr::mutate(color_x = ifelse(value %in% c("Unknown", "Empty"), "darkred", "black"))
  
  # colors_nas <- ifelse(factor(gtools::mixedsort(unique(DF_wide$value))) %in% c("Unknown", "Empty"), "red", "black")
  
  Paleta_DV = c("grey90", "grey80", "grey70", "grey60", "grey50", "gray40", "gray30", "gray20", "gray10", "black")
  
  MIN = min(DF_wide$n)
  MAX = max(DF_wide$n)
  
  PLOT_all =
    DF_wide |> 
    ggplot2::ggplot(ggplot2::aes(value, name, fill = n, labels = value)) +
    ggplot2::geom_tile() +
    ggplot2::theme_test() +
    
    # ggplot2::scale_x_discrete(labels = factor(gtools::mixedsort(unique(DF_wide$value)))) +
    
    # Legend
    ggplot2::scale_fill_gradientn(colours = Paleta_DV, 
                         na.value = "transparent", 
                         limits = c(MIN, MAX),
                         breaks = round(seq(MIN, MAX, length.out = 5), 0)) +
    # WARNING: Vectorized input to `element_text()` is not officially supported.
    ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, 
                                     color = ifelse(factor(gtools::mixedsort(unique(DF_wide$value))) %in% c("Unknown", "Empty"), "darkred", "black"))) +
    ggplot2::labs(title = "Values per variable")
  
  
  # Skimr ---
  OUT = list(DF_out = DF_out,
             PLOT = PLOT_all / PLOT,
             DF |> skimr::skim()
  )
  
  return(OUT)
}


#' snapshot_check
#' Check all changing snapshots. For some reason testthat::snapshot_review() does
#'  not seem to work when multiple images change. With snapshot_check, we use 
#'  testthat:::review_app to check all .new snapshots
#'
#' @param snapshot_location Location of the snapshots, by default "testthat/_snaps/snapshots/"
#'
#' @return Open a shiny app
#' @export
snapshot_check <- function(snapshot_location = "testthat/_snaps/snapshots/") {
  
  FILES = list.files(path = snapshot_location, pattern = "new", full.names = TRUE)
  FILES_old = gsub("\\.new", "", FILES)
  
  testthat:::review_app(name = basename(FILES_old), 
                        old_path = FILES_old,
                        new_path = FILES)
  
}

#' clean_names_analysis
#' Clean names of columns to use in output tables and plots
#'
#' @param DF 
#'
#' @return The same data frame after renaming the DIRd, DIRt and other similar variables
#' @export
#'
#' @examples clean_names_analysis(tibble::tibble(IdParticipant = 1, TASK_SimpleThing_DIRd = 1, TASK3_NotSoSimple_DIRt = 1))
clean_names_analysis <- function(DF) {

  cols_to_rename = DF |> 
    select(ends_with(c("DIRd", "DIRt", "RELd", "STDd"))) |> 
    names()
  
  new_names = gsub(" DIRd| DIRt| RELd| STDd", "",
       gsub("([a-z0-9])([A-Z])", "\\1 \\2", 
            gsub("_", " ", cols_to_rename)))

  DF_output = DF |> 
    rename_with(~ new_names[which(cols_to_rename == .x)], 
                .cols = all_of(cols_to_rename))
  
  return(DF_output)
     
}
