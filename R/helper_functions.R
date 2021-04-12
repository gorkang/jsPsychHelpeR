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


  # CHECKS ------------------------------------------------------------------

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

    # Message with details ----------------------------------------------------
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

  
  
  # Message with details ----------------------------------------------------
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
create_raw_long <- function(DF_clean, short_name_scale, numeric_responses = FALSE) {
  
  # DEBUG
  # short_name_scale = "SCSORF"

  DF_clean %>% 
    # filter(experimento == name_scale) %>% 
    filter(grepl(paste0(short_name_scale, "_[0-9]"), trialid)) %>% 
    select(id, experimento, rt, trialid, stimulus, responses) %>% 
    mutate(responses = 
             if (numeric_responses == TRUE) {
               as.numeric(responses) 
              } else {
                as.character(responses) 
              }
           ) %>% 
    drop_na(trialid) %>% 
    rename(RAW = responses) %>% 
    arrange(trialid, id)
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
  code <- parse("targets/targets_main.R")
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
    data_path = ".vault/output/data/"
  } else {
    data_path = "output/data/"
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



# Create a new prepare_TASK.R file from prepare_TEMPLATE.R replacing TEMPLATE by the short name of the new task
create_new_task <- function(short_name_task, overwrite = FALSE) {
  
  #DEBUG
  # short_name_task = "PSS"
  # old_names = TRUE
  

  # Create file -------------------------------------------------------------
  new_task_file = paste0("R/prepare_", short_name_task ,".R")
  
  if (!file.exists(new_task_file) | overwrite == TRUE) {
  cat(crayon::green("\nCreating new file: ", crayon::silver(new_task_file), "\n"))
  file.copy("R/prepare_TEMPLATE.R", new_task_file, overwrite = overwrite)
  
  
  # Replace lines -----------------------------------------------------------
  x <- readLines(new_task_file)
  y <- gsub( "TEMPLATE", short_name_task, x )
  cat(y, file = new_task_file, sep = "\n")
  
  } else {
    cat(crayon::yellow("\nFile ", crayon::silver(new_task_file), " already exists. Not overwriting\n"))
  }
  
  # Line to add in _targets.R 
  short_name_old = short_name_task
  
  
  # Output messages ---------------------------------------------------------
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
create_targets_file <- function(folder) {
  
  template = readLines("R/_targets_TEMPLATE.R")
  folder = basename(list.dirs(folder, recursive = FALSE))
  targets = paste0("   tar_target(df_", folder, ", prepare_", folder, "(DF_clean, short_name_scale_str = '", folder,"')),\n") %>% paste(., collapse = "")
  joins = paste0("\t\t\t\t\t\t\t df_", folder, ",\n") %>% paste(., collapse = "") %>% gsub(",\n$", "", .)
  
  final_targets = gsub("TARGETS_HERE", targets, template)
  final_joins = gsub("JOINS_HERE", joins, final_targets)
  # cat(final_joins, sep = "\n")
  
  cat(final_joins, file = "_targets_file.R", sep = "\n")
  
  cat(crayon::green("_targets_file.R created."), crayon::yellow("Rename as _targets.R"))
  
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
  quiet_alpha_table = quietly(alpha_table)
  alpha_raw <- function(DF) {psych::alpha(DF, check.keys = TRUE)$total$raw_alpha}
  quiet_alpha_raw = quietly(alpha_raw)
  
  
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
  
  # deleted_items_NAs
  deleted_items_NAs = paste(names(temp_clean_RAW)[!names(temp_clean_RAW) %in% names(temp_clean)])
  if (length(deleted_items_NAs) > 0) cat(crayon::red("\nDELETED VARS (have NA's)", paste(deleted_items_NAs, collapse = ", "), "\n"))
  
  # Filter items where r.drop < min_rdrop
  delete_items_raw = quiet_alpha_table(temp_clean)
  delete_items = delete_items_raw$result %>% select(nitem, r.drop) %>% filter(r.drop <= min_rdrop) %>% pull(nitem)
  delete_items_warnings = delete_items_raw$warnings
  
  
  if (length(delete_items) == 0) {
    
    alpha_initial = round(quiet_alpha_raw(temp_clean)$result, 3)
    alpha_final = NULL
    delete_items = NULL
    keep_items = names(temp_clean)
    
    cat(crayon::green("\nNo items with r.drop <= ", min_rdrop), "|| alpha: ", alpha_initial, "\n")
    
  } else {
  
    # Select items that won't be deleted
    keep_items = names(temp_clean[,!(names(temp_clean) %in% delete_items)])
    
    # Temp DF with selected items
    temp_seleccionados = temp_clean %>% dplyr::select(all_of(keep_items))
    
    # Alphas
    alpha_initial = round(quiet_alpha_raw(temp_clean)$result, 3)
    alpha_final = round(quiet_alpha_raw(temp_seleccionados)$result, 3)
    
    cat(crayon::yellow("\nFiltered", paste0(length(delete_items), "/", ncol(temp_clean_RAW)), "items with r.drop <= ", min_rdrop), "|| alpha: ", alpha_initial , "->", alpha_final, "\n")
    
    
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
  
  write_rds(reliability_output, paste0("output/reliability/", short_name_scale, ".rds"))
  
  return(reliability_output)
  
}
