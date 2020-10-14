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
    # short_name_scale = "XXX"
    # dimensions = c("dimension1", "dimension2")
    
    # Build strings for DIR
    names_dimensions_DIR = paste0(short_name_scale, "_", dimensions, "_DIRd")
    names_variables_DIR = paste0("name_DIRd", 1:length(names_dimensions_DIR))
    
    # Build strings for STD
    names_dimensions_STD = paste0(short_name_scale, "_", dimensions, "_STDd")
    names_variables_STD = paste0("name_STDd", 1:length(names_dimensions_STD))
    
    # Creates variables in Global Environment
    map2(names_dimensions_DIR, names_variables_DIR, assign, envir = .GlobalEnv)
    map2(names_dimensions_STD, names_variables_STD, assign, envir = .GlobalEnv)

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
      "\n"
    ))
  }
  
  # Direct scores totals
  .GlobalEnv$name_DIRt_NA = paste0(short_name_scale, "_DIRt_NA")
  .GlobalEnv$name_DIRt = paste0(short_name_scale, "_DIRt")
  
  # Standardized scores totals
  .GlobalEnv$name_STDt_NA = paste0(short_name_scale, "_STDt_NA")
  .GlobalEnv$name_STDt = paste0(short_name_scale, "_STDt")

  
  
  # Message with details ----------------------------------------------------
  if (help_names == TRUE) cat("", crayon::green(
    
    crayon::magenta(crayon::underline("Total scores\n")),
    
    paste0("- For the DIRect total score of '",.GlobalEnv$name_DIRt, "'", 
           " use the name 'name_DIRt'",
           crayon::silver(paste0(" FOR EXAMPLE: !!name_DIRt := rowSums(...)'\n"))),
    
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
##' @param DF
##' @return
##' @author gorkang
##' @export
check_NAs <- function(DF) {
  
  DF_CHECK_NA = DF %>% 
    select(ends_with("_NA")) 
  
  if (ncol(DF_CHECK_NA) == 2) {
  
    # Check we have the same number of NAs in RAW and PROC DFs
    if (!identical(DF_CHECK_NA[[1]], DF_CHECK_NA[[2]])) stop("Missing data when processing RAW responses")
    
    # [REWIEW]: Other ways to check equality
    # all(DF_CHECK_NA[1] == DF_CHECK_NA[2])
    # all.equal(DF_CHECK_NA[[1]], DF_CHECK_NA[[2]])
    
  } else {
    
    cat(crayon::blue("\n  - Can't perform NA check, DF does not have RAW_NA and PROC_NA columns\n"))
    
  }
  
}




##' Create long DF for a specific task (name_scale)
##'
##' 
##'
##' @title create_raw_long
##' @param DF
##' @return
##' @author gorkang
##' @export
create_raw_long <- function(DF, name_scale, numeric_responses = FALSE) {
  
  # DEBUG
  # name_scale = "CRT_7"
  

  DF %>% 
    filter(experimento == name_scale) %>% 
    
    # [TODO]: DELETE THIS ONCE input files are OK ---------------------
    mutate(response = 
             case_when(
               is.na(response_x) ~ response,
               is.na(response) ~ response_x)
           ) %>% 
    
    select(id, experimento, rt, trialid, question_text, response) %>% 
    
    mutate(response = 
             if(numeric_responses == TRUE) {
               as.numeric(response) 
              } else {
                as.character(response) 
              }
           ) %>% 
    drop_na(trialid)
}




##' Create wide DF for a specific task (short_name_scale)
##'
##' 
##'
##' @title create_raw_wide
##' @param DF
##' @return
##' @author gorkang
##' @export
create_raw_wide <- function(DF_long, short_name_scale) {

  # DEBUG  
  # short_name_scale = "Supernatural"
  
  name_RAW_NA = paste0(short_name_scale, "_RAW_NA")

  DF_long %>% 
    select(id, trialid, response) %>% 
    mutate(trialid = paste0(trialid, "_RAW")) %>% 
    pivot_wider(names_from = trialid, values_from = response) %>% 
    mutate(!!name_RAW_NA := rowSums(is.na(select(., matches(short_name_scale)))))
  
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
  # name_function = "prepare_CRT7"
  
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
##' @return
##' @author gorkang
##' @export
save_files <- function(DF, short_name_scale, is_scale = TRUE) {
  
  # Use "df_" if it's a scale otherwise use "DF_"
  if (is_scale == TRUE) {
    
    write_csv(DF, here::here(paste0("output/data/df_", short_name_scale , ".csv")))
    write_rds(DF, here::here(paste0("output/data/df_", short_name_scale , ".rds")))
    
  } else {
    
    write_csv(DF, here::here(paste0("output/data/DF_", short_name_scale , ".csv")))
    write_rds(DF, here::here(paste0("output/data/DF_", short_name_scale , ".rds")))
    
  }
  
}
