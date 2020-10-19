##' Join the df of all the processed tasks
##'
##' .. content for \details{} ..
##'
##' @title prepare_joined
##' @param df_SBS
##' @param df_CRT7
##' @return
##' @author gorkang
##' @export
create_joined <- function(...) {
  
  # DEBUG
  # arguments = c("df_SBS", "df_CRT7")
  
  
  # Load targets objects used in tests --------------------------------------

  argnames <- sys.call()
  arguments = lapply(argnames[-1], as.character) %>% unlist()
  
  # Make sure the targets we will load exist
  existing_targets = list.files(path = "_targets/objects/", pattern="df_.*", full.names = FALSE, ignore.case = FALSE)
  final_prepared_files = arguments[arguments %in% existing_targets]
  
  # Check if we are not loading all targets
  not_loaded = existing_targets[!existing_targets %in% arguments]
  if (length(not_loaded > 0)) cat(crayon::yellow(paste0("\n[WARNING]: Not joining some of the df: '", paste(not_loaded, collapse = ", "), "'. Did you forgot to include them in the create_joined() target?\n\n")))
  
  # Warning if any of the files do no exist
  if (length(final_prepared_files) != length(arguments)) cat(crayon::yellow(paste0("\n[WARNING]: Can't find ", paste(arguments[!arguments %in% existing_targets], collapse = ", "), " in the '_targets/objects' folder\n\n")))

  
  # Loads and gets all the targets in a single list -------------------------

  # Loads all the prepared targets
  targets::tar_load(!!final_prepared_files, envir = .GlobalEnv)
  
  # List with all the input DF's
  input_list <- map(final_prepared_files, get)
  
  # Name lists
  names(input_list) <- final_prepared_files
  

  # Join all files ----------------------------------------------------------

  # Join all by ID in a single DF
  DF_joined = 
    input_list %>% 
    reduce(full_join, by = "id")
  
  
  # Save files --------------------------------------------------------------
  save_files(DF_joined, short_name_scale = "joined", is_scale = FALSE)

  
  # Output of function ---------------------------------------------------------
  return(DF_joined)

}
