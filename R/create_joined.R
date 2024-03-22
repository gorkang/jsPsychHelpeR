#' create_joined
#' 
#' Join the df of all the processed tasks
#'
#' @param ... .
#'
#' @return DF_joined
#' @export
create_joined <- function(...) {
  
  # DEBUG
  # targets::tar_load_globals()
  # debug_function("create_joined")
  # arguments = c("df_SBS", "df_CRT7")
  
  
  # Load targets objects used in tests --------------------------------------

  argnames <- sys.call()
  arguments = lapply(argnames[-1], as.character) %>% unlist()
  
  # Make sure the targets we will load exist
  existing_targets = list.files(path = "_targets/objects/", pattern="df_.*", full.names = FALSE, ignore.case = FALSE)
  final_prepared_files = arguments[arguments %in% existing_targets]
  
  # Check if we are not loading all targets
  not_loaded = existing_targets[!existing_targets %in% arguments]
  if (length(not_loaded > 0)) cat(cli::col_yellow(paste0("\n[WARNING]: Not joining some of the df: '", paste(not_loaded, collapse = ", "), "'. Did you forgot to include them in the create_joined() target?\n\n")))
  
  # Warning if any of the files do no exist
  if (length(final_prepared_files) != length(arguments)) cat(cli::col_yellow(paste0("\n[WARNING]: Can't find ", paste(arguments[!arguments %in% existing_targets], collapse = ", "), " in the '_targets/objects' folder\n\n")))

  
  # Loads and gets all the targets in a single list -------------------------

  # Loads all the prepared targets
  # Since targets 1.3.0 cannot use tar_load() in a running pipeline
  1:length(final_prepared_files) |> 
    walk(~{
      assign(final_prepared_files[.x], readRDS(paste0("_targets/objects/", final_prepared_files[.x])), envir = .GlobalEnv)
    })
  
  # List with all the input DF's
  input_list <- purrr::map(final_prepared_files, get)
  
  # Name lists
  names(input_list) <- final_prepared_files
  

  # Join all files ----------------------------------------------------------

  # Join all by ID in a single DF
  DF_joined = 
    input_list %>% 
    purrr::reduce(dplyr::full_join, by = "id")
  
  # Only RAW
  DF_joined_RAW = DF_joined %>% dplyr::select(id, dplyr::ends_with("RAW"))
  
  
  # Save files --------------------------------------------------------------
  save_files(DF_joined, short_name_scale = "joined", is_scale = FALSE)
  save_files(DF_joined_RAW, short_name_scale = "joined_RAW", is_scale = FALSE)

  
  # Output of function ---------------------------------------------------------
  return(DF_joined)

}
