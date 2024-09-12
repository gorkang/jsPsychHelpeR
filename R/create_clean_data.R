#' create_clean_data
#'
#' @param DF_raw DF
#' @param save_output TRUE / FALSE
#' @param check_duplicates TRUE / FALSE
#'
#' @return DF_clean
#' @export
create_clean_data <- function(DF_raw, save_output = TRUE, check_duplicates = TRUE) {
  
  # targets::tar_load_globals()
  # debug_function("create_clean_data")

  DF_clean_raw =
    DF_raw %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!grepl("repeated", id)) %>% # Delete users that have "repeated" in name
    dplyr::filter(trial_type != "fullscreen") %>% # Empty line
    dplyr::filter(!trialid %in% c("Screen_WM", "Instructions")) %>%  # Delete instructions [TODO]: use regexp to clean instrucciones_NOMBRETEST
    dplyr::filter(!grepl("Instructions", trialid, ignore.case = TRUE)) %>% 
    dplyr::mutate(response = gsub('&nbsp;|\u00A0', '', response), # HTML space and invisible character
           # If we have [] in response, it is probably a multi-select (e.g. DEMOGR_19: "{\"\"Q0\"\":[\"\"&nbsp;Madre\"\",\"\"&nbsp;Hermanas\"\",\"\"&nbsp;Amigas cercanas\"\"]}")
           # In this case, join all responses and separate by ;
           response = 
            dplyr::case_when(
               grepl("\\[", response) ~ gsub(pattern = '"",""', replacement = "; ", x = response, perl = TRUE), # In multi-select, output is: {\"\"Q0\"\":\"\"response1"",""response2\"\"} -> {\"\"Q0\"\":\"\"response1; response2\"\"}
               TRUE ~ response
             ),
           response = gsub("\\[|\\]", "", response) # Clean up remaining "[]"
    ) 
  
  # Creates one line per response
  DF_clean = 
    # Separate multiple responses and put questions in trialid. e.g. PRFBM
    separate_responses(DF_clean_raw) %>% 
    
    # Clean up remaining responses (e.g. DEMOGR_19)
    dplyr::mutate(
      response = gsub('\\{"".*"":', "", response), # Get rid of {""Q0"":
      response = gsub('\\}$', "", response), # Get rid of }
      response = gsub(pattern = '""', replacement = "", x = response, perl = TRUE) # Remaining double quotes""
    ) %>% 
    
    # Clean up stimulus
    dplyr::rename(stimulus_raw = stimulus) %>% 
    dplyr::mutate(
      stimulus = gsub('\\{"".*"":', "", stimulus_raw), # Get rid of {""Q0"":
      stimulus = gsub('\\}$', "", stimulus), # Get rid of }
      stimulus = gsub(pattern = '""', replacement = "", x = stimulus, perl = TRUE) # Remaining double quotes""
    ) %>% 
    
    # Need to make sure the columns used below exist
    dplyr::mutate(button_pressed = ifelse("button_pressed" %in% names(.), button_pressed, NA_character_)) %>% 
    
    # Plugings not using response to store responses
    dplyr::mutate(response = 
            dplyr::case_when(
               is.na(response) & !is.na(button_pressed) ~ button_pressed, # html-button-response
               TRUE ~ response
               ))
  
  
  # CHECK duplicate trialid's -----------------------------------------------
  DF_duplicate_trialids_raw = DF_clean %>% dplyr::count(id, experiment, trialid) %>% dplyr::arrange(desc(n)) %>% dplyr::filter(n > 1)
  
  DF_message = DF_duplicate_trialids_raw %>% 
    # dplyr::filter(experiment != "Consent") |> 
    # dplyr::mutate(experiment = gsub("(.*)_[0-9]{2,3}", "\\1", trialid)) %>% 
    dplyr::group_by(id) %>% 
    dplyr::summarise(duplicate_tasks = paste(unique(experiment), collapse = ", ")) %>% 
    dplyr::transmute(message = paste0(id, ": ", duplicate_tasks ))
  
  if (check_duplicates == TRUE & nrow(DF_message) > 0) rlang::abort(message = paste0("There are duplicate trialid's for: \n['participant: tasks']\n", paste("-", stringr::str_sort(DF_message$message, numeric = TRUE), collapse = "\n"), "\n\nFor more details check `create_clean_data()`"))
  
  # To get full detail, with filenames: 
  # DF_message = DF_clean %>% 
  #   dplyr::group_by(id, trialid) %>% dplyr::summarise(N_files = dplyr::n(), filenames = paste(filename, collapse = "\n")) %>% 
  #   dplyr::filter(N_files > 1) %>%  
  #   dplyr::group_by(id, filenames) %>%  dplyr::summarise(N_trialids = dplyr::n(), N_files = unique(N_files), trialids = paste(trialid, collapse = ", ")) %>% 
  #   dplyr::distinct(filenames, .keep_all = TRUE) %>% dplyr::select(id, N_files, N_trialids, trialids, dplyr::everything()) 
  
  # IDs = DF_message %>% dplyr::group_by(1) %>% dplyr::summarise(IDs = paste(id, collapse = ", ")) %>% dplyr::arrange(desc(IDs)) %>%  dplyr::pull(IDs)
  # FILES = DF_message %>% dplyr::group_by(1) %>% dplyr::summarise(filenames = paste(filenames, collapse = "\n"))  %>% dplyr::pull(filenames)
  
  duplicate_trialids = DF_duplicate_trialids_raw %>% dplyr::count(trialid) %>% dplyr::pull(trialid) %>% paste(., collapse = "; ")
  DF_duplicate_trialids = DF_duplicate_trialids_raw %>% dplyr::count(id)
  if (check_duplicates == TRUE & nrow(DF_duplicate_trialids) > 0) rlang::abort(message = paste0("There are duplicate trialid's: \n", paste("-", duplicate_trialids, collapse = "\n"), "\n\nFor more details check `create_clean_data()`"))
  
  

  # Wide version ------------------------------------------------------------

  DF_clean_wide = 
    DF_clean %>% 
    dplyr::rename(RAW = response) %>%
    dplyr::select(id, trialid, RAW) %>%
    tidyr::pivot_wider(
      names_from = trialid, 
      values_from = c(RAW),
      names_glue = "{trialid}_{.value}")
  
  
  # DF_clean_wide_rt = 
  #   DF_clean %>% 
  #  dplyr::select(id, trialid, rt) %>%
  #   tidyr::pivot_wider(
  #     names_from = trialid, 
  #     values_from = c(rt),
  #     names_glue = "{trialid}_{.value}")
  
  
  
  
  # Save files --------------------------------------------------------------
  if (save_output == TRUE) save_files(DF_clean, short_name_scale = "clean", is_scale = FALSE)
  if (save_output == TRUE) save_files(DF_clean_wide, short_name_scale = "clean_wide", is_scale = FALSE)
  
  
  # Output of function ---------------------------------------------------------
  return(DF_clean)
  
}
