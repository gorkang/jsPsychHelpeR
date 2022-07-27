##' Create DF_clean
##'
##' .. content for \details{} ..
##'
##' @title
##' @param DF_raw
##' @return
##' @author gorkang
##' @export
create_clean_data <- function(DF_raw, save_output = TRUE) {
  
  # targets::tar_load_globals()
  # debug_function("create_clean_data")
  
  DF_clean_raw =
    DF_raw %>% 
    janitor::clean_names() %>% 
    filter(trial_type != "fullscreen") %>% # Empty line
    filter(!trialid %in% c("Screen_WM", "Instructions")) %>%  # Delete instructions [TODO]: use regexp to clean instrucciones_NOMBRETEST
    filter(!grepl("Instructions", trialid, ignore.case = TRUE)) %>% 
    mutate(response = gsub('&nbsp;|\u00A0', '', response), # HTML space and invisible character
           # If we have [] in response, it is probably a multi-select (e.g. DEMOGR_19: "{\"\"Q0\"\":[\"\"&nbsp;Madre\"\",\"\"&nbsp;Hermanas\"\",\"\"&nbsp;Amigas cercanas\"\"]}")
           # In this case, join all responses and separate by ;
           response = 
             case_when(
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
    mutate(
      response = gsub('\\{"".*"":', "", response), # Get rid of {""Q0"":
      response = gsub('\\}$', "", response), # Get rid of }
      response = gsub(pattern = '""', replacement = "", x = response, perl = TRUE) # Remaining double quotes""
    ) %>% 
    
    # Clean up stimulus
    rename(stimulus_raw = stimulus) %>% 
    mutate(
      stimulus = gsub('\\{"".*"":', "", stimulus_raw), # Get rid of {""Q0"":
      stimulus = gsub('\\}$', "", stimulus), # Get rid of }
      stimulus = gsub(pattern = '""', replacement = "", x = stimulus, perl = TRUE) # Remaining double quotes""
    ) %>% 
    
    # Need to make sure the columns used below exist
    mutate(button_pressed = ifelse("button_pressed" %in% names(.), button_pressed, NA_character_)) %>% 
    
    # Plugings not using response to store responses
    mutate(response = 
             case_when(
               is.na(response) & !is.na(button_pressed) ~ button_pressed, # html-button-response
               TRUE ~ response
               ))
  
  
  # CHECK duplicate trialid's -----------------------------------------------
  DF_duplicate_trialids_raw = DF_clean %>% count(id, trialid) %>% arrange(desc(n)) %>% filter(n > 1)
  
  DF_message = DF_duplicate_trialids_raw %>% 
    mutate(experimento = gsub("(.*)_[0-9]{2,3}", "\\1", trialid)) %>% 
    group_by(id) %>% 
    summarize(duplicate_tasks = paste(unique(experimento), collapse = ", ")) %>% 
    transmute(message = paste0(id, ": ", duplicate_tasks ))
  
  if (nrow(DF_message) > 0) rlang::abort(message = paste0("There are duplicate trialid's for: \n['participant: tasks']\n", paste("-", str_sort(DF_message$message, numeric = TRUE), collapse = "\n"), "\n\nFor more details check `create_clean_data()`"))
  
  
  # DF_message = DF_clean %>% count(id, experimento, trialid) %>% arrange(desc(n)) %>% filter(n > 1) %>% 
  #   group_by(id) %>% summarize(duplicate_tasks = paste(unique(experimento), collapse = ", ")) %>% transmute(message = paste0(id, ": ", duplicate_tasks ))

  # DF_message = DF_clean %>% 
  #   group_by(id, trialid) %>% summarise(N_files = n(), filenames = paste(filename, collapse = "\n")) %>% 
  #   filter(N_files > 1) %>%  
  #   group_by(id, filenames) %>%  summarise(N_trialids = n(), N_files = unique(N_files), trialids = paste(trialid, collapse = ", ")) %>% 
  #   distinct(filenames, .keep_all = TRUE) %>% select(id, N_files, N_trialids, trialids, everything()) 
  
  # IDs = DF_message %>% group_by(1) %>% summarise(IDs = paste(id, collapse = ", ")) %>% arrange(desc(IDs)) %>%  pull(IDs)
  # FILES = DF_message %>% group_by(1) %>% summarise(filenames = paste(filenames, collapse = "\n"))  %>% pull(filenames)
  
  duplicate_trialids = DF_duplicate_trialids_raw %>% count(trialid) %>% pull(trialid) %>% paste(., collapse = "; ")
  DF_duplicate_trialids = DF_duplicate_trialids_raw %>% count(id)
  if (nrow(DF_duplicate_trialids) > 0) rlang::abort(message = paste0("There are duplicate trialid's: \n", paste("-", duplicate_trialids, collapse = "\n"), "\n\nFor more details check `create_clean_data()`"))
  
  # Save files --------------------------------------------------------------
  if (save_output == TRUE) save_files(DF_clean, short_name_scale = "clean", is_scale = FALSE)
  
  
  # Output of function ---------------------------------------------------------
  return(DF_clean)
  
}
