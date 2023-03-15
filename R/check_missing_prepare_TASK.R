# Run this to Download all protocols (minus csv's) to a local folder
# Then check:
# - for which ones we do not have preparation scripts 
# - for which ones we do not have googledoc details in
    # + https://docs.google.com/spreadsheets/d/1Eo0F4GcmqWZ1cghTpQlA4aHsc8kTABss-HAeimE2IqA/edit#gid=0
    # + https://docs.google.com/spreadsheets/d/1LAsyTZ2ZRP_xLiUBkqmawwnKWgy8OCwq4mmWrrc_rpQ/edit#gid=0

check_missing_prepare_TASK <- function(sync_protocols = FALSE, 
                                       check_trialids = FALSE, 
                                       check_new_task_tabs = FALSE, 
                                       delete_nonexistent = FALSE, 
                                       show_all_messages = FALSE,
                                       helper_folder = ".",
                                       CSCN_server_folder = "..") {
  
  # DEBUG
  # sync_protocols = TRUE
  # check_trialids = TRUE
  # check_new_task_tabs = TRUE
  # delete_nonexistent = FALSE
  # show_all_messages = TRUE
  # helper_folder = "."
  # CSCN_server_folder = ".."
  # suppressPackageStartupMessages(targets::tar_load_globals())
  

  # Load functions  ---------------------------------------------------------

  suppressPackageStartupMessages(source(paste0(helper_folder, "/_targets_packages.R")))
  invisible(lapply(list.files(paste0(helper_folder, "/R", full.names = TRUE, pattern = ".R$")), source))
  
  local_protocols = here::here(paste0(CSCN_server_folder, "/CSCN-server/protocols/"))
  local_prepare_tasks = here::here(paste0(helper_folder, "/R_tasks/"))
  if (!file.exists(local_prepare_tasks)) local_prepare_tasks = here::here("../jsPsychHelpeR/R_tasks/")
  
  
  
  # SOURCES -----------------------------------------------------------------
  
    # tasks: s files in jsPsychMaker/*/tasks/*
    # scripts: prepare_*.R files in jsPsychHelpeR
    # googledoc: information in the two google docs: 
      # + Tareas jsPsychR 
      # + NUEVAS Tareas jsPsychR
  
  
  # Download all protocols ------------------------------------------------
  
  # rsync's to local folder all except the csv files
  
  if (sync_protocols == TRUE) {
    # https://unix.stackexchange.com/questions/432801/rsync-exclude-a-certain-file-extension-unless-zipped
    sync_server_local(server_folder = "", 
                      local_folder = local_protocols, 
                      direction = "server_to_local", 
                      only_test = !sync_protocols, # we do !sync_protocols because the parameter is only_test (!)
                      exclude_csv = TRUE, # DO NOT INCLUDE DATA
                      delete_nonexistent = delete_nonexistent) # Delete files localy if they are NOT in server anymore
  }

  

  # DATA SOURCES ------------------------------------------------------------
      
    # Unique tasks -----------------------------------------------------------
  
      # Unique tasks found in all the protocols
      ALL_tasks = list.files(local_protocols, pattern = "*.js", full.names = TRUE, recursive = TRUE)
      DF_tasks =
        ALL_tasks[grepl("*tasks/.*\\.js", ALL_tasks)] %>% tibble::as_tibble() %>% 
        dplyr::filter(!grepl("OLD_TESTS", value)) %>% 
        dplyr::mutate(task = gsub("\\.js", "", basename(value)),
               protocol = gsub(".*\\/(test\\/.*)\\/tasks\\/.*|.*\\/(.*)\\/tasks\\/.*", "\\1\\2", value)) %>% 
          dplyr::group_by(task) %>% 
          dplyr::summarise(protocols = paste(unique(protocol), collapse = ", ")) %>% 
        dplyr::distinct(task, .keep_all = TRUE)
  
  
    # Unique correction scripts -----------------------------------------------
  
      ALL_scripts = list.files(local_prepare_tasks, pattern = "*.R", full.names = TRUE, recursive = TRUE)
      DF_scripts = 
        ALL_scripts %>% tibble::as_tibble() %>% 
        dplyr::mutate(script = gsub("prepare_(.*)\\.R", "\\1", basename(value))) %>% 
        dplyr::distinct(script)
  
  
    # Google doc --------------------------------------------------------------
    
      # Reads canonical googledoc and googledoc with NEW tasks and combines them
      googlesheets4::local_gs4_quiet() # No googlesheets4::read_sheet messages
      
      googlesheets4::gs4_auth("gorkang@gmail.com")
      DF_googledoc1 = 
        googlesheets4::read_sheet("1Eo0F4GcmqWZ1cghTpQlA4aHsc8kTABss-HAeimE2IqA", sheet = 2, skip = 0) %>% 
        dplyr::rename(short_name = `Codigo Test`) %>% 
        dplyr::filter(!grepl("short_name", short_name)) %>% 
        dplyr::arrange(short_name) %>% 
        dplyr::select(short_name, Nombre, Descripcion) %>% 
        tidyr::drop_na(short_name)
      
      DF_googledoc_NEW = 
        googlesheets4::read_sheet("1LAsyTZ2ZRP_xLiUBkqmawwnKWgy8OCwq4mmWrrc_rpQ", sheet = 2, skip = 0) %>% 
        dplyr::rename(short_name = `Codigo Test`) %>% 
        dplyr::filter(!grepl("SIN espacios ni caracteres extraños", short_name)) %>% 
        dplyr::arrange(short_name) %>% 
        dplyr::select(short_name, Nombre, Descripcion, EMAIL) %>% 
        tidyr::drop_na(short_name)
      
      DF_googledoc = DF_googledoc1 %>% 
        dplyr::bind_rows(DF_googledoc_NEW) %>% 
        dplyr::distinct(short_name)
      
    
   

  # MISSING -----------------------------------------------------------------

    # JS Tasks without correction scripts -----------------------------------
  
    missing_script = DF_tasks$task[!DF_tasks$task %in% DF_scripts$script]
    DF_missing_script = DF_tasks %>% dplyr::filter(task %in% missing_script) %>% dplyr::arrange(task)
   
  
    # Tasks without info on google docs -------------------------------------
  
    missing_tasks_googledoc = DF_tasks$task[!DF_tasks$task %in% DF_googledoc$short_name]
    DF_missing_googledoc = DF_tasks %>% dplyr::filter(task %in% missing_tasks_googledoc) %>% dplyr::arrange(task)
    
  
    # In google docs not yet implemented (JS) -------------------------------
    
    missing_googledoc_tasks = DF_googledoc_NEW$short_name[!DF_googledoc_NEW$short_name %in% DF_tasks$task]
    DF_missing_JS_tasks = DF_googledoc_NEW %>% dplyr::filter(short_name %in% missing_googledoc_tasks) %>% dplyr::arrange(short_name) %>% 
      dplyr::rename(task = short_name) %>% dplyr::select(-EMAIL)
    
    
    
  # CHECK TRIALIDs ----------------------------------------------------------
  
  # Same name as function
  if (check_trialids == TRUE) {
    
    source(here::here("../jsPsychHelpeR/R/helper_functions_minimal.R"))
    ALL_PROTOCOLS = basename(list.dirs(local_protocols, recursive = FALSE))
    TEST_PROTOCOLS = basename(list.dirs(paste0(local_protocols, "/test/protocols_DEV/"), recursive = FALSE))
    
    
    cli::cli_h1("FOLDER: protocols/")
    OUTPUT_ALL = 1:length(ALL_PROTOCOLS) %>% 
      purrr::map(~  check_trialids(local_folder_protocol = paste0(local_protocols, ALL_PROTOCOLS[.x], "/"), show_all_messages = show_all_messages))
    
    cli::cli_h1("FOLDER: protocols/test/protocols_DEV/")
    OUTPUT_TEST = 1:length(TEST_PROTOCOLS) %>% 
      purrr::map(~  check_trialids(local_folder_protocol = paste0(local_protocols, "/test/protocols_DEV/", TEST_PROTOCOLS[.x], "/"), show_all_messages = show_all_messages))
    
    # CHECK 999
    cli::cli_h1("FOLDER: protocols/999/")
    check_trialids(local_folder_protocol = paste0(local_protocols, "/999/"), show_all_messages = show_all_messages)
    
  }
  

# CHECK all info NEW tasks ------------------------------------------------

    
  if (check_new_task_tabs == TRUE) {
    
    
    # All sheets from NEW to check we have all data
    DF_googledoc_NEW_citas = 
      googlesheets4::read_sheet("1LAsyTZ2ZRP_xLiUBkqmawwnKWgy8OCwq4mmWrrc_rpQ", sheet = 3, skip = 0) %>% 
      dplyr::rename(short_name = `Codigo Test`) %>% 
      dplyr::filter(!grepl("Identico a nombre en pestaña Resumen", short_name)) %>%
      tidyr::drop_na(short_name) %>% 
      dplyr::select(short_name) %>% dplyr::mutate(citas = "")
    
    DF_googledoc_NEW_puntajes = 
      googlesheets4::read_sheet("1LAsyTZ2ZRP_xLiUBkqmawwnKWgy8OCwq4mmWrrc_rpQ", sheet = 4, skip = 0) %>% 
      dplyr::rename(short_name = `Codigo Test`) %>% 
      dplyr::filter(!grepl("Identico a nombre en pestaña Resumen", short_name)) %>%
      tidyr::drop_na(short_name) %>% 
      dplyr::select(short_name) %>% dplyr::mutate(puntajes = "")
    
    DF_googledoc_NEW_dimensiones = 
      googlesheets4::read_sheet("1LAsyTZ2ZRP_xLiUBkqmawwnKWgy8OCwq4mmWrrc_rpQ", sheet = 5, skip = 0) %>% 
      dplyr::rename(short_name = `Codigo Test`) %>% 
      dplyr::filter(!grepl("Identico a nombre en pestaña Resumen", short_name)) %>%
      tidyr::drop_na(short_name) %>% 
      dplyr::select(short_name) %>% dplyr::mutate(dimensiones = "")
    
    
  
    # MISSING tabs in NEW tasks -----------------------------------------------
    
    cli::cli_h1("CHECK missing info in tabs")
    
    DF_all_NEW = 
      DF_googledoc_NEW %>% dplyr::select(short_name, EMAIL) %>% dplyr::mutate(resumen = "") %>% 
      dplyr::left_join(DF_googledoc_NEW_citas, by = "short_name", multiple = "all") %>% 
      dplyr::left_join(DF_googledoc_NEW_puntajes, by = "short_name", multiple = "all") %>% 
      dplyr::left_join(DF_googledoc_NEW_dimensiones, by = "short_name", multiple = "all") %>% 
      dplyr::distinct(short_name, .keep_all = TRUE) %>% 
      dplyr::filter(is.na(resumen) | is.na(citas) | is.na(puntajes) | is.na(dimensiones)) %>% 
      tidyr::replace_na(replace = list(resumen = "resumen", citas = "citas", puntajes = "puntajes", dimensiones = "dimensiones")) %>% 
      dplyr::mutate(TEXT = paste0(resumen, ", ", citas, ", ", puntajes, ", ", dimensiones, sep = ", ")) %>% 
      dplyr::select(short_name, EMAIL, TEXT) %>% 
      dplyr::mutate(TEXT = gsub("  |^ | $| ,|^ ,|^,|, $", "", TEXT),
             TEXT = trimws(TEXT)) # Remove white space at the begining and end of the string
    
    
    MISSING_n = 
      DF_all_NEW %>% 
      dplyr::count(EMAIL) %>% 
      dplyr::mutate(TEXT = paste0(EMAIL, ": ", n)) %>% 
      dplyr::pull(TEXT)
    
    MISSING_tabs = 
      DF_all_NEW %>% 
      dplyr::group_by(EMAIL) %>% 
      dplyr::summarise(tasks = paste0(paste0(short_name, " (", TEXT, ") "), collapse = "; "),
                TEXT = unique(TEXT), 
                .groups = "drop") %>% 
      dplyr::mutate(TEXT = paste0(cli::col_cyan(EMAIL), ": ", tasks)) %>% 
      dplyr::distinct(TEXT, .keep_all = TRUE) # Delete duplicates (when tasks with different missing pieces for same EMAIL)
      
    
    cli::cli_alert_danger("Tasks missing info in tabs: ")
    cli::cli_li(MISSING_n)
    cli::cli_h2("Details")
    cli::cli_li(MISSING_tabs$TEXT)
    
  } else {
    MISSING_tabs = NULL
  }
    
    # Unique names ------------------------------------------------------------
    
    cli::cli_h1("CHECK duplicates")
    # Check we don't have duplicate names
    count_TASK_names = DF_googledoc %>% dplyr::count(short_name) %>% dplyr::filter(n != 1)
    if (nrow(count_TASK_names) != 0) { 
      cli::cli_alert_danger("DUPLICATED short_name: {paste(count_TASK_names$short_name, collapse = '; ')}")
    } else {
      cli::cli_alert_success("No duplicated short_name in the Google docs")
    }
    

  # OUTPUT ------------------------------------------------------------------

  DF_FINAL = 
    DF_tasks %>% 
    dplyr::full_join(DF_missing_script %>% dplyr::mutate(missing_script = task) %>% dplyr::select(-protocols), by = c("task")) %>% 
    dplyr::left_join(DF_missing_googledoc %>% dplyr::mutate(missing_gdoc = task) %>% dplyr::select(-protocols), by = c("task")) %>% 
    dplyr::bind_rows(DF_missing_JS_tasks %>% dplyr::mutate(missing_task = task) %>% dplyr::select(-Nombre, -Descripcion)) %>% 
    dplyr::left_join(DF_googledoc1 %>% dplyr::bind_rows(DF_googledoc_NEW), by = c("task" = "short_name"), multiple = "all") %>% 
    dplyr::select(task, dplyr::starts_with("missing"), dplyr::everything())
  

  OUTPUT = 
    list(DF_FINAL = DF_FINAL,
         DF_missing_JS_tasks = DF_missing_JS_tasks,
         DF_missing_script = DF_missing_script,
         DF_missing_googledoc = DF_missing_googledoc,
         MISSING_tabs = MISSING_tabs)
  
  return(OUTPUT)
  
}
