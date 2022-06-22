# Run this to Download all protocols (minus csv's) to a local folder
# Then check:
# - for which ones we do not have preparation scripts 
# - for which ones we do not have googledoc details in
    # + https://docs.google.com/spreadsheets/d/1Eo0F4GcmqWZ1cghTpQlA4aHsc8kTABss-HAeimE2IqA/edit#gid=0
    # + https://docs.google.com/spreadsheets/d/1LAsyTZ2ZRP_xLiUBkqmawwnKWgy8OCwq4mmWrrc_rpQ/edit#gid=0

check_missing_prepare_TASK <- function(sync_protocols = FALSE, check_trialids = FALSE, delete_nonexistent = FALSE) {
  
  # DEBUG
  # sync_protocols = FALSE
  # check_trialids = FALSE

  
  targets::tar_load_globals()
  
  
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
                      local_folder = "../CSCN-server/protocols/", 
                      direction = "server_to_local", 
                      only_test = !sync_protocols, # we do !sync_protocols because the parameter is only_test (!)
                      exclude_csv = TRUE, # DO NOT INCLUDE DATA
                      delete_nonexistent = delete_nonexistent) # Delete files localy if they are NOT in server anymore
  }

  

  # DATA SOURCES ------------------------------------------------------------
  
  
    # Unique tasks -----------------------------------------------------------
  
      # Unique tasks found in all the protocols
      ALL_tasks = list.files("../CSCN-server/protocols/", pattern = "*.js", full.names = TRUE, recursive = TRUE)
      DF_tasks =
        ALL_tasks[grepl("*tasks/.*\\.js", ALL_tasks)] %>% as_tibble() %>% 
        filter(!grepl("OLD_TESTS", value)) %>% 
        mutate(task = gsub("\\.js", "", basename(value)),
               protocol = gsub(".*\\/(test\\/.*)\\/tasks\\/.*|.*\\/(.*)\\/tasks\\/.*", "\\1\\2", value)) %>% 
          group_by(task) %>% 
          summarise(protocols = paste(unique(protocol), collapse = ", ")) %>% 
        distinct(task, .keep_all = TRUE)
  
  
    # Unique correction scripts -----------------------------------------------
  
      ALL_scripts = list.files("R_tasks/", pattern = "*.R", full.names = TRUE, recursive = TRUE)
      DF_scripts = 
        ALL_scripts %>% as_tibble() %>% 
        mutate(script = gsub("prepare_(.*)\\.R", "\\1", basename(value))) %>% 
        distinct(script)
  
  
    # Google doc --------------------------------------------------------------
    
      # Reads canonical googledoc and googledoc with NEW tasks and combines them
      
      googlesheets4::gs4_auth("gorkang@gmail.com")
      DF_googledoc1 = googlesheets4::read_sheet("1Eo0F4GcmqWZ1cghTpQlA4aHsc8kTABss-HAeimE2IqA", sheet = 2, skip = 0) %>% 
        rename(short_name = `Código Test`) %>% 
        filter(short_name != "short_name") %>% 
        arrange(short_name) %>% 
        select(short_name, Nombre, Descripcion) %>% 
        tidyr::drop_na(short_name)
      
      DF_googledoc_NEW = googlesheets4::read_sheet("1LAsyTZ2ZRP_xLiUBkqmawwnKWgy8OCwq4mmWrrc_rpQ", sheet = 2, skip = 0) %>% 
        rename(short_name = `Código Test`) %>% 
        filter(short_name != "short_name: NO debe contener espacios ni caracteres extraños :)") %>% 
        arrange(short_name) %>% 
        select(short_name, Nombre, Descripcion) %>% 
        tidyr::drop_na(short_name)
      
      DF_googledoc = DF_googledoc1 %>% bind_rows(DF_googledoc_NEW) %>% distinct(short_name)
      
    

  # MISSING -----------------------------------------------------------------

    # Tasks without correction scripts --------------------------------------
  
    missing_script = DF_tasks$task[!DF_tasks$task %in% DF_scripts$script]
    DF_missing_script = DF_tasks %>% filter(task %in% missing_script) %>% arrange(task)
   
  
    # Tasks without info on google docs -------------------------------------
  
    missing_tasks_googledoc = DF_tasks$task[!DF_tasks$task %in% DF_googledoc$short_name]
    DF_missing_googledoc = DF_tasks %>% filter(task %in% missing_tasks_googledoc) %>% arrange(task)
    
  
    # In google docs not yet implemented -----------------------------------
    
    missing_googledoc_tasks = DF_googledoc_NEW$short_name[!DF_googledoc_NEW$short_name %in% DF_tasks$task]
    DF_missing_tasks = DF_googledoc_NEW %>% filter(short_name %in% missing_googledoc_tasks) %>% arrange(short_name) %>% 
      rename(task = short_name)
    
    
    
  # CHECK TRIALIDs ----------------------------------------------------------
  
  if (check_trialids == TRUE) {
    
    source("../jsPsychMaker/R/helper_functions.R")  
    ALL_PROTOCOLS = basename(list.dirs("../CSCN-server/protocols/", recursive = FALSE))
    TEST_PROTOCOLS = basename(list.dirs("../CSCN-server/protocols/test/", recursive = FALSE))
    
    
    OUTPUT_ALL = 1:length(ALL_PROTOCOLS) %>% 
      map(~  check_trialids(local_folder_protocol = paste0("../CSCN-server/protocols/", ALL_PROTOCOLS[.x], "/")))
    
    OUTPUT_TEST = 1:length(TEST_PROTOCOLS) %>% 
      map(~  check_trialids(local_folder_protocol = paste0("../CSCN-server/protocols/test/", TEST_PROTOCOLS[.x], "/")))
    
    # CHECK 999
    check_trialids(local_folder_protocol = paste0("../CSCN-server/protocols/999/"))
    
    # - experiment: RMET.js 
    # - trialid:    question001_1, question001_2 
    
    # NOMBRE TAREA MAL!!!  
    # - experiment: faux_pas.js 
    # - trialid:    faux_pas_001 
    
    # - experiment: Goodbye.js, IRS.js 
    # - trialid:    question001, effort 
      
  }
  

  # OUTPUT ------------------------------------------------------------------

  DF_FINAL = 
    DF_tasks %>% 
    full_join(DF_missing_script %>% mutate(missing_script = task) %>% select(-protocols), by = c("task")) %>% 
    left_join(DF_missing_googledoc %>% mutate(missing_googledoc = task) %>% select(-protocols), by = c("task")) %>% 
    bind_rows(DF_missing_tasks %>% mutate(missing_task = task) %>% select(-Nombre, -Descripcion)) %>% 
    left_join(DF_googledoc1 %>% bind_rows(DF_googledoc_NEW), by = c("task" = "short_name")) %>% 
    select(task, starts_with("missing"), everything())
  

  OUTPUT = 
    list(DF_FINAL = DF_FINAL,
         DF_missing_tasks = DF_missing_tasks,
         DF_missing_script = DF_missing_script,
         DF_missing_googledoc = DF_missing_googledoc)
  
  return(OUTPUT)
  
}
