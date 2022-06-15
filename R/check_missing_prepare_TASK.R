# Run this to Download all protocols (minus csv's) to a local folder, and then check for which ones we do not have preparation scripts
check_missing_prepare_TASK <- function(sync_protocols = FALSE, check_trialids = FALSE) {
  
  
  # DEBUG
  # sync_protocols = FALSE

  targets::tar_load_globals()
  
  # Download all protocols ------------------------------------------------
  
  if (sync_protocols == TRUE) {
    # https://unix.stackexchange.com/questions/432801/rsync-exclude-a-certain-file-extension-unless-zipped
    sync_server_local(server_folder = "", 
                      local_folder = "../CSCN-server/protocols/", 
                      direction = "server_to_local", 
                      only_test = !sync_protocols, # we do !sync_protocols because the parameter is only_test (!)
                      exclude_csv = TRUE) # DO NOT INCLUDE DATA
  }


# Unique tasks -----------------------------------------------------------

  ALL_files = list.files("../CSCN-server/protocols/", pattern = "*.js", full.names = TRUE, recursive = TRUE)
  DF_files =
    ALL_files[grepl("*tasks/.*\\.js", ALL_files)] %>% as_tibble() %>% 
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


# Tasks without correction scripts ----------------------------------------

  # List
  missing_tasks = DF_files$task[!DF_files$task %in% DF_scripts$script]
  
  # DF
  DF_missing_tasks = DF_files %>% filter(task %in% missing_tasks) %>% arrange(task)
 
#   # List
#   missing_tasks = DF_files %>% filter(task %in% missing_tasks) %>% arrange(task) %>% pull(task) %>% cat()


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

  OUTPUT = 
    list(missing_tasks = missing_tasks,
         DF_missing_tasks = DF_missing_tasks)
  return(OUTPUT)
  
}