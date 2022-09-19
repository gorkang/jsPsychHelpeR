# Create NEW_TASKS protocol -----------------------------------------------

# In this script we:
  # - Detect tasks for which we do not have CSV's in 999.zip (but have a prepare_TASK.R)
  # - Sync server protocols to local
  # - Create a NEW_TASKS protocol with all the new tasks


# SOME TASKS CAN GIVE ERRORS. 
# - IMPORTANT TO CHECK EVERYTHING OK BEFORE MOVE TO CANONICAL



# LAST RUN ----------------------------------------------------------------

# 2022/07/29
  # OK: jsPsychHelpeR/data/NEW_TASKS.zip
  # NOT OK: "FORM6", "PATTI","DEMOGR3", "FORM4","CEL","MLQ"


# create_protocol_with_missing_in_999.zip ----------------------------------

  # Reads task results in 999.zip
  # Reads ANY tasks available in jsPsychMaker/protocols_DEV/
  # Read all prepare_TASK in jsPSychHelpeR/R_tasks/
  # DOWNLOADS ALL protocols from server to ../CSCN-server/
  # Modify config.js to adapt to new tasks

create_protocol_with_missing_in_999 <- function(search_where = "prepare_TASK", destination_folder) {
  
  cli::cli_alert_info("Loading necessary packages")
  suppressPackageStartupMessages(targets::tar_load_globals())
  
  # We can find csv missing when we have the prepare_TASK.R script or
    # csv missing when we have the tasks/TASK.js script
  if (!search_where %in% c("prepare_TASK", "js")) cli::cli_abort('ONE OF "prepare_TASK", "js"')
  
  
  

  # READ all files ----------------------------------------------------------

  cli::cli_alert_info("Read all `999.zip`, `jsPsychMaker/protocols_DEV/.js` and `R_tasks/prepare_*.R` files")
  
  # Read csv's in 999.zip
  input_files = list.files("data/999", recursive = TRUE, full.names = TRUE) 
  files = read_zips(input_files) %>% distinct(procedure) %>% pull(procedure)
  
  # Read all .js tasks in jsPsychMaker/protocols_DEV/
  JS_missing_CSV =
    list.files("../jsPsychMaker/protocols_DEV/", pattern = "js$", recursive = TRUE) %>% as_tibble()  %>% 
    filter(grepl("*tasks/.*\\.js", value)) %>% 
    mutate(task = gsub("\\.js", "", basename(value))) %>% 
    filter(!task %in% files) %>% 
    filter(!task %in% c("TEMPLATE", "TEMPLATE_OLD")) %>% pull(task) 
  
  # Read all prepare_TASK in jsPSychHelpeR/R_tasks/
  TASKS_missing_CSV = 
    list.files("R_tasks/") %>% as_tibble() %>% 
    mutate(task = gsub("prepare_(.*)\\.R", "\\1", value)) %>% 
    filter(!task %in% files & !grepl("\\.csv", value)) %>% 
    filter(!task %in% c("TEMPLATE", "TEMPLATE_OLD")) %>% pull(task) 
  
  if (search_where == "prepare_TASK") {
    NEW_TASKS = paste0(TASKS_missing_CSV, ".js")    
  } else {
    NEW_TASKS = paste0(JS_missing_CSV, ".js")  
  }
  
  
  # Remove TASKS ***
    # DEMOGR[N] and FORM[N] # Idiosyncratic tasks
    # SCGT uses mic (mic not available in docker container browser)
  blacklist_tasks = "^DEMOGR|^FORM[0-9]{1,3}|^SCGT|^PPD"
  cli::cli_alert_info("Not including tasks (e.g. SCGT uses mic): {blacklist_tasks}")
  NEW_TASKS = NEW_TASKS[!grepl(blacklist_tasks, NEW_TASKS)]
  

# Get last version from SERVER --------------------------------------------

  # Download to ../CSCN-server/
  invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))
  DF_missing = check_missing_prepare_TASK(sync_protocols = TRUE) 
  DF_missing
  
  
# COPY a clean protocol and the new tasks to NEW_TASKS --------------------
  
  cli::cli_alert_info("Create new protocol in /CSCN-server/")
  
  ALL_js = 
    list.files("../CSCN-server/", recursive = TRUE, pattern = "\\.js") %>% as_tibble() %>% 
    filter(!grepl("NEW_TASKS", value)) %>% # Do not look into NEW_TASKS to avoid circularity
    filter(grepl("*tasks/.*\\.js", value)) %>% 
    mutate(task_name = basename(value)) %>% 
    mutate(mtime = file.info(paste0("../CSCN-server/", value))$mtime,
           size = file.info(paste0("../CSCN-server/", value))$size)
    
  # DT::datatable(ALL_js)
  
  # Select newer versions of tasks
  PATHS_NEW_TASKS = 
    ALL_js %>% 
    filter(task_name %in% NEW_TASKS) %>% 
    group_by(task_name) %>% filter(mtime == max(mtime)) %>% 
    distinct(task_name, .keep_all = TRUE) %>% 
    arrange(task_name) %>% 
    pull(value)
  
  

  # DELETE OLD NEW_TASKS
  unlink(destination_folder, recursive = TRUE)
  cli::cli_alert_info("Deleted {destination_folder}")
  
  source("admin/helper-scripts-admin.R")
  copy_canonical_clean(destination_folder = destination_folder)
  
  # Remove non essential tasks from canonical_clean
  TASKS_CLEAN = list.files(paste0(destination_folder, "/tasks/"))
  file.remove(paste0(destination_folder, "/tasks/", TASKS_CLEAN[!TASKS_CLEAN %in% c("Consent.js", "Goodbye.js")]))
  
  # Copy NEW tasks
  # dir.create(paste0(destination_folder, "tasks/"), recursive = TRUE)
  file.copy(paste0("../CSCN-server/", PATHS_NEW_TASKS), paste0(destination_folder, "tasks/"), overwrite = TRUE)
  
  
  # INCLUDE NEW TASKS IN config.js -----------------------------------
  
  TASKS_NEW_PROTOCOL = extract_tasks_from_protocol(destination_folder)
  
  replace_tasks_config_js(folder_protocol = destination_folder,
                          tasks = TASKS_NEW_PROTOCOL, 
                          block_tasks = "randomly_ordered_tasks_1") 

  cli::cli_alert_success("ALL DONE \nProtocol in `{destination_folder}` \nRemember to CHECK config.js")
  
}


# Using the missing prepare_tasks()
# create_protocol_with_missing_in_999(search_where = "prepare_TASK")



# CREATE PROTOCOL ---------------------------------------------------------

FOLDER_PROTOCOL = "../CSCN-server/protocols/test/protocols_DEV/NEW_TASKS/"
create_protocol_with_missing_in_999(search_where = "js", destination_folder = FOLDER_PROTOCOL)



# CHECK CONFIG ------------------------------------------------------------

  # CHECK config.js
    # - online
    # - pid
  rstudioapi::navigateToFile("../CSCN-server/protocols/test/protocols_DEV/NEW_TASKS/config.js")

# TASKS protocolo Colombia-Chile
  # 'CTT', 'CIT', 'CRQ', 'ICvsID', 'LSNS', 'MDDF', 'PSC', 'UCLA'
  # //'SCGT', ESTA USA MICRO. CORRER MANUALMENTE (no funciona en Docker)
  


# UPLOAD NEW_TASKS to server --------------------------------------------------

  source("../jsPsychMaker/admin/sync_server_local.R")
  
  # UPLOAD CSCN-server/.../NEW_TASKS to server
  sync_server_local(direction = "local_to_server", 
                    local_folder = FOLDER_PROTOCOL,
                    server_folder = "test/protocols_DEV/NEW_TASKS/", only_test = FALSE)
  
  
  

# DELETE .data in server --------------------------------------------------

  # DELETE SERVER "test/protocols_DEV/NEW_TASKS/" .data/ files
  list_credentials = source(".vault/.credentials") # Get server credentials
  system(paste0('sshpass -p ', list_credentials$value$password, ' ssh ', list_credentials$value$user, '@', list_credentials$value$IP, ' rm ', list_credentials$value$main_FOLDER, "test/protocols_DEV/NEW_TASKS/", '/.data/*'))
  

# DOWNLOAD to protocols_DEV -----------------------------------------------
  
  # TODO: Can just copy instead of download
    # FROM: ../CSCN-server/protocols/test/protocols_DEV/NEW_TASKS/
    # TO: ../jsPsychMaker/protocols_DEV/NEW_TASKS/
      
  # DOWNLOAD to NEW_TASKS to ../jsPsychMaker/protocols_DEV/NEW_TASKS/
  # To make sure the Github jsPsychMaker/protocols_DEV/NEW_TASKS is up to date
  sync_server_local(direction = "server_to_local", 
                    server_folder = "test/protocols_DEV/NEW_TASKS/",
                    local_folder = "../jsPsychMaker/protocols_DEV/NEW_TASKS/", 
                    only_test = FALSE)
  
  cli::cli_h1("REMEMBER")
  cli::cli_alert_info("Commit and PUSH NEW_TASKS changes in jsPsychMaker!!!")
  rstudioapi::openProject(path = "../jsPsychMaker/", newSession = TRUE)


# MONKEYS -----------------------------------------------------------------

# DELETE protocol 999 MYSQL rows
  system("mysql-workbench")
  # system("mysql-workbench-community")
  
# Launch monkeys! # Go to jsPsychMonkeys and use the following in _targets.R
rstudioapi::openProject(path = "../jsPsychMonkeys/", newSession = TRUE)

parameters_monkeys_minimal = list(uid = 100:105, uid_URL = TRUE, 
                                  forced_seed = 11, # Reproducible. Comment for random
                                  forced_random_wait = TRUE, forced_refresh = TRUE,
                                  initial_wait = 5,
                                  wait_retry = 5, # Increase if fails
                                  server_folder_tasks = "test/protocols_DEV/NEW_TASKS/",
                                  big_container = TRUE, debug_file = FALSE, console_logs = TRUE, debug = TRUE, 
                                  open_VNC = TRUE, keep_alive = TRUE,
                                  disable_web_security = FALSE)





# SETUP HELPER ------------------------------------------------------------

  # Delete OLD data
  OLD_data = list.files("data/test/protocols_DEV/NEW_TASKS/", full.names = TRUE)
  file.remove(OLD_data)
  
  # Download new data and setup Helper
  run_initial_setup(pid = "test/protocols_DEV/NEW_TASKS/", 
                    download_files = TRUE, 
                    download_task_script = TRUE)
