
# Create NEW_TASKS protocol -----------------------------------------------

# In this script we:
  # - Detect tasks for which we do not have CSV's in 999.zip (but have a prepare_TASK.R)
  # - Sync server protocols to local
  # - Create a NEW_TASKS protocol with all the new tasks


# ALGUNAS TAREAS PUEDEN DAR ERRORES. IMPORTANTE COMPROBAR QUE TODO OK ANTES DE PASAR A CANONICAL


# LAST RUN ----------------------------------------------------------------

# 2022/07/29
  # OK: jsPsychHelpeR/data/NEW_TASKS.zip
  # NOT OK: "FORM6", "PATTI","DEMOGR3", "FORM4","CEL","MLQ"


# Missing CSV's in 999.zip -----------------------------------------------------
  
find_missing_tasks_in_999 <- function(search_where = "prepare_TASK") {
  targets::tar_load_globals()
  # We can find csv missing when we have the prepare_TASK.R script or
    # csv missing when we have the tasks/TASK.js script
  if (!search_where %in% c("prepare_TASK", "js")) cli::cli_abort('ONE OF "prepare_TASK", "js"')
  
  # For tasks with a prepare_TASK script
  
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
  blacklist_tasks = "DEMOGR[0-9]{1,3}|FORM[0-9]{1,3}|SCGT|PPD"
  cli::cli_alert_info("Not including tasks: {blacklist_tasks}")
  NEW_TASKS = NEW_TASKS[!grepl(blacklist_tasks, NEW_TASKS)]
  

# Get last version from SERVER --------------------------------------------

  invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))
  DF_missing = check_missing_prepare_TASK(sync_protocols = TRUE) # Download to ../CSCN-server/
  DF_missing
  
  
# COPY a clean protocol and the new tasks to NEW_TASKS --------------------
  
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
  
  
  destination_folder = "../CSCN-server/protocols/test/protocols_DEV/NEW_TASKS/"
  
  source("admin/helper-scripts-admin.R")
  copy_canonical_clean(destination_folder = destination_folder)
  
  # # canonical_protocol_clean files
  # canonical_folder = "/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/CSCN-server/protocols/test/canonical_protocol_clean/"
  # canonical_files = list.files(canonical_folder, full.names = FALSE, recursive = TRUE)
  # 
  # # Copy canonical_protocol_clean files to NEW_TASKS
  # folders_to_create = unique(paste0(destination_folder, dirname(canonical_files)))
  # walk(folders_to_create, dir.create, recursive = TRUE)
  # file.copy(paste0(canonical_folder, canonical_files), paste0(destination_folder, canonical_files), overwrite = TRUE)

  
  
  # Copy NEW tasks
  dir.create(paste0(destination_folder, "tasks/"), recursive = TRUE)
  file.copy(paste0("../CSCN-server/", PATHS_NEW_TASKS), paste0(destination_folder, "tasks/"), overwrite = TRUE)
  
  
# INCLUDE NEW TASKS IN config.js -----------------------------------
  
  TASKS_vector = paste0("randomly_ordered_tasks_1 = ['", paste(gsub("\\.js", "", NEW_TASKS), collapse = "', '"), "'];")
  CONFIG = readLines("../CSCN-server/protocols/test/protocols_DEV/NEW_TASKS/config.js")
  
  # Replace
  final_file = gsub("randomly_ordered_tasks_1 = \\['DEMOGR', 'AIM'\\]; // Block of tasks in random order",
                    TASKS_vector, CONFIG)

  # Write file
  cat(final_file, file = "../CSCN-server/protocols/test/protocols_DEV/NEW_TASKS/config.js", sep = "\n")
  

}

# find_missing_tasks_in_999(search_where = "prepare_TASK")
find_missing_tasks_in_999(search_where = "js")



# UPLOAD NEW_TASKS to server --------------------------------------------------

# CHECK config.js first
rstudioapi::navigateToFile("../CSCN-server/protocols/test/protocols_DEV/NEW_TASKS/config.js")

source("../jsPsychMaker/R/sync_server_local.R")

# UPLOAD CSCN-server/.../NEW_TASKS to server
sync_server_local(direction = "local_to_server", 
                  local_folder = "../CSCN-server/protocols/test/protocols_DEV/NEW_TASKS/",
                  server_folder = "test/protocols_DEV/NEW_TASKS/", only_test = FALSE)


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
system("mysql-workbench-community")

# Launch monkeys! # Go to jsPsychMonkeys and use the following in _targets.R
rstudioapi::openProject(path = "../jsPsychMonkeys/", newSession = TRUE)

parameters_monkeys_minimal = list(uid = 100:105, uid_URL = TRUE, forced_seed = 11,
                                  forced_random_wait = TRUE, forced_refresh = TRUE,
                                  initial_wait = 5,
                                  wait_retry = 10,
                                  server_folder_tasks = "test/protocols_DEV/NEW_TASKS/",
                                  big_container = TRUE, debug_file = FALSE, console_logs = TRUE, debug = TRUE, 
                                  open_VNC = TRUE, keep_alive = TRUE,
                                  disable_web_security = FALSE)




# RENAME CSVs -------------------------------------------------------------

# Renombrar zips
# FILES_IN = list.files("data/999", full.names = TRUE)
# FILES_OUT = gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{6}", "2222-02-22T222222", FILES_IN)
# file.rename(from = FILES_IN, FILES_OUT)

