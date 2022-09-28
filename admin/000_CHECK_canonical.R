# CHECK canonical_protocol ------------------------------------------------

# Help checks all tasks in canonical_protocol work as expected

  # 1. Creates a config.js file with all the tasks in ../jsPsychMaker/canonical_protocol/ 
  # 2. Uploads ../jsPsychMaker/canonical_protocol/ to the 999 protocol in the server
  # 3. Deletes the contents of .data/
  # 4. Open Mysql-workbech to delete all 999 tables
  # 5. Opens jsPsychMonkeys to simulate the 5 standard participants
  # 6. Opens jsPsychHelpeR to prepare data (deletes data/999, downloads, renames...)
  # 7. TODO: Compare new RUN with old RUN

# REMEMBER to:
  # test NEW TASKS with admin/create_protocol_with_NEW_tasks.R before moving them to canonical_protocol
  # MOVE SHARED-FONDECYT-Faridy/jsPsychR/NEW_scales-docs to docs and to jsPsychmaker/docs


### TODO --
  # - Unzip snapshots to 'tests/testthat/_snaps/snapshots' before running the pipeline
  # - Are we using tests/manual_correction?
  # STEP 0 SHOULD BE TO CREATE A BACKUP

  # CHECK TWO RUNS ARE IDENTICAL 



# Scripts -----------------------------------------------------------------

  targets::tar_load_globals()
  source("admin/helper-scripts-admin.R")
  source("../jsPsychMaker/admin/sync_server_local.R")


# prepare config.js -------------------------------------------------------

  # Use all available tasks in canonical_protocol to create a new config.js

  tasks_canonical = jsPsychMaker::extract_tasks_from_protocol(folder_protocol = "../jsPsychMaker/canonical_protocol/")
  
  jsPsychMaker::update_config_js(folder_protocol = "../jsPsychMaker/canonical_protocol/",
                                 tasks = tasks_canonical, 
                                 block_tasks = "randomly_ordered_tasks_1")
  

  rstudioapi::navigateToFile("../jsPsychMaker/canonical_protocol/config.js")



# Sync canonical_protocol to 999 and test ------------------------------
  
  # 0) RENAME config_CANONICAL.js to config.js
  
    # rstudioapi::navigateToFile("../jsPsychMaker/canonical_protocol/config.js")
    
     # # CS of config.js
     #  file.copy("../jsPsychMaker/canonical_protocol/config.js", 
     #            "../jsPsychMaker/canonical_protocol/config_BACKUP.js")
     # # RENAME
     #  file.copy("../jsPsychMaker/canonical_protocol/config_CANONICAL.js", 
     #            "../jsPsychMaker/canonical_protocol/config.js")

  # 1) UPLOAD jsPsychMaker/canonical_protocolV to http://cscn.uai.cl/lab/public/instruments/protocols/999 ----------------------------
  
    # Upload canonical protocol to 999
    sync_server_local(direction = "local_to_server", 
                      local_folder = "../jsPsychMaker/canonical_protocol", 
                      server_folder = "999", 
                      only_test = FALSE)
    
    # DELETE SERVER 999/.data/ files
    DELETE_data_server(pid = "999")
    # list_credentials = source(".vault/.credentials") # Get server credentials
    # system(paste0('sshpass -p ', list_credentials$value$password, ' ssh ', list_credentials$value$user, '@', list_credentials$value$IP, ' rm ', list_credentials$value$main_FOLDER, 999, '/.data/*'))

    
  
  # 2) jPsychMonkeys: Run 5 monkeys... same ID's, same responses per task (only if no randomization in jsPsych) ----------------------
    
    # REMEMBER: to delete the 999 protocol rows in all the MYSQL tables
    system("mysql-workbench-community")
    system("mysql-workbench")
    
    
    # Open jsPsychMonkeys RStudio project and launch with the parameters below
    rstudioapi::openProject(path = "../jsPsychMonkeys/", newSession = TRUE)
    # TODO: SHOULD have a specific sub-script for this. See https://books.ropensci.org/targets/projects.html#multiple-projects
    

  
  # 3) jsPsychHelpeR: prepare data. CHECK differences --------------------------------------------------------------------------------
  
    # Delete old LOCAL 999/ files
    system(paste0('rm data/999/*'))
    # system(paste0('rm data/test/canonical_protocol/*'))
    
    # Update data from server
    invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))
    run_initial_setup(pid = 999, download_files = TRUE)
    
    
    # Rename all FILES so the filenames do not change
    FILES_IN = list.files("data/999", full.names = TRUE)
    FILES_OUT = gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{6}", "2222-02-22T222222", FILES_IN)
    file.rename(from = FILES_IN, FILES_OUT)
    
    
    # CHECK we have results files for all tasks
    check_project_and_results(participants = 5,
                              folder_protocol = "../jsPsychMaker/canonical_protocol/tasks",
                              folder_results = "data/999/")
  
    
    # Run project
    targets::tar_destroy(ask = FALSE)
    targets::tar_make()
    
    # Check test file
    # rstudioapi::navigateToFile("outputs/tests_outputs/test-DF_clean.csv")

    
    
# CHECK TWO RUNS ARE IDENTICAL --------------------------------------------
  
  folder1 = "/home/emrys/Downloads/JSPSYCH/jsPsychHelpeR_test-survey/TEST/data999/"
  folder2 = "/home/emrys/Downloads/JSPSYCH/jsPsychHelpeR_test-survey/TEST/data999/"
  
  DF_joined1 = readr::read_csv(paste0(folder1, "/DF_joined.csv"))
  DF_joined2 = readr::read_csv(paste0(folder2, "/DF_joined.csv"))
  DF_clean1 = readr::read_csv(paste0(folder1, "/DF_clean.csv"))
  DF_clean2 = readr::read_csv(paste0(folder2, "/DF_clean.csv"))
  
  waldo::compare(DF_joined1, DF_joined2)
  waldo::compare(DF_clean1, DF_clean2)
  