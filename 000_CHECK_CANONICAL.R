### TODO --

  # - Unzip snapshots to 'tests/testthat/_snaps/snapshots' before running the pipeline
  # - Are we using tests/manual_correction?

# SYNC ALL protocols to CSCN-server ---------------------------------------

  # Load all R/ functions
  invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))
  
  # CHECK:  
    # - Tasks with no prepare_TASK() script!
    # - Tasks NOT in Google Doc
    # - Check trialid's are OK
  DF_missing = check_missing_prepare_TASK(sync_protocols = TRUE, check_trialids = TRUE, delete_nonexistent = TRUE)
  
  # DF_missing$DF_FINAL %>% View
  DF_missing$DF_FINAL %>% tidyr::replace_na(list(missing_script = "", 
                                                 missing_googledoc = "",
                                                 missing_task = "")) %>% write_csv("dev/DF_missing.csv")

  
  # Tasks ready to create prepare_*.R script   
  DF_missing$DF_FINAL %>% filter(!is.na(missing_script) & is.na(missing_googledoc))
  DF_missing$DF_FINAL %>% filter(!is.na(missing_script) | !is.na(missing_googledoc)) %>% 
    filter(!task %in% c("DEMOGR24", "DEMOGRfondecyt2022E1", "ITC", "fauxPasEv")) %>%  # "MDDF_respaldo", "mic_test", "faux_pas",
    select(-matches("missing"), -Nombre, -Descripcion) %>% #View
    write_csv("dev/missing_tasks.csv")
  
  
  
# Sync canonical_protocol_DEV to 999 and test ------------------------------
  
    
  # 0) RENAME config_CANONICAL.js to config.js
    
     # - CS of config.js
     # - RENAME
   
    
  # 1) UPLOAD jsPsychMaker/canonical_protocol to http://cscn.uai.cl/lab/public/instruments/protocols/999 ----------------------------
  
    # Upload canonical protocol to 999
    sync_server_local(direction = "local_to_server", server_folder = "999", local_folder = "../jsPsychMaker/canonical_protocol_DEV")
    
    # DELETE old SERVER 999/.data/ files from server
    list_credentials = source(".vault/.credentials") # Get server credentials
    system(paste0('sshpass -p ', list_credentials$value$password, ' ssh ', list_credentials$value$user, '@', list_credentials$value$IP, ' rm ', list_credentials$value$main_FOLDER, 999, '/.data/*'))
    # system(paste0('sshpass -p ', list_credentials$value$password, ' ssh ', list_credentials$value$user, '@', list_credentials$value$IP, ' rm ', list_credentials$value$main_FOLDER, 'test/canonical_protocol_DEV/', '/.data/*'))
    # system(paste0('sshpass -p ', list_credentials$value$password, ' ssh ', list_credentials$value$user, '@', list_credentials$value$IP, ' rm ', list_credentials$value$main_FOLDER, 'test/tasks_DEV/', '/.data/*'))
  
  
  # 2) jPsychMonkeys: Run 5 monkeys... same ID's, same responses per task (only if no randomization in jsPsych) ----------------------
    
    # REMEMBER: to delete the 999 protocol things in MYSQL
    
    # Open jsPsychMonkeys RStudio project and launch with the parameters below
    rstudioapi::openProject(path = "../jsPsychMonkeys/", newSession = TRUE)
    
    # TODO: SHOULD have a specific sub-script for this. See https://books.ropensci.org/targets/projects.html#multiple-projects
    
    # RUN with: parameters_monkeys_minimal = list(uid = 1:5, server_folder_tasks = "999")
  
  
  # 3) jsPsychHelpeR: prepare data. CHECK differences --------------------------------------------------------------------------------
  
    # Delete old LOCAL 999/ files
    system(paste0('rm data/999/*'))
    # system(paste0('rm data/test/canonical_protocol_DEV/*'))
    
    # Update data from server
    invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))
    run_initial_setup(pid = 999, download_files = TRUE)
    # update_data(id_protocol = "test/canonical_protocol_DEV/")
    # update_data(id_protocol = 999)
    
    # Rename all FILES so the filenames do not change
    FILES_IN = list.files("data/999", full.names = TRUE)
    FILES_OUT = gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{6}", "2222-02-22T222222", FILES_IN)
    file.rename(from = FILES_IN, FILES_OUT)
    
    
    # CHECK we have results files for all tasks
    check_project_and_results(participants = 5,
                              folder_protocol = "../jsPsychMaker/canonical_protocol_DEV/tasks",
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
  