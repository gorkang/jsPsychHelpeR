# Checklist para pasar protocolos de test/protocols_DEV/ a produccion

  # PARAMETERS --------------------------------
  # Debe estar en test/protocols_DEV/!!!
  PROTOCOLID = "test/protocols_DEV/999"
  # -------------------------------------------
  
  # Sources and credentials
  invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))
  source("admin/helper-scripts-admin.R")
  pid = gsub("test/protocols_DEV/", "", PROTOCOLID)
  cli::cli_h1("PROTOCOL {pid}")
  
  
  # 1) Pilotaje final on Monkeys! -------------------------------------------
  
    # Launch monkeys
    rstudioapi::openProject(path = "../jsPsychMonkeys/", newSession = TRUE)
    
    # Delete LOCAL data/PROTOCOLID files
    system(paste0('rm data/', PROTOCOLID, '/*'))
  
    # jsPsychHelper WORKS?
    run_initial_setup(pid = PROTOCOLID, download_files = TRUE)
    targets::tar_make()
  
  
  # 2) Limpiar la base de datos --------------------------------------------
  
    # Delete the XYZ protocol rows in all the MYSQL tables
    source("admin/mysql_helper_functions.R")
    # list_credentials = decrypt_data(key_public = readLines(".vault/data_public_key.txt"), data_encrypted = ".vault/data_encrypted.rds", mysupersecretpassword = rstudioapi::askForPassword())
    delete_MySQL_tables_pid(pid)
    
    # system("mysql-workbench")
    # system("mysql-workbench-community")
    
  
  # 3) Limpiar los archivos de resultados de Monkeys -----------------------
    
    # Delete csv files in .data/
    DELETE_data_server(pid = PROTOCOLID)
    
    
  # 4) Revisar el config.js para pasar el experimento a produccion ----------
    
    -[] online = true
    -[] pid OK?
    -[] debug_mode = false
    - ETC...
    
  
  # 5) Copiar protocolo ZIPeado a test/protocols_DEV/OLD_TESTS/ -------------  
    
    # TODO: automatico!
    
  # 6) Copiar protocolo a protocols/ ----------------------------------------
    
    # TODO: automatico!
  
  # 7) BORRAR protocolo de test/protocols_DEV/OLD_TESTS/ --------------------
    
    # TODO: automatico!
    