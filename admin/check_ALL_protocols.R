# SYNC ALL protocols to CSCN-server ---------------------------------------

# Will sync all protocols on server to the LOCAL folder ../CSCN-server

  # Then, will CHECK:  
  # - Tasks with no prepare_TASK() script!
  # - Tasks NOT in Google Doc
  # - Check trialid's are OK
  # - Check no missing info in Google doc of NEW tasks


# DOWNLOAD AND CHECK -----------------------------------------------------

  # Download protocols WITHOUT data
  invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))

  DF_missing = 
    check_missing_prepare_TASK(
      sync_protocols = TRUE,
      check_trialids = TRUE,
      check_new_task_tabs = TRUE,
      delete_nonexistent = TRUE,
      show_all_messages = FALSE
    )


# SHOW results ------------------------------------------------------------

  # SHOW tasks with something missing
  DF_missing$DF_FINAL %>% 
    filter(!is.na(missing_script) | !is.na(missing_gdoc) | !is.na(missing_task)) %>% 
    DT::datatable()
  
  # Tasks ready to create prepare_*.R script   
  DF_missing$DF_FINAL %>% filter(!is.na(missing_script) & is.na(missing_gdoc))
  
  # Tasks, protocols and emails for tasks with missing components
  DF_missing$DF_FINAL %>% filter(!is.na(missing_script) | !is.na(missing_gdoc)) %>% 
    filter(!task %in% c("DEMOGR24", "DEMOGRfondecyt2022E1", "ITC", "fauxPasEv")) %>%   # "MDDF_respaldo", "mic_test", "faux_pas",
    select(-matches("missing"), -Nombre, -Descripcion) %>%  View()
  
