# SYNC ALL protocols to CSCN-server ---------------------------------------

invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))

# CHECK:  
# - Tasks with no prepare_TASK() script!
# - Tasks NOT in Google Doc
# - Check trialid's are OK
# - Check no missing info in Google doc of NEW tasks
DF_missing = 
  check_missing_prepare_TASK(
    sync_protocols = TRUE,
    check_trialids = TRUE,
    check_new_task_tabs = TRUE,
    delete_nonexistent = TRUE,
    show_all_messages = FALSE
  )

DF_missing$DF_FINAL %>% DT::datatable()
# DF_missing$DF_FINAL %>% tidyr::replace_na(list(missing_script = "", missing_googledoc = "", missing_task = "")) %>% write_csv("dev/DF_missing.csv")


# Tasks ready to create prepare_*.R script   
DF_missing$DF_FINAL %>% filter(!is.na(missing_script) & is.na(missing_gdoc))
DF_missing$DF_FINAL %>% filter(!is.na(missing_script) | !is.na(missing_gdoc)) %>% 
  filter(!task %in% c("DEMOGR24", "DEMOGRfondecyt2022E1", "ITC", "fauxPasEv")) %>%  # "MDDF_respaldo", "mic_test", "faux_pas",
  select(-matches("missing"), -Nombre, -Descripcion) %>% View
# write_csv("dev/missing_tasks.csv")
