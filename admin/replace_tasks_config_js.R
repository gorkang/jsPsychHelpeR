replace_tasks_config_js <- function(folder_protocol, tasks, block_tasks = "randomly_ordered_tasks_1") {
  
  # DEBUG
  # folder_protocol = "../jsPsychMaker/canonical_protocol/"
  # block_tasks = "randomly_ordered_tasks_1"
  # tasks = extract_tasks_from_protocol(folder_protocol)

  config_file = paste0(folder_protocol, "/config.js")
  
  
  # CONFIG[which(grepl("first_tasks = ", CONFIG))]
  
  tasks_clean = tasks$tasks[!tasks$tasks %in% c("Consent", "Goodbye")]
  
  TASKS_vector = paste0(block_tasks, " = ['", paste(gsub("\\.js", "", tasks_clean), collapse = "', '"), "'];")
  CONFIG = readLines(config_file) #"../CSCN-server/protocols/test/protocols_DEV/NEW_TASKS/config.js")
  
  # Replace
  CONFIG[which(grepl("randomly_ordered_tasks_1 = ", CONFIG))] = TASKS_vector
  final_file = CONFIG
  
  # final_file = gsub(paste0(block_tasks, " = \\['DEMOGR', 'AIM'\\]; // Block of tasks in random order"),
  #                   TASKS_vector, CONFIG)
  
  cli::cli_alert_info("Replaced {block_tasks} in {config_file} with:\n\n {TASKS_vector}")
  
  # Write file
  cat(final_file, file = config_file, sep = "\n")
  
  
}
