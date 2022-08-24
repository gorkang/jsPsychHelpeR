#' extract_tasks_from_protocol
#' Extract the list of scripts in the task folder of a protocol, and a vector 
#' with the names of the tasks
#'
#' @param folder_protocol 
#'
#' @return
#' @export
#'
#' @examples
extract_tasks_from_protocol <- function(folder_protocol) {
  
  library(dplyr)
  
  # DEBUG
  # folder_protocol = "../CSCN-server/protocols/999/"
  
  ALL_js = 
    list.files(folder_protocol, recursive = TRUE, pattern = "\\.js") %>% as_tibble() %>% 
    filter(!grepl("NEW_TASKS", value)) %>% # Do not look into NEW_TASKS to avoid circularity
    filter(grepl("*tasks/.*\\.js", value)) %>% 
    mutate(task_name = basename(value)) %>% 
    mutate(mtime = file.info(paste0(folder_protocol, "/tasks/", task_name))$mtime,
           size = file.info(paste0(folder_protocol, "/tasks/", task_name))$size)
  
  # Select newer versions of tasks
  PATHS_tasks = 
    ALL_js %>% 
    # filter(task_name %in% NEW_TASKS) %>% 
    group_by(task_name) %>% filter(mtime == max(mtime)) %>% 
    distinct(task_name, .keep_all = TRUE) %>% 
    arrange(task_name) %>% 
    pull(value)
  
  
  list(PATHS_tasks = PATHS_tasks,
       tasks = gsub("\\.js", "", basename(PATHS_tasks)))
  
}



#' replace_tasks_config_js
#' Uses a vector of tasks to replace one of the task blocks in config.js
#'
#' @param folder_protocol 
#' @param tasks String vector of tasks
#' @param block_tasks 
#'
#' @return
#' @export
#'
#' @examples
replace_tasks_config_js <- function(folder_protocol, tasks, block_tasks = "randomly_ordered_tasks_1") {
  
  # DEBUG
  # folder_protocol = "../jsPsychMaker/canonical_protocol/"
  # block_tasks = "randomly_ordered_tasks_1"
  # tasks = extract_tasks_from_protocol(folder_protocol)
  
  
  # CHECK block_tasks
  allowed_blocks = c("first_tasks", "last_tasks", paste0(gsub("1$", "", "randomly_ordered_tasks_1"), 1:5), paste0(gsub("1$", "", "secuentially_ordered_tasks_1"), 1:5))
  if (!block_tasks %in% allowed_blocks) cli::cli_abort("{.var block_tasks} needs to be one of the following: {allowed_blocks}")
  
  
  config_file = paste0(folder_protocol, "/config.js")
  
  
  # CONFIG[which(grepl("first_tasks = ", CONFIG))]
  
  tasks_clean = tasks$tasks[!tasks$tasks %in% c("Consent", "Goodbye")]
  
  TASKS_vector = paste0(block_tasks, " = ['", paste(gsub("\\.js", "", tasks_clean), collapse = "', '"), "'];")
  CONFIG = readLines(config_file) #"../CSCN-server/protocols/test/protocols_DEV/NEW_TASKS/config.js")
  
  # Replace
  CONFIG[which(grepl(paste0(block_tasks, " = "), CONFIG))] = TASKS_vector
  final_file = CONFIG
  
  # final_file = gsub(paste0(block_tasks, " = \\['DEMOGR', 'AIM'\\]; // Block of tasks in random order"),
  #                   TASKS_vector, CONFIG)
  
  cli::cli_alert_info("Replaced {block_tasks} in {config_file} with:\n - {TASKS_vector}")
  
  # Write file
  cat(final_file, file = config_file, sep = "\n")
  
}


copy_canonical_clean <- function(destination_folder) {
  # destination_folder = "../CSCN-server/protocols/test/protocols_DEV/NEW_TASKS/"
  
  library(purrr)
  if (!grepl("/$", destination_folder)) destination_folder = paste0(destination_folder, "/")
  
  
  # canonical_protocol_clean files
  canonical_folder = "/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychMaker/canonical_protocol_clean/"
  canonical_files = list.files(canonical_folder, full.names = FALSE, recursive = TRUE)
  
  # Copy canonical_protocol_clean files to NEW_TASKS
  folders_to_create = unique(paste0(destination_folder, dirname(canonical_files)))
  walk(folders_to_create, dir.create, recursive = TRUE, showWarnings = FALSE)
  file.copy(paste0(canonical_folder, canonical_files), paste0(destination_folder, canonical_files), overwrite = TRUE)
  
}

