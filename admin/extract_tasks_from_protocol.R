extract_tasks_from_protocol <- function(folder_protocol) {
  
  # DEBUG
  # folder_protocol = "../CSCN-server/protocols/999/"
  
  ALL_js = 
    list.files(folder_protocol, recursive = TRUE, pattern = "\\.js") %>% as_tibble() %>% 
    filter(!grepl("NEW_TASKS", value)) %>% # Do not look into NEW_TASKS to avoid circularity
    filter(grepl("*tasks/.*\\.js", value)) %>% 
    mutate(task_name = basename(value)) %>% 
    mutate(mtime = file.info(paste0(folder_protocol, value))$mtime,
           size = file.info(paste0(folder_protocol, value))$size)
  
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
