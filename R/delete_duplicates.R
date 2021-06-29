#' delete_duplicates
#' 
#' Checks or deletes duplicate files, keeping the older file.
#'
#' @param folder 
#' @param check TRUE/FALSE
#' @param keep_which "older"/"newer"
#'
#' @return
#' @export
#'
#' @examples
delete_duplicates <- function(folder, check = TRUE, keep_which = "older") {
  
  # DEBUG
  # folder = "data/3"
  # check = TRUE
  
  suppressPackageStartupMessages(source("_targets_packages.R"))
  
  # Main files --------------------------------------------------------------
  
  DF_files =
    tibble(full_filename = list.files(path = folder, pattern = "*.csv", full.names = TRUE)) %>% 
    mutate(filename = basename(full_filename)) %>% 
    separate(col = filename, 
             into = c("project", "experimento", "version", "datetime", "id"), 
             sep = c("_"), remove = FALSE) %>% 
    mutate(id = gsub("(*.)\\.csv", "\\1", id),
           id = gsub(" \\([2-9]{1,2}\\)", "", id))
  
  
  folder = dirname(DF_files$full_filename[1])
  
  suppressMessages({
    DUPLICATES = 
      DF_files %>% 
      count(id, experimento, filename) %>% 
      janitor::get_dupes(c(id, experimento))
  })
  
  if (nrow(DUPLICATES) > 0) {
    
    if (keep_which == "older") {
      
      # Select the oldest file for each id/experimento
      KEEP = DF_files %>% group_by(id, experimento) %>% filter(datetime == min(datetime))
      
    } else if (keep_which == "newer") {
      
      # Select the newest file for each id/experimento
      KEEP = DF_files %>% group_by(id, experimento) %>% filter(datetime == max(datetime))
      
    } else {
      cat(crayon::red("keep_which should be either 'older' or 'newer'"))
      stop("Error in the keep_which parameter")
    }
    
    
    # Delete the rest
    DELETE = 
      DUPLICATES %>% 
      anti_join(KEEP, by = c("id", "experimento", "filename")) %>% 
      pull(filename)
    

  # Check or delete ---------------------------------------------------------

    if (check == FALSE) {
      
      file.remove(paste0(folder, "/", DELETE))
      cat(crayon::yellow(length(DELETE), "duplicates deleted: "), "\n -", crayon::silver(paste(DELETE, collapse = "\n - ")))
      if (file.exists("_targets/objects/input_files")) targets::tar_delete("input_files")
      
    } else {
      
      cat(crayon::green(length(DELETE), "duplicates found. Use check = FALSE to DELETE: "), "\n -", crayon::silver(paste(DELETE, collapse = "\n - ")))
      
      DF_dups_clean = 
        DUPLICATES %>% 
        split(interaction(DUPLICATES$id, DUPLICATES$experimento)) %>% 
        .[lapply(., nrow) > 0] 
      
      DF_differences_all = 
        1:length(DF_dups_clean) %>% 
        map(~ 
              {
                if (!DF_dups_clean[[.x]]$experimento[1] %in% c("Consent", "Goodbye")) { 
                  
                  # Reads all files of a set of duplicates (id/experimento) and filters out all the non-duplicates values
                  map_df(paste0(folder, "/", DF_dups_clean[[.x]]$filename) %>% set_names(basename(.)), data.table::fread, .id = "filename", encoding = 'UTF-8') %>% 
                    drop_na(trialid) %>% filter(trialid != "") %>% 
                    select(filename, trialid, response) %>%  replace_na(replace = list(response = "")) %>% 
                    filter(!trialid %in% c("Instructions", "Instrucciones")) %>% # SHOULD NOT, but sometimes the trialid Instructions repeats itself
                    pivot_wider(names_from = filename, values_from = response) %>% 
                    filter(ifelse(apply(.[ , 2:ncol(.)], MARGIN=1, function(x) length(unique(x))) == 1, FALSE, TRUE))
                }
              }
            )
      
      # Use names of sets of duplicates for the elements of the list
      names(DF_differences_all) <- 1:length(DF_dups_clean) %>% map_chr(~ DF_dups_clean[[.x]] %>% transmute(name_list = paste0(id, "_", experimento)) %>% pull(name_list) %>% unlist() %>% head(1))
      
      # Filter out empty entries
      LIST_differences_diff = 
        DF_differences_all %>% 
        .[lapply(., length) > 0] %>% 
        .[lapply(., nrow) > 0] 
      
      # Get names of empty entries
      LIST_differences_equal = names(DF_differences_all)[!names(DF_differences_all) %in% names(LIST_differences_diff)]
      

      # Duplicates that are SAFE to delete because they are == 
      SAFE_DELETE = 
        DUPLICATES %>% mutate(index = paste0(id, "_", experimento)) %>% 
        filter(index %in% LIST_differences_equal) %>% 
        filter(filename %in% DELETE) %>% pull(filename)
      
      
      
      # # LOCAL
      # 1:length(SAFE_DELETE) %>% 
      #   walk(~ {
      #     
      #     message("Delete: ", paste0('rm ', getwd(), '/data/3/', SAFE_DELETE[.x]))
      #     system(paste0('rm ', getwd(), '/data/3/', SAFE_DELETE[.x]))
      #   })
      # 
      #
      # # SERVER
      # CHECK .credentials file exists
      # if (!file.exists(".vault/.credentials")) cat(crayon::red("The .vault/.credentials file does not exist. RUN: \n"), crayon::silver("rstudioapi::navigateToFile('setup/setup_server_credentials.R')\n"))
      # list_credentials = source(".vault/.credentials")
      # 1:length(SAFE_DELETE) %>% 
      #   walk(~ {
      #     
      #     message("Delete: ", paste0('ssh ', list_credentials$value$user, '@', list_credentials$value$IP, ' rm ', list_credentials$value$main_FOLDER, id_protocol, '/.data/', SAFE_DELETE[.x]))
      #     # system(paste0('rm ', getwd(), '/data/3/', SAFE_DELETE[.x]))
      #     system(paste0('sshpass -p ', list_credentials$value$password, ' ssh ', list_credentials$value$user, '@', list_credentials$value$IP, ' rm ', list_credentials$value$main_FOLDER, id_protocol, '/.data/', SAFE_DELETE[.x]))
      #   })
      
      
    }
    
  } else {
    cat(crayon::green("No duplicates in data!"))
  }

  if (!exists("LIST_differences_diff")) LIST_differences_diff = NA
  if (!exists("LIST_differences_equal")) LIST_differences_equal = NA
  if (!exists("DELETE")) DELETE = NA
  
  OUTPUT = 
    list(DUPLICATES = DUPLICATES,
         DELETE = DELETE,
         LIST_differences_diff = LIST_differences_diff,
         LIST_differences_equal = LIST_differences_equal
         )

  return(OUTPUT)

}
