#' delete_duplicates
#' 
#' Checks or deletes duplicate files, keeping the older file.
#'
#' @param folder A folder with a zip or csv files from a jsPsychMaker protocol
#' @param check If TRUE, only checks, but does not make changes
#' @param keep_which "older"/"newer"
#'
#' @return A list with information
#' @export
#'
#' @examples delete_duplicates(folder = system.file("extdata/",package = "jsPsychHelpeR"))
delete_duplicates <- function(folder, check = TRUE, keep_which = "older") {
  
  # PARAMETERS --------------------------------------------------------------
  
  id_protocol = basename(gsub("/$", "", folder))
  input_files = list.files(path = folder, pattern = "*.csv|*.zip", full.names = TRUE)
  
  
  # Main files --------------------------------------------------------------
  
  DF_files =
    read_csv_or_zip(input_files = input_files) |> 
    dplyr::mutate(filename = basename(filename)) %>% 
    parse_filename() |> 
    dplyr::mutate(id = gsub(" \\([2-9]{1,2}\\)", "", id)) # remove the (2) from the filename
  
  
  folder = dirname(DF_files$filename[1])
  
  suppressMessages({
    DUPLICATES = 
      DF_files %>% 
      dplyr::count(id, experiment, filename) %>% 
      janitor::get_dupes(c(id, experiment))
  })
  
  if (nrow(DUPLICATES) > 0) {
    
    if (keep_which == "older") {
      
      # Select the oldest file for each id/experiment
      KEEP = DF_files %>% dplyr::group_by(id, experiment) %>% dplyr::filter(datetime == min(datetime))
      
    } else if (keep_which == "newer") {
      
      # Select the newest file for each id/experiment
      KEEP = DF_files %>% dplyr::group_by(id, experiment) %>% dplyr::filter(datetime == max(datetime))
      
    } else {
      cat(cli::col_red("keep_which should be either 'older' or 'newer'"))
      stop("Error in the keep_which parameter")
    }
    
    
    # Delete the rest
    DELETE = 
      DUPLICATES %>% 
      dplyr::anti_join(KEEP, by = c("id", "experiment", "filename")) %>% 
      dplyr::pull(filename)
    
    
    # Check or delete ---------------------------------------------------------
    
    if (check == FALSE) {
      
      file.remove(paste0(folder, "/", DELETE))
      cat(cli::col_yellow(length(DELETE), "duplicates deleted: "), "\n -", cli::col_silver(paste(DELETE, collapse = "\n - ")))
      if (file.exists("_targets/objects/input_files")) targets::tar_delete("input_files")
      
    } else {
      
      cat(cli::col_green(length(DELETE), "duplicates found. Use check = FALSE to DELETE: "), "\n -", cli::col_silver(paste(DELETE, collapse = "\n - ")), "\n\n")
      
      DF_dups_clean = 
        DUPLICATES %>% 
        split(interaction(DUPLICATES$id, DUPLICATES$experiment)) %>% 
        .[lapply(., nrow) > 0] 
      
      DF_differences_all = 
        1:length(DF_dups_clean) %>% 
        purrr::map(~ 
              {
                if (!DF_dups_clean[[.x]]$experiment[1] %in% c("Consent", "Goodbye")) { 
                  
                  # Reads all files of a set of duplicates (id/experiment) and filters out all the non-duplicates values
                  DF_temp = purrr::map_df(paste0(folder, "/", DF_dups_clean[[.x]]$filename) %>% set_names(basename(.)), data.table::fread, .id = "filename", encoding = 'UTF-8') %>% 
                   tidyr::drop_na(trialid) %>% dplyr::filter(trialid != "") 
                  
                  if (!"responses" %in% names(DF_temp) & "response" %in% names(DF_temp)) DF_temp = DF_temp %>% dplyr::rename(responses = response)
                  if (!"responses" %in% names(DF_temp)) DF_temp = DF_temp %>% dplyr::mutate(responses = paste0("CHECK_ME_", runif(dplyr::n(), min = 0, max = 10)))
                  DF_temp %>%  
                   dplyr::select(filename, trialid, responses) %>%  replace_na(replace = list(responses = "")) %>% 
                    dplyr::filter(!trialid %in% c("Instructions", "Instrucciones")) %>% # SHOULD NOT, but sometimes the trialid Instructions repeats itself
                    tidyr::pivot_wider(names_from = filename, values_from = responses) %>% 
                    dplyr::filter(ifelse(apply(.[ , 2:ncol(.)], MARGIN=1, function(x) length(unique(x))) == 1, FALSE, TRUE))
                }
              }
        )
      
      # Use names of sets of duplicates for the elements of the list
      names(DF_differences_all) <- 1:length(DF_dups_clean) %>% purrr::map_chr(~ DF_dups_clean[[.x]] %>% dplyr::transmute(name_list = paste0(id, "_", experiment)) %>% dplyr::pull(name_list) %>% unlist() %>% head(1))
      
      # Filter out empty entries
      LIST_differences_diff = 
        DF_differences_all %>% 
        .[lapply(., length) > 0] %>% 
        .[lapply(., nrow) > 0] 
      
      # Get names of empty entries
      LIST_differences_equal = names(DF_differences_all)[!names(DF_differences_all) %in% names(LIST_differences_diff)]
      
      
      # Duplicates that are SAFE to delete because they are == 
      SAFE_DELETE = 
        DUPLICATES %>% dplyr::mutate(index = paste0(id, "_", experiment)) %>% 
        dplyr::filter(index %in% LIST_differences_equal) %>% 
        dplyr::filter(filename %in% DELETE) %>% dplyr::pull(filename)
      
      
      # REMOVING SAFE_DELETE FILES
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
      # if (!file.exists(".vault/.credentials")) cat(cli::col_red("The .vault/.credentials file does not exist. RUN: \n"), cli::col_silver("rstudioapi::navigateToFile('setup/setup_server_credentials.R')\n"))
      # list_credentials = source(".vault/.credentials")
      # 1:length(SAFE_DELETE) %>%
      #   walk(~ {
      # 
      #     message("\n", paste0('ssh ', list_credentials$value$user, '@', list_credentials$value$IP, ' rm ', list_credentials$value$main_FOLDER, id_protocol, '/.data/', SAFE_DELETE[.x]))
      #     ### system(paste0('rm ', getwd(), '/data/3/', SAFE_DELETE[.x]))
      #     # system(paste0('sshpass -p ', list_credentials$value$password, ' ssh ', list_credentials$value$user, '@', list_credentials$value$IP, ' rm ', list_credentials$value$main_FOLDER, id_protocol, '/.data/', SAFE_DELETE[.x]))
      #   })
      
      
      # SAFE DELETE commands
      if (!file.exists(".vault/.credentials")) cat(cli::col_red("The .vault/.credentials file does not exist. RUN: \n"), cli::col_silver("rstudioapi::navigateToFile('setup/setup_server_credentials.R')\n"))
      list_credentials = source(".vault/.credentials")
      SAFE_DELETE = 1:length(SAFE_DELETE) %>%
        purrr::map(~ {
          
          c(paste0('ssh ', list_credentials$value$user, '@', list_credentials$value$IP, ' rm ', list_credentials$value$main_FOLDER, id_protocol, '/.data/', SAFE_DELETE[.x]))
          ### system(paste0('rm ', getwd(), '/data/3/', SAFE_DELETE[.x]))
          # system(paste0('sshpass -p ', list_credentials$value$password, ' ssh ', list_credentials$value$user, '@', list_credentials$value$IP, ' rm ', list_credentials$value$main_FOLDER, id_protocol, '/.data/', SAFE_DELETE[.x]))
        })
      
      # UNSAFE DELETE commands
      if (!file.exists(".vault/.credentials")) cat(cli::col_red("The .vault/.credentials file does not exist. RUN: \n"), cli::col_silver("rstudioapi::navigateToFile('setup/setup_server_credentials.R')\n"))
      list_credentials = source(".vault/.credentials")
      ALL_DELETE = 1:length(DELETE) %>%
        purrr::map(~ {
          
          c(paste0('ssh ', list_credentials$value$user, '@', list_credentials$value$IP, ' rm ', list_credentials$value$main_FOLDER, id_protocol, '/.data/', DELETE[.x]))
          ### system(paste0('rm ', getwd(), '/data/3/', DELETE[.x]))
          # system(paste0('sshpass -p ', list_credentials$value$password, ' ssh ', list_credentials$value$user, '@', list_credentials$value$IP, ' rm ', list_credentials$value$main_FOLDER, id_protocol, '/.data/', DELETE[.x]))
        })
      
      
    }
    
  } else {
    cat(cli::col_green("No duplicates in data!\n"))
  }
  
  if (!exists("LIST_differences_diff")) LIST_differences_diff = NA
  if (!exists("LIST_differences_equal")) LIST_differences_equal = NA
  if (!exists("DELETE")) DELETE = NA
  if (!exists("KEEP")) KEEP = NA
  if (!exists("SAFE_DELETE")) SAFE_DELETE = NA
  if (!exists("ALL_DELETE")) ALL_DELETE = NA
  
  
  
  OUTPUT = 
    list(DUPLICATES = DUPLICATES %>% dplyr::mutate(ACTION = ifelse(filename %in% DELETE, "DELETE", "KEEP")),
         KEEP = KEEP,
         DELETE = DELETE,
         LIST_differences_diff = LIST_differences_diff,
         LIST_differences_equal = LIST_differences_equal,
         DELETE_COMMANDS = list(SAFE_DELETE = SAFE_DELETE,
                                ALL_DELETE = ALL_DELETE)
    )
  
  return(OUTPUT)
  
}
