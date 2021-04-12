test_manually_corrected_files <- function(delete_test_files = TRUE) {
  
  # Load all libraries  
  source("_packages.R")
  # lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source)
  
  
  # Input files Manually corrected
  input_files_test = list.files(path = "tests/manual_correction/data", full.names = FALSE)
  
  
  # Copy test files to data/ folder -----------------------------------------
  file.copy(from = paste0("tests/manual_correction/data/", input_files_test), 
            to = paste0("data/", input_files_test), 
            overwrite = TRUE)
  
  
  # READ MANUAL FILES -------------------------------------------------------
  
  DF_manual_correction = 
    read_data(input_files = paste0("tests/manual_correction/data/", input_files_test), save_output = FALSE) %>% 
    select(id, experimento, trialid, responses, correction, inverse) %>% 
    mutate(DIR = 
             case_when(
               is.na(inverse) ~ correction,
               is.na(correction) ~ inverse,
               TRUE ~ inverse
             ))
  
  # DIMENSIONS AND INDIVIDUAL ITEMS -------------------------------------------------
  
  DF_manual_correction_DIM = 
    DF_manual_correction %>% 
    filter(grepl("DIR", trialid)) %>% 
    select(id, trialid, DIR)
  
  DF_manual_correction_DIR = 
    DF_manual_correction %>% 
    filter(!grepl("DIR", trialid)) %>% 
    select(id, trialid, responses, DIR) %>% 
    rename(RAW = responses) %>% 
    drop_na(DIR)
  
  
  
  # READ AND PREPARE automatic correction files ----------------------------------
  
  # Rerun FULL project (to include manually corrected files)
  targets::tar_invalidate(matches("input_files"))
  targets::tar_make()
  
  # Load 
  targets::tar_load(DF_analysis)
  targets::tar_load(DF_joined)
  
  DF_automatic_correction_DIM = 
    DF_analysis %>% 
    filter(id %in% c("3", "6", "7")) %>% 
    select(id, matches("_RAWd$|_DIRd$|_RAWt$|_DIRt$")) %>%
    pivot_longer(2:ncol(.), values_transform = list(DIR = as.character), names_to = "trialid", values_to = "DIR") 
  
  
  DF_automatic_correction_DIR =
    DF_joined %>%
    filter(id %in% c("3", "6", "7")) %>% 
    select(id, matches("_RAW$|_DIR$")) %>%
    pivot_longer(2:ncol(.), values_transform = list(RAW = as.character), names_to = "trialid", values_to = "RAW") %>% 
    mutate(RAW_DIR = 
             case_when(
               grepl("_RAW$", trialid) ~ "RAW",
               grepl("_DIR$", trialid) ~ "DIR"
             )) %>% 
    mutate(trialid = gsub("_DIR$|_RAW$", "", trialid)) %>% 
    pivot_wider(names_from = RAW_DIR, values_from = RAW)
  
  
  
  
  # JOINED DIMENSIONS -------------------------------------------------------
  
  DF_joined_DIM = 
    DF_automatic_correction_DIM %>% 
    full_join(DF_manual_correction_DIM, by = c("id", "trialid")) %>% 
    mutate(DIR.x = str_sub(DIR.x, start = 1, end = 5),
           DIR.y = str_sub(DIR.y, start = 1, end = 5)) %>% 
    mutate(DIFF = DIR.x != DIR.y) %>% 
    filter(DIFF == TRUE | is.na(DIFF)) %>% 
    arrange(trialid) %>% 
    rename(DIR_automatic = DIR.x,
           DIR_manual = DIR.y)
  
  if (nrow(DF_joined_DIM) > 0) {
    cat(crayon::red("Differences in", nrow(DF_joined_DIM), "rows! See folder: 'tests/manual_correction/outputs/'\n"))
    write_csv(DF_joined_DIM, "tests/manual_correction/outputs/DF_joined_DIM.csv")
  } else {
    cat(crayon::green("NO Differences found!\n"))
  }
  
  DF_joined_DIR = 
    DF_automatic_correction_DIR %>% 
    full_join(DF_manual_correction_DIR, by = c("id", "trialid")) %>% 
    mutate(DIFF = DIR.x != DIR.y) %>% 
    filter(DIFF == TRUE | is.na(DIFF)) %>% 
    arrange(trialid) %>% 
    rename(RAW_automatic = RAW.x,
           RAW_manual = RAW.y,
           DIR_automatic = DIR.x,
           DIR_manual = DIR.y) %>% 
    select(id, trialid, RAW_automatic, RAW_manual, DIR_automatic, DIR_manual, DIFF)
  
  if (nrow(DF_joined_DIR) > 0) {
    cat(crayon::red("Differences in", nrow(DF_joined_DIR), "rows! See folder: 'tests/manual_correction/outputs/'\n"))
    write_csv(DF_joined_DIR, "tests/manual_correction/outputs/DF_joined_DIR.csv")
  } else {
    cat(crayon::green("NO Differences found!\n"))
  }
  
  
  
  # DELETE test files from data/ --------------------------------------------
  if (delete_test_files == TRUE) file.remove(paste0("data/", input_files_test))
  
}

# test_manually_corrected_files(delete_test_files = FALSE)
