##' Read raw data and prepare a global DF
##'
##' 
##'
##' @title
##' @param input_files
##' @return
##' @author gorkang
##' @export
read_data <- function(input_files, anonymize = FALSE) {
  
  # [TODO]: response_X sera response cuando en input files solo exista responses -----------------------------
  # [TODO]: HARDCODE vars of input files once Herman fixes the issues -----------------------------
  
  # [TODO]: SI ID'S SON RUTS O SIMILAR, ESTO ES UTIL, SI SON NUMEROS CORRELATIVOS NO TANTO 
  # [TODO]: SHOULD ASK FOR A PASSWORD!!!! BUT ONLY ONCE!!!!  ----------------
  

  # CHECK status ------------------------------------------------------------

  # Check vault is empty
  files_vault = dir(path = "vault/RAW_data", full.names = TRUE)
  
  # Check if we already anonymized data
  already_anonymized = length(dir(path = "data", pattern = "raw_data_anonymized.csv")) > 0

  # Check if we have nothing in /data and are not asking to anonymize
  if (length(input_files) == 0 & anonymize == FALSE) cat(crayon::red("\n[ERROR]: There are no files in '/data/ and 'anonymize = FALSE'\n\n"))
  
  
  if (anonymize == TRUE){
    
    if (length(files_vault) == 0) { 
      
      cat(crayon::red("[WARNING]: The raw sensitive files need to be in 'vault/RAW_data'"))
      
    } else if (already_anonymized == TRUE) {
      
      cat(crayon::red("\n[WARNING]: Data is already anonymized:\n"),
          crayon::yellow("   - If you want to re-anonymize the data: delete 'data/raw_data_anonymized.csv'\n"),
          crayon::yellow("   - If you DO NOT want to re-anonymize the data: read_data(input_files, anonymize = ", crayon::bgRed("FALSE"), ")\n")
      )
      
    } else {
      
      # Check targets input_files is empty (no files in "data")
      # if(length(dir(path = "_targets/objects/", pattern = "input_files")) == 0)
        
      # set.seed(20200928)
      # KEY = stringi::stri_rand_strings(1, 40, pattern = "[A-Za-z0-9;!]")
      # KEY = rstudioapi::askForPassword(prompt = "Enter the secret key to anonymize:\n\n  - This should only be done once\n\n  - Make sure you will remember this secret key\n    (hashed IDs can't be recovered without it)")
      KEY = getPass::getPass(msg = "Enter the secret key to anonymize:\n\n  - This should only be done once\n\n  - Make sure you will remember this secret key\n    (hashed IDs can't be recovered without it)")
      
      
      # Read and encrypt --------------------------------------------------------
      
        DF_raw = purrr::map_df(files_vault %>% set_names(basename(.)), readr::read_csv, .id = "filename", 
                               col_types = 
                                 cols(
                                   .default = col_character(),
                                   success = col_logical(),
                                   trial_type = col_character(),
                                   trial_index = col_double(),
                                   time_elapsed = col_double(),
                                   internal_node_id = col_character(),
                                   # view_history = col_character(),
                                   rt = col_double()
                                   # trialid = col_character()
                                   # `question text` = col_character(),
                                   # responses = col_character()
                                 )
        ) %>% 
        
        mutate(
          # [REVIEW]: experimento and ID should be in the DF_raw?
          # [REVIEW]: response_X will not be needed when input is fixed
          experimento = gsub("(.*)_[0-9].csv", "\\1", filename), # Extrae nombre de experimento
          id = gsub(".*_([0-9]).csv", "\\1", filename), # Extrae nombre de participante
          response_x = gsub('\\{"Q0":"|"\\}', '', responses), # Limpia respuestas [REMEMBER: Ahora solo funciona con una respuesta por pantalla]
          responses = gsub('\\{"Q0":"|"\\}', '', responses),
          `question text` = gsub('\\{"Q0":"|"\\}', '', `question text`)
        ) %>%  
      
        # Encrypt
        rowwise() %>% 
        mutate(id = safer::encrypt_string(id, key = KEY)) %>% 
        
        # Save anonymous DF
        write_csv("data/raw_data_anonymized.csv")
      
        # Update status  
        already_anonymized = length(dir(path = "data", pattern = "raw_data_anonymized.csv")) > 0
        input_files = list.files(path = "data", pattern="*.csv", full.names = TRUE)
        
      
    }
    
    
      
  }
  

  # Read all files
  DF_raw = purrr::map_df(input_files %>% set_names(basename(.)), readr::read_csv, .id = "filename", 
                         col_types = 
                           cols(
                             .default = col_character(),
                             success = col_logical(),
                             trial_type = col_character(),
                             trial_index = col_double(),
                             time_elapsed = col_double(),
                             internal_node_id = col_character(),
                             # view_history = col_character(),
                             rt = col_double()
                             # trialid = col_character()
                             # `question text` = col_character(),
                             # responses = col_character()
                           )
  )  
  
  
  # If not anonymized, need to read important variables from filenames
  if (already_anonymized == FALSE) {
    
    DF_raw %>% 
      mutate(
        # [REVIEW]: experimento and ID should be in the DF_raw?
        # [REVIEW]: response_X will not be needed when input is fixed
        experimento = gsub("(.*)_[0-9].csv", "\\1", filename), # Extrae nombre de experimento
        id = gsub(".*_([0-9]).csv", "\\1", filename), # Extrae nombre de participante
        response_x = gsub('\\{"Q0":"|"\\}', '', responses), # Limpia respuestas [REMEMBER: Ahora solo funciona con una respuesta por pantalla]
        responses = gsub('\\{"Q0":"|"\\}', '', responses),
        `question text` = gsub('\\{"Q0":"|"\\}', '', `question text`)
      ) 
    
  }
  
 

  # CHECK -------------------------------------------------------------------
  DF_duplicates = suppressMessages(DF_raw %>% janitor::get_dupes(-c(filename)))
  input_files_duplicates = DF_duplicates %>% filter(success == TRUE) %>% distinct(filename) %>% pull(filename)
  
  if (nrow(DF_duplicates) > 0) stop("[ERROR]: There are duplicates in the '/data' input files: ", paste(input_files_duplicates, collapse = ", "))
  
  
  # Output of function ---------------------------------------------------------
  return(DF_raw)
  
}
