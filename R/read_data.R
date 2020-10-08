##' Read raw data and prepare a global DF
##'
##' 
##'
##' @title
##' @param input_files
##' @return
##' @author gorkang
##' @export
read_data <- function(input_files) {
  
  # [TODO]: response_X sera response cuando en input files solo exista responses -----------------------------
  # [TODO]: HARDCODE vars of input files once Herman fixes the issues -----------------------------
  
  
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
  
  DF =
    DF_raw %>% 
    mutate(
      # [REVIEW]: experimento and ID should be in the DF_raw?
      # [REVIEW]: response_X will not be needed when input is fixed
      experimento = gsub("(.*)_[0-9].csv", "\\1", filename), # Extrae nombre de experimento
           ID = gsub(".*_([0-9]).csv", "\\1", filename), # Extrae nombre de participante
           response_X = gsub('\\{"Q0":"|"\\}', '', responses) # Limpia respuestas [REMEMBER: Ahora solo funciona con una respuesta por pantalla]
           ) %>%
    # [REVIEW]: Screen_WM se usa ahora para las instruccinoes. En el futuro deberia ser [instrucciones_NOMBRETEST]
    filter(!trialid %in% c("Screen_WM")) %>% # Elimina instrucciones [TODO: usar regexp para limpiar instrucciones_NOMBRETEST]
    janitor::clean_names()  

  return(DF)
  
}
