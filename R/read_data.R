##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param input_files
##' @return
##' @author gorkang
##' @export
read_data <- function(input_files) {
  
  DF_raw = purrr::map_df(input_files %>% set_names(basename(.)), readr::read_csv, .id = "filename")
  
  DF =
    DF_raw %>% 
    mutate(experimento = gsub("(.*)_[0-9].csv", "\\1", filename), # Extrae nombre de experimento
           ID = gsub(".*_([0-9]).csv", "\\1", filename), # Extrae nombre de participante
           response_X = gsub('\\{"Q0":"|"\\}', '', responses) # Limpia respuestas [REMEMBER: Ahora solo funciona con una respuesta por pantalla]
           ) %>%
    filter(!trialid %in% c("Screen_WM")) %>% # Elimina instrucciones [TODO: usar regexp para limpiar instrucciones_NOMBRETEST]
    janitor::clean_names()  
    # count(experimento)
    
  
  return(DF)
  
}
