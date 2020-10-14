##' Create DF_clean
##'
##' .. content for \details{} ..
##'
##' @title
##' @param DF_raw
##' @return
##' @author gorkang
##' @export
create_clean_data <- function(DF_raw) {
  
  DF_clean =
    DF_raw %>% 
    janitor::clean_names() %>% 
    mutate(
      # [REVIEW]: experimento and ID should be in the DF_raw?
      # [REVIEW]: response_X will not be needed when input is fixed
      experimento = gsub("(.*)_[0-9].csv", "\\1", filename), # Extrae nombre de experimento
      id = gsub(".*_([0-9]).csv", "\\1", filename), # Extrae nombre de participante
      response_x = gsub('\\{"Q0":"|"\\}', '', responses), # Limpia respuestas [REMEMBER: Ahora solo funciona con una respuesta por pantalla]
      responses = gsub('\\{"Q0":"|"\\}', '', responses),
      question_text = gsub('\\{"Q0":"|"\\}', '', question_text)
    ) %>%
    # [REVIEW]: Screen_WM se usa ahora para las instruccinoes. En el futuro deberia ser [instrucciones_NOMBRETEST]
    filter(trial_type != "fullscreen") %>% # Empty line
    filter(!trialid %in% c("Screen_WM")) # Elimina instrucciones [TODO: usar regexp para limpiar instrucciones_NOMBRETEST]
  
  
  # Output of function ---------------------------------------------------------
  return(DF_clean)

}
