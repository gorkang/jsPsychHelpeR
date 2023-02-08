#' get_dimensions_googledoc
#' Get information from the main jsPsychR GoogleSheet to help creating a new task
#'
#' @param short_name_text .
#' @param google_username .
#' @param google_sheet .
#'
#' @return
#' @export
#'
#' @examples
get_dimensions_googledoc <- function(short_name_text, google_username = "gorkang@gmail.com", google_sheet = "NEW") {
  
  # DEBUG
  # short_name_text = "CEL"
  # short_name_text = "LoB"
  # google_username = "gorkang@gmail.com"
  
  # READ google sheet ---
  
  if (google_sheet == "NEW") {
    google_sheet_ID = "1LAsyTZ2ZRP_xLiUBkqmawwnKWgy8OCwq4mmWrrc_rpQ" # NEW tasks
  } else {
    google_sheet_ID = "1Eo0F4GcmqWZ1cghTpQlA4aHsc8kTABss-HAeimE2IqA" # All tasks
  }
  
  cli::cli_h1("Reading https://docs.google.com/spreadsheets/d/{google_sheet_ID}/edit#gid=0")
  
  googlesheets4::gs4_auth(google_username)
  googlesheets4::local_gs4_quiet() # No googlesheets4::read_sheet messages
  
  # We use this to get the number of items
  DF_resumen_ALL = googlesheets4::read_sheet(google_sheet_ID, sheet = 2, skip = 0) %>% 
    dplyr::rename(short_name = `Codigo Test`) %>% 
    dplyr::filter(!grepl("short_name.*", short_name)) %>% 
    tidyr::drop_na(short_name) 
  
  DF_resumen = DF_resumen_ALL %>% 
    dplyr::filter(short_name == short_name_text) %>% 
    janitor::clean_names()
  
  
  # CHECK ---
  ## Read other tabs only if check passes
  if (nrow(DF_resumen) == 0) {
    cli::cli_h1("{short_name_text} not found")
    cli::cli_par()
    cli::cli_text('Available tasks: {DF_resumen_ALL$short_name}\n\n')
    cli::cli_end()
    cli::cli_abort("{short_name_text} is not in the {google_sheet} Google Doc. ")
  }
  
  
  DF_dimensions = googlesheets4::read_sheet(google_sheet_ID, sheet = 5, skip = 0) %>% 
    dplyr::rename(short_name = `Codigo Test`) %>% 
    tidyr::drop_na(short_name) %>% 
    dplyr::filter(short_name == short_name_text) %>% 
    janitor::clean_names()
  
  
  DF_items = googlesheets4::read_sheet(google_sheet_ID, sheet = 4, skip = 0) %>% 
    dplyr::rename(short_name = `Codigo Test`) %>% 
    tidyr::drop_na(short_name) %>% 
    dplyr::filter(short_name == short_name_text) %>% 
    janitor::clean_names()
  
  # CHECK2 ---
  ## Read other tabs only if check passes
  if (nrow(DF_dimensions) == 0 & nrow(DF_items) == 0) {
    cli::cli_alert_danger("{short_name_text} not found in the dimensions and items tabs")
  }
  
  
  
  # ITEMS ----------------------------------------------------------------
  
  if (nrow(DF_items) > 0) {
    
    ## Items invertidos ---
    
    cli::cli_par()
    cli::cli_text("")
    cli::cli_h1("Items to reverse")
    cli::cli_end()
    
    # For each of the rows in the google doc
    1:nrow(DF_items) %>% 
      purrr::walk(~
             {
               #.x = 1
               numbers_RAW = DF_items[.x,"items_invertidos"] %>% dplyr::pull(items_invertidos)
               
               # Extract numbers from cell with individual numbers and intervals as (1-7)
               NUMBERS_formatted = create_number_series(numbers_RAW)
               
               # Create R vector
               paste0('items_to_reverse = c("', paste(NUMBERS_formatted, collapse = '", "'), '")\n') %>% cat()
             })
    
    
    
    ## Conversion numerica -------------------------------------------------
    
    cli::cli_par()
    cli::cli_text("")
    cli::cli_h1("Numeric conversion")
    cli::cli_end()
    
    
    short_name = DF_items[1,"short_name"] %>% dplyr::pull(short_name)
    number_items = unlist(DF_resumen$items)
    
    # For each of the rows in the google doc
    1:nrow(DF_items) %>%
      purrr::walk(~
             {
               
               cli::cli_text()
               ## Items to apply the numeric conversion
               # .x = 1
               numbers_RAW = DF_items[.x,"items"] %>% dplyr::pull(items)
               
               # Extract numbers from cell with individual numbers and intervals as (1-7)
               NUMBERS_formatted = create_number_series(numbers_RAW)
               
               
               ## Specific numeric conversion
               numeric_conversion = DF_items[.x,"conversion_numerica"] %>% dplyr::pull(conversion_numerica)
               numbers_chunks_all = stringi::stri_extract_all(str = gsub("=| = ", "=", numeric_conversion) %>% gsub("$", "\n", .), regex = ".*\n") %>% unlist() %>% gsub("\n", "", .)
               numbers_chunks_destination = stringi::stri_extract_all(str = numbers_chunks_all, regex = ".*=") %>% unlist() %>% gsub("=", "", .)
               numbers_chunks_origin = stringi::stri_extract_all(str = numbers_chunks_all, regex = "=.*") %>% unlist() %>% gsub("=", "", .)
               
               if (all(is.na(numbers_chunks_destination)) | all(is.na(numbers_chunks_origin))) {
                 cli::cli_alert_danger("NON standard values in column 'conversion_numerica':\n {numbers_chunks_all}")
               } else {
                 # Create final R vector
                 if (number_items == length(NUMBERS_formatted)) {
                   # If all items of test equal, no need to use trialid
                   paste0('RAW == "', numbers_chunks_origin,'" ~ ', numbers_chunks_destination, ',\n') %>% cat()
                 } else {
                   
                   # Add "" if numbers_chunks_destination are not numbers
                   if (all(grepl("\\d{1,3}", numbers_chunks_destination))) {
                     paste0('trialid %in% c("', paste(paste0(short_name , '_', NUMBERS_formatted), collapse = '", "'),'") & RAW == "', numbers_chunks_origin,'" ~ ', numbers_chunks_destination, ',\n') %>% cat()
                   } else {
                     paste0('trialid %in% c("', paste(paste0(short_name , '_', NUMBERS_formatted), collapse = '", "'),'") & RAW == "', numbers_chunks_origin,'" ~ "', numbers_chunks_destination, '",\n') %>% cat()
                   }
                   # paste0('trialid %in% c("', paste(paste0(short_name , '_', NUMBERS_formatted), collapse = '", "'),'") & RAW == "', numbers_chunks_origin,'" ~ ', numbers_chunks_destination, ',\n') %>% cat()
                 }
                 
               }
               
             })
  }
  
  # Dimensiones -----------------------------------------------------------
  
  if (nrow(DF_dimensions) > 0) {
    
    cli::cli_par()
    cli::cli_h1("Dimensiones")
    cli::cli_end()
    
    NAMES_dimensions_CamelCase = janitor::make_clean_names(DF_dimensions %>% dplyr::pull(nombre_dimension), case = "big_camel")
    
    
    # NEW TEMPLATE
    
    # For each of the rows in the google doc
    dimensions_items = 
      1:nrow(DF_dimensions) %>%
      purrr::map_chr(~
                {
                  #.x = 1
                  numbers_RAW = DF_dimensions[.x,"numero_item_dimension_o_sub_escala"] %>% dplyr::pull(numero_item_dimension_o_sub_escala)
                  
                  # Extract numbers from cell with individual numbers and intervals as (1-7)
                  NUMBERS_formatted = create_number_series(numbers_RAW)
                  
                  # Create R vector
                  # paste0(DF_dimensions[.x,"nombre_dimension"],' = c("', paste(NUMBERS_formatted, collapse = '", "'),
                  #        ifelse(.x == nrow(DF_dimensions), '")\n', '"),\n')) 
                  
                  paste0(NAMES_dimensions_CamelCase[.x],' = c("', paste(NUMBERS_formatted, collapse = '", "'),
                         ifelse(.x == nrow(DF_dimensions), '")\n', '"),\n')) 
                  
                })
    
    paste0('items_dimensions = list(\n',
           paste(dimensions_items, collapse = ''),
           ')') %>% cat()
    
    
    Dimensions_NOT_CammelCase = DF_dimensions$nombre_dimension[DF_dimensions$nombre_dimension != NAMES_dimensions_CamelCase]
    if (length(Dimensions_NOT_CammelCase) > 0) {
      cli::cli_text("")  
      cli::cli_par()
      cli::cli_h3("ISSUES with names of dimensions")
      cli::cli_end()
      
      cli::cli_alert_danger("`{Dimensions_NOT_CammelCase}` NOT in CammelCase. FIX THIS in GoogleDoc!")
    }
    
      
    cli::cli_par()
    cli::cli_text("")
    cli::cli_h2("Calculo Dimensiones")
    cli::cli_end()
    
    
    DF_dims_final = 
      1:nrow(DF_dimensions) %>%
      purrr::map_df(~
             {
               calculo_dimension_RAW = DF_dimensions[.x, "calculo_dimension"] %>% dplyr::pull(calculo_dimension)
               notas_RAW = DF_dimensions[.x, "notas"] %>% dplyr::pull(notas)
               error_text = NA
               # cli::cli_alert_info(calculo_dimension_RAW)
               if (tolower(calculo_dimension_RAW) %in% c("promedio", "media", "mean")) {
                 string_function = "rowMeans"
                 
               } else if (tolower(calculo_dimension_RAW) %in% c("suma", "sumatorio", "sumatoria")){
                 string_function = "rowSums"
               } else {
                 error_text = glue::glue("calculo_dimension is '{calculo_dimension_RAW}', but we only know how to work with either 'promedio' or 'suma'")
                 # cli::cli_alert_danger("calculo_dimension is '{calculo_dimension_RAW}', but we only know how to work with either 'promedio' or 'suma'")
                 string_function = calculo_dimension_RAW
               }
               
               # OLD
               # !!name_DIRd1 := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd1, "_DIR")), na.rm = TRUE),
               # paste0('!!name_DIRd', .x, ' := ', string_function, '(select(., paste0(short_name_scale_str, "_", items_DIRd', .x, ', "_DIR")), na.rm = TRUE),\n') %>% cat()
               
               # NEW
               # !!names_list$name_DIRd[1] := rowMeans(select(., paste0(short_name_scale_str, "_", items_dimensions[[1]], "_DIR")), na.rm = TRUE), 
               tibble::tibble(calculo = paste0('!!names_list$name_DIRd[', .x, '] := ', cli::col_yellow(string_function), '(select(., paste0(short_name_scale_str, "_", items_dimensions[[', .x, ']], "_DIR")), na.rm = TRUE),\n'),
                      error_text = error_text,
                      notas = notas_RAW)
             })
    
    cat(DF_dims_final$calculo)
    
    # IF there are important notes, show
    if (nrow(DF_dims_final %>% dplyr::select(notas) %>%tidyr::drop_na()) > 0) {
      cli::cli_par()
      cli::cli_text("")
      cli::cli_h3("Dimensions calculations - IMPORTANT NOTES")
      cli::cli_end()
      cli::cli_alert_warning("Important details about the dimensions calculations")

      1:length(DF_dims_final$notas) %>% 
        purrr::walk(~ {
          if (!is.na(DF_dims_final$notas[.x])) cli::cli_alert_warning("{.x}: {DF_dims_final$notas[.x]}")
        })      
      # cli::cli_ol(DF_dims_final$notas)
    }
    
    # IF there are errors, show
    if (nrow(DF_dims_final %>% dplyr::select(error_text) %>%tidyr::drop_na()) > 0) {
      cli::cli_par()
      cli::cli_text("")
      cli::cli_h3("ERRORS - Dimensions calculations")
      cli::cli_end()
      
      1:length(DF_dims_final$error_text) %>% 
        purrr::walk(~ {
          if (!is.na(DF_dims_final$error_text[.x])) cli::cli_alert_danger("{.x}: {DF_dims_final$error_text[.x]}")
          })
    }
    
      
    
  }
  
}
