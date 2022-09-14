##' Prepare fauxPasEv
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_FauxPasEv -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_FauxPasEv
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_fauxPasEv <- function(DF_clean, short_name_scale_str) {
  
  # DEBUG
  # targets::tar_load_globals()
  # debug_function(prepare_fauxPasEv)
  
  
  
  # [ADAPT]: Items to ignore and reverse ---------------------------------------
  # ****************************************************************************
  
  items_to_ignore = c("000") # Ignore these items: If nothing to ignore, keep items_to_ignore = c("00")
  items_to_reverse = c("000") # Reverse these items: If nothing to reverse, keep  items_to_reverse = c("00")
  
  names_dimensions = c("") # If no dimensions, keep names_dimensions = c("")
  
  items_DIRd1 = c("")
  items_DIRd2 = c("")
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # Standardized names ------------------------------------------------------
  names_list = standardized_names(short_name_scale = short_name_scale_str, 
                                  dimensions = names_dimensions, # Use names of dimensions, "" or comment out line
                                  help_names = FALSE) # help_names = FALSE once the script is ready
  
  # Create long -------------------------------------------------------------
  DF_long_RAW = create_raw_long(DF_clean, short_name_scale = short_name_scale_str, numeric_responses = FALSE, is_experiment = FALSE)
  
  # Show number of items, responses, etc. [uncomment to help prepare the test] 
  # prepare_helper(DF_long_RAW, show_trialid_questiontext = TRUE)
  
  

  # Preparacion items  ------------------------------------------------------

  stories_noFP = c(1, 3, 5, 6, 8, 9, 10, 17, 19, 20)
  stories_FP = c(2, 4, 7, 11, 12, 13, 14, 15, 16, 18)

  # Q1 are the instructions for all stories
  numbers_Q1 = (1:20 * 9) - (9 - 1)
  items_Q1 = paste0("fauxPasEv_", sprintf("%03d", numbers_Q1))
    
  # Q2 is the first question in each story (item1 is the story)
  numbers_noFP_Q2 = (stories_noFP * 9) - (9 - 2)
  numbers_FP_Q2 = (stories_FP * 9) - (9 - 2)
  items_noFP_Q2 = paste0("fauxPasEv_", sprintf("%03d", numbers_noFP_Q2))
  items_FP_Q2 = paste0("fauxPasEv_", sprintf("%03d", numbers_FP_Q2))
  
  numbers_noFP_Q3_Q7 = (9 - 3:7) |> map(~ (stories_noFP * 9) - .x) |> unlist()
  items_noFP_Q3_Q7 = paste0("fauxPasEv_", sprintf("%03d", numbers_noFP_Q3_Q7))
  
  
  # Items 8 and 9 (old Questions 7 and 8)
  numbers_FP_Q8_Q9 = (9 - 8:9) |> map(~ (stories_FP * 9) - .x) |> unlist()
  items_FP_Q8_Q9 = paste0("fauxPasEv_", sprintf("%03d", numbers_FP_Q8_Q9))
  
  numbers_noFP_Q8_Q9 = (9 - 8:9) |> map(~ (stories_noFP * 9) - .x) |> unlist()
  items_noFP_Q8_Q9 = paste0("fauxPasEv_", sprintf("%03d", numbers_noFP_Q8_Q9))
  

  # Items Q2 to Q7, for final score
  numbers_Q2_Q7 = (9 - 2:7) |> map(~ (1:20 * 9) - .x) |> unlist()
  items_Q2_Q7 = paste0("fauxPasEv_", sprintf("%03d", numbers_Q2_Q7))
  
  # Items Q8 and Q9, for final score
  numbers_noFP_Q8_Q9 = (9 - 8:9) |> map(~ (stories_noFP * 9) - .x) |> unlist()
  items_noFP_Q8_Q9 = paste0("fauxPasEv_", sprintf("%03d", numbers_noFP_Q8_Q9))
  
    

  
  # Create long DIR ------------------------------------------------------------
  
  DF_long_DIR = 
    DF_long_RAW %>% 
    select(id, trialid, RAW) %>%
    
    
    # [ADAPT]: RAW to DIR for individual items -----------------------------------
  # ****************************************************************************
  
  # Transformations
  mutate(
    DIR =
      case_when(
        
        # Stories
        trialid %in% items_Q1 ~ NA_real_,
        
        # Stories without FauxPas ---
        trialid %in% items_noFP_Q2 & RAW == 'No' ~ 2,
        trialid %in% items_noFP_Q2 & RAW != 'No' ~ 0,
        
        # Do not score items 3 to 7 (old Questions 2 to 6) 
        trialid %in% items_noFP_Q3_Q7 ~ 0,
        
        
        # Stories with FauxPas ---
        trialid %in% items_FP_Q2 & RAW == 'Si' ~ 1,
        trialid %in% items_FP_Q2 & RAW != 'Si' ~ 0,
        
        # Items 3 to 7
        trialid %in% c("fauxPasEv_012") & RAW == "Sara" ~ 1,
        trialid %in% c("fauxPasEv_012") & RAW != "Sara" ~ 0,
        trialid %in% c("fauxPasEv_015") & RAW == "Si" ~ 1,
        trialid %in% c("fauxPasEv_015") & RAW == "No" ~ 0,
        trialid %in% c("fauxPasEv_030") & RAW == "Alicia" ~ 1,
        trialid %in% c("fauxPasEv_030") & RAW == "Julia" ~ 0,
        trialid %in% c("fauxPasEv_033","fauxPasEv_060","fauxPasEv_096","fauxPasEv_105","fauxPasEv_114","fauxPasEv_123","fauxPasEv_132","fauxPasEv_141","fauxPasEv_159") & RAW == "No" ~ 1,
        trialid %in% c("fauxPasEv_033","fauxPasEv_060","fauxPasEv_096","fauxPasEv_105","fauxPasEv_114","fauxPasEv_123","fauxPasEv_132","fauxPasEv_141","fauxPasEv_159") & RAW == "Si" ~ 0,
        trialid %in% c("fauxPasEv_057") & RAW == "La vecina María" ~ 1,
        trialid %in% c("fauxPasEv_057") & RAW != "La vecina María" ~ 0,
        trialid %in% c("fauxPasEv_093") & RAW == "Roberto, el ingeniero" ~ 1,
        trialid %in% c("fauxPasEv_093") & RAW != "Roberto, el ingeniero" ~ 0,
        trialid %in% c("fauxPasEv_102") & RAW == "José" ~ 1,
        trialid %in% c("fauxPasEv_102") & RAW != "José" ~ 0,
        trialid %in% c("fauxPasEv_111") & RAW == "Sergio, el primo de Karina" ~ 1,
        trialid %in% c("fauxPasEv_111") & RAW == "Karina" ~ 0,
        trialid %in% c("fauxPasEv_120") & RAW == "Ana" ~ 1,
        trialid %in% c("fauxPasEv_120") & RAW == "Josefina" ~ 0,
        trialid %in% c("fauxPasEv_129") & RAW == "Julian" ~ 1,
        trialid %in% c("fauxPasEv_129") & RAW == "Cristina" ~ 0,
        trialid %in% c("fauxPasEv_138") & RAW == "Tito" ~ 1,
        trialid %in% c("fauxPasEv_138") & RAW != "Tito" ~ 0,
        trialid %in% c("fauxPasEv_156") & RAW == "Clara" ~ 1,
        trialid %in% c("fauxPasEv_156") & RAW != "Clara" ~ 0,
        
        
        # SIN FAUX PAS. Items 8 & 9 ---------------------
        trialid %in% c("fauxPasEv_008") & RAW == "Casa de Oscar" ~ 1,
        trialid %in% c("fauxPasEv_008") & RAW != "Casa de Oscar" ~ 0,
        trialid %in% c("fauxPasEv_009") & RAW == "No" ~ 1,
        trialid %in% c("fauxPasEv_009") & RAW == "Si" ~ 0,
        trialid %in% c("fauxPasEv_026") & RAW == "una camisa" ~ 1,
        trialid %in% c("fauxPasEv_026") & RAW != "una camisa" ~ 0,
        trialid %in% c("fauxPasEv_027") & RAW == "más grande" ~ 1,
        trialid %in% c("fauxPasEv_027") & RAW != "más grande" ~ 0,
        trialid %in% c("fauxPasEv_053") & RAW == "bencina" ~ 1,
        trialid %in% c("fauxPasEv_053") & RAW != "bencina" ~ 0,
        trialid %in% c("fauxPasEv_054") & RAW == "no aceptó tarjeta" ~ 1,
        trialid %in% c("fauxPasEv_054") & RAW != "no aceptó tarjeta" ~ 0,
        trialid %in% c("fauxPasEv_071") & RAW == "parque" ~ 1,
        trialid %in% c("fauxPasEv_071") & RAW != "parque" ~ 0,
        trialid %in% c("fauxPasEv_072") & RAW == "Sultan persigue pichones" ~ 1,
        trialid %in% c("fauxPasEv_072") & RAW != "Sultan persigue pichones" ~ 0,
        trialid %in% c("fauxPasEv_080") & RAW == "rol principal" ~ 1,
        trialid %in% c("fauxPasEv_080") & RAW != "rol principal" ~ 0,
        trialid %in% c("fauxPasEv_081") & RAW == "debe estar decepcionada" ~ 1,
        trialid %in% c("fauxPasEv_081") & RAW != "debe estar decepcionada" ~ 0,
        trialid %in% c("fauxPasEv_089") & RAW == "trepar el gran cañon" ~ 1,
        trialid %in% c("fauxPasEv_089") & RAW != "trepar el gran cañon" ~ 0,
        trialid %in% c("fauxPasEv_090") & RAW == "no tenia credencial" ~ 1,
        trialid %in% c("fauxPasEv_090") & RAW != "no tenia credencial" ~ 0,
        trialid %in% c("fauxPasEv_152") & RAW == "porque no venía" ~ 1,
        trialid %in% c("fauxPasEv_152") & RAW != "porque no venía" ~ 0,
        trialid %in% c("fauxPasEv_153") & RAW == "no" ~ 1,
        trialid %in% c("fauxPasEv_153") & RAW != "no" ~ 0,
        trialid %in% c("fauxPasEv_170") & RAW == "por un rayón" ~ 1,
        trialid %in% c("fauxPasEv_170") & RAW != "por un rayón" ~ 0,
        trialid %in% c("fauxPasEv_171") & RAW == "no se enojó" ~ 1,
        trialid %in% c("fauxPasEv_171") & RAW != "no se enojó" ~ 0,
        trialid %in% c("fauxPasEv_179") & RAW == "carnicería" ~ 1,
        trialid %in% c("fauxPasEv_179") & RAW != "carnicería" ~ 0,
        trialid %in% c("fauxPasEv_180") & RAW == "porque no escuchó" ~ 1,
        trialid %in% c("fauxPasEv_180") & RAW != "porque no escuchó" ~ 0,
        
        
        # CON FAUX PAS. Items 8 & 9 ---------------------
        
        trialid %in% c("fauxPasEv_017") & RAW == "Elena" ~ 1,
        trialid %in% c("fauxPasEv_017") & RAW != "Elena" ~ 0,
        trialid %in% c("fauxPasEv_018") & RAW == "Sara" ~ 1,
        trialid %in% c("fauxPasEv_018") & RAW != "Sara" ~ 0,
        trialid %in% c("fauxPasEv_134") & RAW == "Julian" ~ 1,
        trialid %in% c("fauxPasEv_134") & RAW != "Julian" ~ 0,
        trialid %in% c("fauxPasEv_135") & RAW == "No" ~ 1,
        trialid %in% c("fauxPasEv_135") & RAW != "No" ~ 0,
        
        
        # For manual correction
        # trialid %in% c(
        #   "fauxPasEv_014", "fauxPasEv_016", "fauxPasEv_032", "fauxPasEv_034", "fauxPasEv_061", 
        #   "fauxPasEv_095", "fauxPasEv_097", "fauxPasEv_104", "fauxPasEv_106", "fauxPasEv_113", 
        #   "fauxPasEv_115", "fauxPasEv_122", "fauxPasEv_124", "fauxPasEv_131", "fauxPasEv_133", 
        #   "fauxPasEv_140", "fauxPasEv_142", "fauxPasEv_158", "fauxPasEv_160"
        #   ) ~ 123456789,
        
        is.na(RAW) ~ NA_real_,
        grepl(items_to_ignore, trialid) ~ NA_real_,
        
        # Everything else For manual correction
        TRUE ~ 123456789 #9999
      )
  ) %>% 
    
    # Invert items
    mutate(
      DIR = 
        case_when(
          DIR == 9999 ~ DIR, # To keep the missing values unchanged
          trialid %in% paste0(short_name_scale_str, "_", items_to_reverse) ~ (6 - DIR),
          TRUE ~ DIR
        )
    )
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  # MANUAL CORRECTION -------------------------------------------------------
  
  manual_correction_output = "outputs/data/manual_correction/fauxPasEv_manual_correction.xlsx"
  manual_correction_input = "data/manual_correction/fauxPasEv_manual_correction.xlsx"
  
  
  # Save DF with OPEN responses for manual correction ---
  
  if (!file.exists(dirname(here::here(manual_correction_output)))) dir.create(dirname(here::here(manual_correction_output)))
  
  # CHECK output file
  if (file.exists(here::here(manual_correction_output))) {
    cli::cli_alert_info(cli::col_none(c("OVERWIRITING '{manual_correction_output}'")))
  } else {
    cli::cli_alert_info(cli::col_none(c("Creating '{manual_correction_output}'")))
  }
  
  # Create file
  OUTPUT_DF = DF_long_DIR %>% 
    # Manual correction
    filter(DIR == 123456789) %>% 
    # Instructions are empty
    filter(RAW != "") 
  
  # Write
  OUTPUT_DF %>% 
    writexl::write_xlsx(here::here(manual_correction_output))
  
  
  # CHECK FILES EXIST!!! IF NOT, JUMP ALL and give message
  
  
  # Read manual correction file ---
  if (file.exists(manual_correction_input)) {
    # cli::cli_par()
    cli::cli_alert_success(cli::col_none("CHECK 1 OK | Manual correction of fauxPas already exists in '{manual_correction_input}'"))
    # cli::cli_end()
    
    DF_manual_correction = readxl::read_excel(here::here("data/manual_correction/fauxPasEv_manual_correction.xlsx")) 
    nrow_output = nrow(readxl::read_excel(manual_correction_output))
    
    nrow_input = nrow(DF_manual_correction)
    
    DF_uncorrected = DF_manual_correction %>% filter(DIR == 123456789 | is.na(DIR))
    nrow_uncorrected = DF_uncorrected %>% nrow()
    
    
    missing_rows = OUTPUT_DF %>% anti_join(DF_manual_correction, by = c("id", "trialid")) %>% 
      bind_rows(DF_uncorrected %>% drop_na(trialid))
    
    # Check if raw output file == manual correction input file
    if(nrow_output == nrow_input) {
      
      cli::cli_par()
      cli::cli_alert_success(cli::col_none("CHECK 2 OK | manual correction and RAW data have the same number of rows"))
      # Show n of uncorrected rows
      if (nrow_uncorrected > 0) cli::cli_alert_danger(paste0("CHECK 2 WARNING | manual correction has {nrow_uncorrected} uncorrected rows (DIR == 123456789 or NA) || ", cli::bg_green('YOU NEED TO FIX THIS'), " ||"))
      cli::cli_end()
      
      # CHECKS ---
      
      # uncorrected_responses = nrow(DF_manual_correction %>% filter(DIR == 123456789))
      if (nrow_uncorrected > 0) {
        
        cli::cli_h1(text = "INSTRUCTIONS")
        cli::cli_par()
        cli::cli_alert_info(c("To manually correct responses:\n",
                              "1) Open file: '{manual_correction_input}'\n", 
                              "2) Look for '123456789' in the DIR column and replace with numeric value"))
        cli::cli_end()
        cli::cli_h1(text = "")
        
        # If just a few, show uncorrected
        if (nrow_uncorrected <= 10) {
          cli::cli_alert_info(c("We don't have the manual correction for the following items: \n", 
                                paste0(capture.output(as.data.frame(missing_rows)), collapse = "\n")))
          cli::cli_h1(text = "")
        }
        
        
        cli::cli_abort(c("{nrow_uncorrected} responses not corrected: follow the INSTRUCTIONS alert in the Console\n",
                         "- In the meantime, for the pipeline to run, you can go to _targets.R and comment the following lines:\n",
                         "- line starting with: tar_target(df_fauxPasEv, prepare_fauxPasEv(...\n",
                         "- df_fauxPasEv line in create_joined() function"))
      } else {
        cli::cli_alert_success(cli::col_none("CHECK 3 OK | All rows have been corrected"))
      }
      
      
    } else {
      
      cli::cli_par()
      cli::cli_alert_danger(paste0("ERROR | manual correction and RAW data **DO NOT HAVE** the same number of rows || ", cli::bg_green('You need to FIX THIS to continue'), " ||"))
      cli::cli_end()
      
      cli::cli_h1(text = "INSTRUCTIONS")
      cli::cli_par()
      cli::cli_alert_info(c("INSTRUCTIONS to fix issue with the manual correction file.\n",
                            "1) Compare '{manual_correction_output}' with '{manual_correction_input}'\n", 
                            "2) Copy new lines to '{manual_correction_input}' (probably new participants?)\n", 
                            "3) manually correct responses in '{manual_correction_input}'\n",
                            "- Look for '123456789' in the DIR column and replace with numeric value"))
      cli::cli_end()
      cli::cli_h1(text = "")
      
      
      cli::cli_abort(c("Correction file has different number of rows than the RAW data follow the ℹ INSTRUCTIONS alert in the Console"))
    }
    
    
    
  } else {
    cli::cli_par()
    cli::cli_alert_danger(c("ERROR | manual correction file '{manual_correction_input}' DOES NOT EXIST || {cli::bg_green('You need to FIX THIS')} ||"))
    cli::cli_end()
    
    cli::cli_h1(text = "INSTRUCTIONS")
    cli::cli_par()
    cli::cli_alert_info(c("INSTRUCTIONS to create the manual correction file.\n",
                          "1) Copy '{manual_correction_output}' to '{manual_correction_input}'\n", 
                          "2) manually correct responses in '{manual_correction_input}'\n",
                          "- Look for '123456789' in the DIR column and replace with numeric value"))
    cli::cli_end()
    cli::cli_h1(text = "")
    
    
    cli::cli_abort(c("Correction file NOT found!: follow the INSTRUCTIONS alert in the Console"))
    
  }
  
  
  
  # Add manual correction file back ---
  
  DF_long_DIR_manually_corrected = 
    DF_long_DIR %>% 
    # Read DF_long_DIR without the manually corrected items
    filter(DIR != 123456789) %>% 
    # Add manually corrected items from manually corrected file
    bind_rows(DF_manual_correction)
  
  # ****************************************************************************
  
  
  
  
  
  
  
  
  
  # Cálculo puntajes totales ----------------------------------------------
  
  # En primer lugar se corrigen las preguntas de comprensión 7 y 8. Se da un punto solo si las dos preguntas control se han respondido correctamente. 
  # La puntuación obtenida se divide entre 20.
  
  # Solo se consideraran para el calculo de puntaje total aquellas  historias en las que se haya respondido bien las preguntas 7 y 8.
  # Calculo puntaje total:  Sumatoria de preguntas 1 a 6 (para historias con fauxpas) y  pregunta 1 (para historias sin fauxpas). 
  # la sumatoria de las preguntas control (7 y 8) se hace aparte.
  
  
  
  # Create a diccionary with stories and trialid's
  DICC_story_items = tibble(story = rep(1:20, each = 9),
                            trialid = paste0("fauxPasEv_", sprintf("%03d", rep(1:180))))
  
  # Items key questions 8 and 9 (old 7 and 8)
  items_Q8_Q9 = c(items_FP_Q8_Q9, items_noFP_Q8_Q9)
  
  # Join stories dictionary
  DF_long_DIR_manually_corrected_DICT = DF_long_DIR_manually_corrected |> 
    left_join(DICC_story_items, by = "trialid") |> 
    mutate(KEY = paste0(id, "_", story))
  
  # Stories where participants have items 8 and 9 OK
  # En primer lugar se corrigen las preguntas de comprensión 7 y 8. 
  # Se da un punto solo si las dos preguntas control se han respondido correctamente. 
  DF_stories_OK = 
    DF_long_DIR_manually_corrected_DICT |> 
    filter(trialid %in% items_Q8_Q9) |> 
    group_by(id, story) |> 
    summarise(Q8_Q9 = sum(DIR), 
              KEY = unique(KEY), 
              .groups = "drop") |> 
    filter(Q8_Q9 == 2) |> 
    mutate(Q8_Q9 = Q8_Q9/2) # Only 1 point when Q8_Q9 of a story are OK
  
  
  # Points of only 
  # La puntuación obtenida se divide entre 20.
  DF_points_Q8Q9 = 
    DF_stories_OK |> 
    group_by(id) |> 
    summarise(Q8_Q9 = sum(Q8_Q9)/20, .groups = "drop") 
  
  
  
  # Solo se consideraran para el calculo de puntaje total aquellas  historias en las que se haya respondido bien las preguntas 7 y 8.
  # Calculo puntaje total:  Sumatoria de preguntas 1 a 6 (para historias con fauxpas) y  pregunta 1 (para historias sin fauxpas). 
  # la sumatoria de las preguntas control (7 y 8) se hace aparte.
  
  DF_points_Q2Q7 = 
    DF_long_DIR_manually_corrected_DICT |>
    filter(trialid %in% items_Q2_Q7) |> # Only items 2 to 7
    filter(KEY %in% DF_stories_OK$KEY) |> # Only if Q8_Q9 are both OK
    group_by(id) |> 
    summarise(Q2_Q7 = sum(DIR), 
              .groups = "drop")
  
  DF_points_final_non_0 = 
    DF_points_Q8Q9 |> 
    full_join(DF_points_Q2Q7, by = "id") %>% 
    mutate(!!names_list$name_DIRt := rowSums(select(., starts_with("Q")), na.rm = TRUE)) |> 
    select(id, !!names_list$name_DIRt)
  
  # id's with 0 points
  DF_points_final_0 = 
    DF_long_DIR_manually_corrected_DICT |> 
    distinct(id) |> 
    filter(!id %in% DF_points_final$id) |> 
    mutate(!!names_list$name_DIRt := 0)
  
  # Join all id's and scores
  DF_points_final = 
    DF_points_final_non_0 |> 
    bind_rows(DF_points_final_0)
  
  
  
  # Create DF_wide_RAW_DIR -----------------------------------------------------
  DF_wide_RAW =
    DF_long_DIR_manually_corrected %>% 
    pivot_wider(
      names_from = trialid, 
      values_from = c(RAW, DIR),
      names_glue = "{trialid}_{.value}") %>% 
    
    # NAs for RAW and DIR items
    mutate(!!names_list$name_RAW_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_RAW")) & matches("_RAW$")))),
           !!names_list$name_DIR_NA := rowSums(is.na(select(., -matches(paste0(short_name_scale_str, "_", items_to_ignore, "_DIR")) & matches("_DIR$")))))
  
  
  
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
  # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
  
  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    left_join(DF_points_final, by = "id")
    
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  
  
  
  
  # CHECK ------------------------------------------------------------------
  # TODO: Delete/comment this after checking all is well
  
  RAW = DF_wide_RAW_DIR %>%
    pivot_longer(starts_with("faux") & ends_with("RAW"), values_transform = as.character, values_to = "RAW") %>%
    select(id, name, RAW) %>%
    separate(name, into = c("task", "item", "type")) %>% 
    select(-task, -type)
  
  
  
  DIR = DF_wide_RAW_DIR %>%
    pivot_longer(starts_with("faux") & ends_with("DIR"), values_transform = as.character, values_to = "DIR") %>%
    select(id, name, DIR) %>%
    separate(name, into = c("task", "item", "type")) %>% 
    select(-task, -type)
  
  RAW %>%
    full_join(DIR, by = c("id", "item")) %>%
    arrange(item) %>% 
    writexl::write_xlsx("outputs/data/TEMP_LONG_fauxpas.xlsx")
  
  DF_wide_RAW_DIR |> writexl::write_xlsx("outputs/data/TEMP_DF_wide_RAW_DIR_fauxpas.xlsx")
  
  
  cli::cli_h1(text = "CHECK DATA SAVED IN: outputs/data/TEMP_LONG_fauxpas.xlsx")
  cli::cli_h1(text = "FINAL DF SAVED IN: outputs/data/TEMP_DF_wide_RAW_DIR_fauxpas.xlsx")
  
  
  
  
  
  
  
  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
  
}
