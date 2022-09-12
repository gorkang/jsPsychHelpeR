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
        
        # Pregunta 1 / Item 2/9
        # Historias con Faux Pas
        # Si!
        trialid %in% c('fauxPasEv_011', 'fauxPasEv_029', 'fauxPasEv_056',
                       'fauxPasEv_092', 'fauxPasEv_101', 'fauxPasEv_110', 
                       'fauxPasEv_119', 'fauxPasEv_128', 'fauxPasEv_137', 
                       'fauxPasEv_155') & RAW == 'Si' ~ 1,
        
        # No!
        trialid %in% c('fauxPasEv_011', 'fauxPasEv_029', 'fauxPasEv_056', 
                       'fauxPasEv_092', 'fauxPasEv_101', 'fauxPasEv_110', 
                       'fauxPasEv_119', 'fauxPasEv_128', 'fauxPasEv_137',
                       'fauxPasEv_155') & RAW == 'No' ~ 0,
        
        # Resto de preguntas con respuestas especificas
        trialid == 'fauxPasEv_011' & RAW == 'No' ~ 2, # historia 1
        trialid == 'fauxPasEv_021' & RAW == 'Si' ~ 1, # historia 2
        trialid == 'fauxPasEv_022' & RAW == 'Sara' ~ 1, # historia 2
        trialid == 'fauxPasEv_025' & RAW == 'No' ~ 1, # historia 2
        trialid == 'fauxPasEv_031' & RAW == 'No' ~ 2, # historia 3
        trialid == 'fauxPasEv_041' & RAW == 'Si' ~ 1, # historia 4
        trialid == 'fauxPasEv_042' & RAW == 'Alicia' ~ 1, # historia 4
        trialid == 'fauxPasEv_045' & RAW == 'No' ~ 1, # historia 4
        trialid == 'fauxPasEv_051' & RAW == 'No' ~ 2, # historia 5
        trialid == 'fauxPasEv_061' & RAW == 'No' ~ 2, # historia 6
        trialid == 'fauxPasEv_071' & RAW == 'Si' ~ 1, # historia 7
        trialid == 'fauxPasEv_072' & RAW == 'La vecina María' ~ 1, # historia 7
        trialid == 'fauxPasEv_075' & RAW == 'No' ~ 1, # historia 7
        trialid == 'fauxPasEv_081' & RAW == 'No' ~ 2, # historia 8
        trialid == 'fauxPasEv_091' & RAW == 'No' ~ 2, # historia 9
        trialid == 'fauxPasEv_101' & RAW == 'No' ~ 2, # historia 10
        trialid == 'fauxPasEv_111' & RAW == 'Si' ~ 1, # historia 11
        trialid == 'fauxPasEv_112' & RAW == 'Roberto, el ingeniero' ~ 1, # historia 11
        trialid == 'fauxPasEv_115' & RAW == 'No' ~ 1, # historia 11
        trialid == 'fauxPasEv_121' & RAW == 'Si' ~ 1, # historia 12
        trialid == 'fauxPasEv_122' & RAW == 'Jose' ~ 1, # historia 12
        trialid == 'fauxPasEv_122' & RAW == 'Pedro' ~ 1, # historia 12
        trialid == 'fauxPasEv_125' & RAW == 'No' ~ 1, # historia 12
        trialid == 'fauxPasEv_131' & RAW == 'Si' ~ 1, # historia 13
        trialid == 'fauxPasEv_132' & RAW == 'Sergio, el primo de karina' ~ 1, # historia 13
        trialid == 'fauxPasEv_135' & RAW == 'No' ~ 1, # historia 13
        trialid == 'fauxPasEv_141' & RAW == 'Si' ~ 1, # historia 14
        trialid == 'fauxPasEv_142' & RAW == 'Ana ' ~ 1, # historia 14
        trialid == 'fauxPasEv_145' & RAW == 'No' ~ 1, # historia 14
        trialid == 'fauxPasEv_151' & RAW == 'Si' ~ 1, # historia 15
        trialid == 'fauxPasEv_152' & RAW == 'Ana' ~ 1, # historia 15
        trialid == 'fauxPasEv_155' & RAW == 'No' ~ 1, # historia 15
        trialid == 'fauxPasEv_161' & RAW == 'Si' ~ 1, # historia 16
        trialid == 'fauxPasEv_162' & RAW == 'Tito' ~ 1, # historia 16
        trialid == 'fauxPasEv_165' & RAW == 'No' ~ 1, # historia 16
        trialid == 'fauxPasEv_171' & RAW == 'No' ~ 2, # historia 17
        trialid == 'fauxPasEv_181' & RAW == 'Si' ~ 1, # historia 18
        trialid == 'fauxPasEv_182' & RAW == 'Clara' ~ 1, # historia 18
        trialid == 'fauxPasEv_185' & RAW == 'No' ~ 1, # historia 18
        trialid == 'fauxPasEv_191' & RAW == 'No' ~ 2, # historia 19
        trialid == 'fauxPasEv_201' & RAW == 'No' ~ 2, # historia 20
        
        
        # Historias sin Faux Pas
        trialid == 'fauxPasEv_002' & RAW == 'No' ~ 2,
        trialid == 'fauxPasEv_020' & RAW == 'No' ~ 2,
        trialid == 'fauxPasEv_038' & RAW == 'No' ~ 2,
        trialid == 'fauxPasEv_047' & RAW == 'No' ~ 2,
        trialid == 'fauxPasEv_065' & RAW == 'No' ~ 2,
        trialid == 'fauxPasEv_074' & RAW == 'No' ~ 2,
        trialid == 'fauxPasEv_083' & RAW == 'No' ~ 2,
        trialid == 'fauxPasEv_146' & RAW == 'No' ~ 2,
        
        # Preguntas que NO se puntuan
        trialid %in% c('fauxPasEv_012', 'fauxPasEv_015', 'fauxPasEv_018',
                       'fauxPasEv_027', 'fauxPasEv_028', 'fauxPasEv_032',
                       'fauxPasEv_035', 'fauxPasEv_052', 'fauxPasEv_055',
                       'fauxPasEv_062', 'fauxPasEv_065', 'fauxPasEv_082',
                       'fauxPasEv_085', 'fauxPasEv_092', 'fauxPasEv_095',
                       'fauxPasEv_102', 'fauxPasEv_105', 'fauxPasEv_157',
                       'fauxPasEv_158', 'fauxPasEv_172', 'fauxPasEv_175',
                       'fauxPasEv_192', 'fauxPasEv_195', 'fauxPasEv_202',
                       'fauxPasEv_205') ~ 0, 
        
        # Para correccion manual
        trialid %in% c('fauxPasEv_013', 'fauxPasEv_014', 'fauxPasEv_016',
                       'fauxPasEv_017', 'fauxPasEv_023', 'fauxPasEv_024', 
                       'fauxPasEv_026', 'fauxPasEv_033', 'fauxPasEv_034', 
                       'fauxPasEv_036', 'fauxPasEv_037', 'fauxPasEv_038',
                       'fauxPasEv_043', 'fauxPasEv_044', 'fauxPasEv_046',
                       'fauxPasEv_047', 'fauxPasEv_048', 'fauxPasEv_053',
                       'fauxPasEv_054', 'fauxPasEv_056', 'fauxPasEv_057',
                       'fauxPasEv_058', 'fauxPasEv_063', 'fauxPasEv_064', 
                       'fauxPasEv_066', 'fauxPasEv_067', 'fauxPasEv_068',
                       'fauxPasEv_073', 'fauxPasEv_074', 'fauxPasEv_076',
                       'fauxPasEv_077', 'fauxPasEv_078', 'fauxPasEv_083', 
                       'fauxPasEv_084', 'fauxPasEv_086', 'fauxPasEv_087', 
                       'fauxPasEv_088', 'fauxPasEv_093', 'fauxPasEv_094',
                       'fauxPasEv_096', 'fauxPasEv_097', 'fauxPasEv_098',
                       'fauxPasEv_103', 'fauxPasEv_104', 'fauxPasEv_106',
                       'fauxPasEv_107', 'fauxPasEv_108', 'fauxPasEv_113',
                       'fauxPasEv_114', 'fauxPasEv_116', 'fauxPasEv_117',
                       'fauxPasEv_118', 'fauxPasEv_123', 'fauxPasEv_124', 
                       'fauxPasEv_126', 'fauxPasEv_127', 'fauxPasEv_128',
                       'fauxPasEv_133', 'fauxPasEv_134', 'fauxPasEv_136', 
                       'fauxPasEv_137', 'fauxPasEv_138', 'fauxPasEv_143', 
                       'fauxPasEv_144', 'fauxPasEv_146', 'fauxPasEv_147', 
                       'fauxPasEv_148', 'fauxPasEv_153', 'fauxPasEv_154', 
                       'fauxPasEv_156', 'fauxPasEv_163', 'fauxPasEv_164', 
                       'fauxPasEv_166', 'fauxPasEv_167', 'fauxPasEv_168',
                       'fauxPasEv_173', 'fauxPasEv_174', 'fauxPasEv_176',
                       'fauxPasEv_177', 'fauxPasEv_178', 'fauxPasEv_183', 
                       'fauxPasEv_184', 'fauxPasEv_186', 'fauxPasEv_187',
                       'fauxPasEv_188', 'fauxPasEv_193', 'fauxPasEv_194', 
                       'fauxPasEv_196', 'fauxPasEv_197', 'fauxPasEv_198', 
                       'fauxPasEv_203', 'fauxPasEv_204', 'fauxPasEv_206', 
                       'fauxPasEv_207', 'fauxPasEv_208') ~ 123456789,
        
        is.na(RAW) ~ NA_real_,
        grepl(items_to_ignore, trialid) ~ NA_real_,
        TRUE ~ 9999
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
  
  
  # Reliability -------------------------------------------------------------
  
  # REL1 = auto_reliability(DF_wide_RAW, short_name_scale = short_name_scale_str, items = items_DIRd1)
  # items_RELd1 = REL1$item_selection_string
  
  
  # [ADAPT]: Scales and dimensions calculations --------------------------------
  # ****************************************************************************
  # [USE STANDARD NAMES FOR Scales and dimensions: name_DIRt, name_DIRd1, etc.] Check with: standardized_names(help_names = TRUE)
  
  DF_wide_RAW_DIR =
    DF_wide_RAW %>% 
    mutate(
      
      # Make sure to use the correct formula: rowMeans() / rowSums()
      
      # Score Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!names_list$name_DIRd[1] := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd1, "_DIR")), na.rm = TRUE), 
      # !!names_list$name_DIRd[2] := rowMeans(select(., paste0(short_name_scale_str, "_", items_DIRd2, "_DIR")), na.rm = TRUE),
      
      # Reliability Dimensions (see standardized_names(help_names = TRUE) for instructions)
      # !!names_list$name_RELd[1] := rowMeans(select(., paste0(short_name_scale_str, "_", items_RELd1, "_DIR")), na.rm = TRUE), 
      
      # Score Scale
      # !!names_list$name_DIRt := rowSums(select(., matches("_DIR$")), na.rm = TRUE)
      
    )
  
  # [END ADAPT]: ***************************************************************
  # ****************************************************************************
  
  
  
  
  
  
  # CHECK ------------------------------------------------------------------
  
  
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
    # mutate(NA_RAW = is.na(RAW),
    #        NA_DIR = is.na(DIR),
    #        DIFF = NA_RAW != NA_DIR) %>%
    # filter(DIFF == TRUE) %>%
    # View()
    writexl::write_xlsx("outputs/data/TEMP_LONG_fauxpas.xlsx")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # CHECK NAs -------------------------------------------------------------------
  check_NAs(DF_wide_RAW_DIR)
  
  # Save files --------------------------------------------------------------
  save_files(DF_wide_RAW_DIR, short_name_scale = short_name_scale_str, is_scale = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_wide_RAW_DIR) 
  
}
