##' Prepare Google FORM data
##'
##' Template for the functions to prepare specific tasks. Most of this file should not be changed
##' Things to change: 
##'   - Name of function: prepare_AIM -> prepare_[value of short_name_scale_str] 
##'   - dimensions parameter in standardized_names()
##'   - 2 [ADAPT] chunks
##'
##' @title prepare_FORM
##'
##' @param short_name_scale_str 
##' @param DF_clean
##'
##' @return
##' @author gorkang
##' @export
prepare_FORM <- function(df_SDG, short_name_scale_str) {
  

# CREDENTIALS -------------------------------------------------------------
  
  # [REMEMBER]: CHANGE email here and use .vault/config/gs4_TEMPLATE.R to authorize the user to check google sheets
  email_googlesheets = ""
  
  # CHECK if we have the credentials
  if (email_googlesheets != "") {
    
    cat(crayon::yellow("Using user", email_googlesheets, "to check Google sheets"))  
    # googlesheets4::gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")
    googlesheets4::gs4_auth(email_googlesheets)
    
  } else {
    
    cat("", 
        crayon::yellow("Need to set email_googlesheets in", crayon::red(".vault/R/prepare_FORM.R"), "to check Google sheets \n", 
        "Remember to also configure .vault/R/gs4_TEMPLATE.R\n")) 
    stop("NEED to set email_googlesheets in .vault/R/prepare_FORM.R")
    
  }
  
  

# Load SDG (id - RUT) -----------------------------------------------------
  
  # We get the id and RUT from df_SDG 
  df_id_RUT = 
    df_SDG %>% 
    select(id, SDG_01_RAW) %>% 
    rename(RUT = SDG_01_RAW)
  
  
  # Read Google sheet -------------------------------------------------------
  
  DF_gsheet = 
    googlesheets4::read_sheet(ss = "1WcPqgJOwJYTTO4uSroraKG49eDf8h3KU8k0Py0sa6uY", 
                              col_types =  "c") %>% 
    janitor::clean_names() %>% 
    rename(
      RUT = x3_rut_sin_puntos_ni_guion,
      FORM_emaild_DIRd = direccion_de_correo_electronico,
      FORM_name_DIRd = x1_nombre_completo,
      FORM_age_DIRd = x2_edad,
      FORM_sex_DIRd = x4_sexo,
      FORM_years_education_DIRd = x5_nivel_educacional_en_anos_de_estudio_considere_hasta_el_ano_realizado_por_ejemplo_si_termino_4to_medio_y_no_realizo_nada_mas_poner_12_anos_si_realizo_dos_anos_de_formacion_tecnica_despues_de_terminar_4to_medio_poner_14_anos_si_llego_solo_hasta_3er_medio_poner_10_ya_que_termino_2do_medio_etc,
      FORM_comuna_DIRd = x6_comuna_de_residencia,
      FORM_telefono_DIRd = x7_numero_telefonico_de_contacto,
      FORM_email2_DIRd = x7_direccion_correo_electronico,
      FORM_enf_psiquiatrica_DIRd = x8_he_sido_diagnosticado_con_alguna_enfermedad_psiquiatrica,
      FORM_detalles_enf_psiquiatrica_DIRd = x8_1_si_su_respuesta_anterior_fue_si_indicar_el_ano_en_que_usted_fue_diagnosticado_y_tipo_de_diagnostico,
      FORM_trat_psiquiatrico_DIRd = x9_se_encuentra_actualmente_bajo_algun_tratamiento_psiquiatrico_ej_usa_medicacion_esta_bajo_terapia_psiquiatrica_y_o_control_medico,
      FORM_detalles_trat_psiquiatrico_DIRd= x9_1_si_su_respuesta_anterior_fue_si_responder_que_tipo_de_tratamiento,
      FORM_drogas_DIRd = x10_ha_tenido_que_acudir_por_ayuda_especializada_o_tiene_la_necesidad_de_buscarla_por_consumo_problematico_de_alcohol_y_o_drogas,
      FORM_detalle_drogas_DIRd = x10_1_si_respondio_si_en_la_pregunta_anterior_que_tipo_de_droga_ha_consumido,
      AIM_01 = aim1_cual_es_el_nivel_educacional_alcanzado_ultimo_ano_aprobado_por_el_principal_sostenedor_del_hogar,
      AIM_02 = aim2_cual_de_las_siguientes_ocupaciones_corresponde_al_trabajo_del_principal_sostenedor_del_hogar_si_el_principal_sostenedor_del_hogar_esta_cesante_o_es_jubilado_indicar_la_ultima_ocupacion_remunerada_que_tuvo_si_el_principal_sostenedor_tiene_mas_de_1_trabajo_debe_registrarse_el_de_mayor_ingreso,
      AIM_03 = aim3_incluyendose_usted_cuantas_personas_viven_en_su_hogar_en_la_actualidad_no_considere_el_servicio_domestico_aunque_sea_puertas_adentro,
      AIM_04 = aim4_1_responda_solo_si_es_un_miembro_en_el_hogar,
      AIM_05 = aim4_2_responda_solo_si_son_dos_miembros_en_el_hogar,
      AIM_06 = aim4_3_responda_solo_si_son_tres_miembros_en_el_hogar,
      AIM_07 = aim4_4_responda_solo_si_son_cuatro_miembros_en_el_hogar,
      AIM_08 = aim4_5_responda_solo_si_son_cinco_miembros_en_el_hogar,
      AIM_09 = aim4_6_responda_solo_si_son_seis_miembros_en_el_hogar,
      AIM_10 = aim4_7_responda_solo_si_son_siete_miembros_en_el_hogar) %>% 
    select(RUT, everything()) %>% 
    drop_na(RUT)
  
  DF_FORM_RAW = 
    DF_gsheet %>% 
    left_join(df_id_RUT, by = "RUT") %>% 
    select(-marca_temporal) %>% 
    select(id, RUT, everything())

  
  
  # Save files --------------------------------------------------------------
  save_files(DF_FORM_RAW, short_name_scale = "FORM_RAW", is_scale = TRUE, is_sensitive = TRUE)
  
  # Output of function ---------------------------------------------------------
  return(DF_FORM_RAW) 
  
}
