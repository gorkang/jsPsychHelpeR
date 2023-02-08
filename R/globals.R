# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
utils::globalVariables(
  c(".", "alternative responses", "CRTMCQ4_Reflectiveness_DIRd", "DIFF", "Descripcion", "EMAIL",
  "NAMES", "N_responses", "Nombre", "ProgressReporter", "RAW", "REI40_Experiential_DIRd",
  "alternative responses", "bold_labels", "button_pressed", 
  "canonical", "citas",  "col_red", "condition_within", "condition_withinOK", "Codigo Test",
  "datetime", "days_since_start", "desc", "dimensiones", "duplicate_tasks",
  "element_text", "error_text", "experiment",
  "fecha_registro", "filename", "first", "full_filename",
  "head", "id", "index",
  "matches", "menu", "n", "name", "nitem", "nombre_dimension", "notas", "num",
  "number of items", "numero_registros", "original", "pid_target",
  "prepare_AIM_gsheet", "prepare_FORM", "project", "protocol", "protocols", "puntajes",
  "r.drop", "response", "responses", "resumen",
  "Responses", "rt", "safely", "scale_fill_gradientn", "scale_fill_manual", "scale_x_date",
  "script", "setup_shortcuts",
  "short_name", "short_name: from trialid", "short_name_scale_str", "size",
  "stimulus", "stimulus_raw", "subnum",
  "suma_total", "task", "tasks", "test",
  "TEXT", "TIME", "time_elapsed",  "trial_type", "trialid", "type", "value"))