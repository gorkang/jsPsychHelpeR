
# TODO --------------------------------------------------------------------

# GHDQ12, ITC, PWb, SDG
# IDQ y SDG tienen items solapados

# Packages ----------------------------------------------------------------

# source("R/helper_functions.R")
# source("R/get_dimensions_googledoc.R")
invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source))

# Create NEW SCALE --------------------------------------------------------

get_dimensions_googledoc(short_name_text = "CIT", google_username = "gorkang@gmail.com", google_sheet = "NEW")
create_new_task(short_name_task = "CIT", old_names = FALSE, overwrite = FALSE)



# Create vector -----------------------------------------------------------

create_vector_items(c(4, 5, 8, 9, 13, 15, 20, 22, 25, 26, 27, 29, 30, 33, 34, 36))








# Tasks ready to create prepare_*.R script ----------------------------------

# DF_missing = check_missing_prepare_TASK(check_new_task_tabs = TRUE)
# DF_ready = DF_missing$DF_FINAL %>% filter(!is.na(missing_script) & is.na(missing_gdoc))
# DF_ready$task
# get_dimensions_googledoc(short_name_text = DF_ready$task[1], google_username = "gorkang@gmail.com", google_sheet = "NEW")

