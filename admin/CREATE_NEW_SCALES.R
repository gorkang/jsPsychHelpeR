
# TODO --------------------------------------------------------------------

# GHDQ12, ITC, PWb, SDG
# IDQ y SDG tienen items solapados

# Packages ----------------------------------------------------------------

# library(tidyverse)
source("R/helper_functions.R")
source("_packages.R")


# Create NEW SCALE --------------------------------------------------------

create_new_task(short_name_task = "CRTMCQ4", old_names = FALSE, overwrite = FALSE)



# Create vector -----------------------------------------------------------

create_vector_items(c(4, 5, 8, 9, 13, 15, 20, 22, 25, 26, 27, 29, 30, 33, 34, 36))
