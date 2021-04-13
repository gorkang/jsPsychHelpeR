#' Say Something Nice and Load packages.R
#'
#' A demo of cool things.
#'
#' @interactive true
#' @shortcut Ctrl+P

# If it does not work, run: shrtcts::add_rstudio_shortcuts()
# See ~/.R/rstudio/keybindings/addins.json

praise::praise

message("Loading _packages.R and R/helper_functions.R")

if (file.exists("./_packages.R")) suppressPackageStartupMessages(source("./_packages.R"))
if (file.exists("./_targets.R")) source("./_targets.R")
if (file.exists("./R/helper_functions.R")) source("./R/helper_functions.R")
