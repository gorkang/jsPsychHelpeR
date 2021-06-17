#' setup_shortcuts
#' 
#' To configure a Control+P shortcut to load all packages and functions you can do:
#'
#' @return
#' @export
#'
#' @examples
setup_shortcuts <- function() {

  cat(crayon::yellow("\nInstalling packages needed for {shrtcts}...\n"))
  if (!require('remotes')) install.packages('remotes'); library('remotes')
  suppressMessages(remotes::install_github("gadenbuie/shrtcts"))
  if (!require('fs')) install.packages('fs'); library('fs')
  
  cat(crayon::yellow("Copying configuration files...\n"))
  file.copy(from = "setup/.shrtcts.R", to = paste0(fs::path_home(), "/.shrtcts.R"))
  file.copy(from = "setup/addins.json", to = paste0(fs::path_home_r(), "/.R/rstudio/keybindings/addins.jsonX"))
  
  cat(crayon::yellow("Aplying configuration. \n"))
  # shrtcts::edit_shortcuts()
  suppressMessages(shrtcts::add_rstudio_shortcuts())
  cat(crayon::green("Remember to restart the R session: Control + Shift + F10\n"))
  
}