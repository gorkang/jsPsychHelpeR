#' setup_shortcuts
#' 
#' To configure a Control+P shortcut to load all packages and functions you can do:
#'
#' @return
#' @export
#'
#' @examples
setup_shortcuts <- function(overwrite = FALSE, show_warnings = FALSE) {

  if (show_warnings == TRUE) cli::cli_alert_warning("Installing packages needed for `shrtcts`...\n")
  
  rlang::check_installed(c("remotes", "fs"), reason = "for {shortcuts} to work")
  if (rlang::is_installed("shrtcts") == FALSE) suppressMessages(remotes::install_github("gadenbuie/shrtcts"))
  
  if (show_warnings == TRUE) cli::cli_alert_warning("Copying configuration files...\n")
  
  file.copy(from = "setup/.shrtcts.R", to = paste0(fs::path_home(), "/.shrtcts.R"), overwrite = overwrite)
  file.copy(from = "setup/addins.json", to = paste0(fs::path_home_r(), "/.R/rstudio/keybindings/addins.json"), overwrite = overwrite)
  
  if (show_warnings == TRUE) cli::cli_alert_warning("Aplying configuration.\n")
  
  # shrtcts::edit_shortcuts()
  suppressMessages(shrtcts::add_rstudio_shortcuts())
  
  if (show_warnings == TRUE) cli::cli_alert_success("`shrtcts` finished installing!\n")
  if (show_warnings == TRUE) cli::cli_alert_warning("Remember to restart the R session: Control + Shift + F10\n")

}