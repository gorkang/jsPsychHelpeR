copy_to_other_projects <- function(file_name, overwrite_str = FALSE, check = TRUE, folder_to_search) {
  
  # DEBUG
  # file_name = "^delete_duplicates.R$"
  # overwrite_str = FALSE
  # folder_to_search = "~/gorkang@gmail.com/RESEARCH"
  
  list_files = list.files(path = folder_to_search, pattern = file_name, recursive = TRUE, full.names = TRUE)

  file_to_copy = list_files[grepl(getwd(), list_files)]
  destinations = list_files[!grepl(getwd(), list_files)]

  if (length(file_to_copy) > 1) stop("More than 1 file matches: '", file_name,"'\n\n", paste(file_to_copy, collapse = "\n"))
  
  if (check == TRUE) {
    
    cat(crayon::green("Would copy", gsub(getwd(), "", file_to_copy), "to: \n"), crayon::silver(paste("-", destinations, collapse = "\n")))
    
  } else {

    file.copy(from = file_to_copy, 
              to = destinations, overwrite = overwrite_str)
    
    cat(crayon::yellow("Copied", gsub(getwd(), "", file_to_copy), "to: \n"), if (overwrite_str == FALSE) {"overwrite_str parameter is FALSE\n"}, crayon::silver(paste("-", destinations, collapse = "\n")))

  }
  
}

# copy_to_other_projects(file_name = "^delete_duplicates.R$", overwrite_str = TRUE, check = TRUE, folder_to_search = "~/gorkang@gmail.com/RESEARCH")
