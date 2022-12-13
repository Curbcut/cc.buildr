#' Create or edit a build and append data script
#'
#' @param name <`character`> Name of the data script to be created, e.g. `canale`.
#' @param pages_folder <`character`> Path to the folder where all the build and
#' append data scripts are in.
#'
#' @return Opens a build and append data script.
#' @export
create_data_script <- function(name, pages_folder = "dev/data_import/") {

  new_file <- paste0("dev/data_import/", name, ".R")

  # If file exists, just open it
  if (file.exists(new_file)) file.show(new_file)

  # Create file
  new_file <- paste0("dev/data_import/", name, ".R")
  file.create(new_file)
  new_file_connection <- file(new_file)

  # Pre-fill the file
  template <- readLines(system.file("data_script_template.R",
                                    package = "cc.buildr"))
  template[1] <- gsub("_name_", toupper(name), template[1])
  template[2:length(template)] <- gsub("_name_", name, template[2:length(template)])
  pound_nb <- 79 - nchar(template[1])
  template[1] <- paste(template[1], paste0(rep("#", pound_nb), collapse = ""))

  writeLines(template, con = new_file_connection)

  # Message
  message(paste0("Add the function `build_and_append_", name, "()` ",
                 "to the `dev/build.R` workflow"))

  close.connection(new_file_connection)
  file.show(new_file)

}
