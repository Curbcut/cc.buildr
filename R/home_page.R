#' Create a home page script (buidl and save necessary tibbles)
#'
#' @param pages_folder <`character`> Path to the folder where all the build and
#' append data scripts are in.
#'
#' @return Opens a home page data building script.
#' @export
create_home_page_script <- function(pages_folder = "dev/data_import/") {
  new_file <- paste0(pages_folder, "home_page", ".R")

  # If file exists, just open it
  if (file.exists(new_file)) file.show(new_file)

  # Create file
  new_file <- paste0(pages_folder, "home_page", ".R")
  file.create(new_file)
  new_file_connection <- file(new_file)

  # Pre-fill the file
  template <- readLines(system.file("home_page_script.R",
    package = "cc.buildr"
  ))

  writeLines(template, con = new_file_connection)

  # Message
  message(paste0(
    "Add the function `home_page()` ",
    "to the `dev/build.R` workflow"
  ))

  close.connection(new_file_connection)
  file.show(new_file)
}
