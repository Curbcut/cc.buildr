#' Create an Rmd file for a story
#'
#' @param name The name of the story
#' @param stories_folder The folder where the story should be created (
#' defaults to "dev/Rmd/stories/")
#'
#' @return Opens an empty new story .Rmd (pre-populated with a template)
#' @export
create_story_rmd <- function(name, stories_folder = "dev/Rmd/stories/") {

  new_file <- paste0(stories_folder, name, ".Rmd")

  # If file exists, just open it
  if (file.exists(new_file)) file.show(new_file)

  # Create file
  new_file <- paste0(stories_folder, name, ".Rmd")
  file.create(new_file)
  new_file_connection <- file(new_file)

  # Pre-fill the file
  template <- readLines(system.file("story_template.Rmd",
                                    package = "cc.buildr"
  ))

  # Write the template in the file
  writeLines(template, con = new_file_connection)

  # Close connection and open the file
  close.connection(new_file_connection)
  file.show(new_file)

}
