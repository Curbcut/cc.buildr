#' Run Shiny App with Selenium to screenshot for Discovery Cards
#'
#' This function launches a Shiny app in the background, uses Selenium to open a browser,
#' navigates through discovery cards, and takes screenshots of each card.
#'
#' @param port <`numeric`> The port number to run the Shiny app. Default is 9999.
#' @param discover_cards <`data.frame`> Data frame containing discovery cards information.
#' @param only_ids <`numeric vector`> Vector of IDs to process, defaults to the 'id' column
#' of discover_cards.
#'
#' @return Invisible NULL. The function's primary purpose is side effects:
#'         running a Shiny app, navigating, and saving screenshots.
#' @export
discover_cards_screenshots <- function(port = 9999, discover_cards, only_ids = discover_cards$id) {

  # Run the app in the background (job) -------------------------------------


  # Define the code to run the Shiny app
  app_code <- sprintf("shiny::runApp('.', launch.browser = FALSE, port = %s)",
                      port)

  # Create a temporary script file
  temp_script <- tempfile(fileext = ".R")
  writeLines(app_code, temp_script)

  # Run the script as a background job in RStudio
  job <- rstudioapi::jobRunScript(
    path = temp_script,
    workingDir = getwd(),
    name = 'Run Shiny App'
  )
  # Register on.exit() to ensure cleanup
  on.exit(rstudioapi::jobRemove(job))


  # Start a selenium server and browser -------------------------------------

  driver <- RSelenium::rsDriver(port = 4843L, browser = "firefox", chromever = "103.0.5060.134")
  remDr <- driver$client
  Sys.sleep(2)
  # Register additional on.exit() to stop the Selenium server
  on.exit(driver$server$stop(), add = TRUE)

  # Construct the shiny url
  url <- sprintf("localhost:%s/", port)


  # Function to create a formatted information string
  create_info_message <- function(id, preview) {
    wrapped_preview <- strwrap(preview, width = 60, simplify = FALSE)[[1]]
    wrapped_preview_text <- paste(wrapped_preview, collapse = "\n")

    message <- paste0(
      crayon::blue("ID: "), crayon::green(id), ".\n",
      crayon::blue("Preview: "), crayon::yellow(wrapped_preview_text), "\n\n",
      "Change the corresponding widget values.\n",
      "Center the map to your desired screenshot area.\n"
    )
    return(message)
  }


  # Loop over every discover card -------------------------------------------

  for (id in only_ids) {

    remDr$navigate(sprintf("%s?tb=%s", url, id))

    # Display the information in a paginated way
    info <- create_info_message(
      id = id,
      preview = discover_cards$preview_en[discover_cards$id == id])
    cat(paste(info, collapse = "\n"), "\n")

    # Now wait for user input to confirm readiness
    while(TRUE) {
      cancel <- readline(prompt = "Enter 'done' to proceed with the screenshot: ")
      if(tolower(cancel) == 'done') break
      cat(crayon::red("Please type 'done' when ready.\n"))
    }

    # Take a screenshot and process the image

    img_file <- sprintf("www/landing/discover/%s.png", id)
    remDr$screenshot(file = img_file)
    image <- magick::image_read(img_file)
    image_cropped <- image |>
      magick::image_crop("332x332+0+0", gravity = "center")
    magick::image_write(image_cropped, path = img_file)

  }


  # Clean up by stopping the Selenium server and removing the RStudio job
  driver$server$stop()
  rstudioapi::jobRemove(job)
}
