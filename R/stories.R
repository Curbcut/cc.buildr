#' Create an empty data table for stories
#'
#' @return A tibble with columns \code{id}, \code{title}, \code{short_title},
#' \code{preview_en}, \code{preview_fr}, \code{themes}, \code{lon}, and \code{lat}.
#' @export
stories_empty_table <- function() {
  tibble::tibble(
    id = character(),
    title = character(),
    short_title = character(),
    preview_en = character(),
    preview_fr = character(),
    themes = list(),
    lon = numeric(),
    lat = numeric()
  )
}

#' Add a story to the stories table
#'
#' @param stories <`data.frame`> The \code{stories} data.frame to which add a
#' new row.
#' @param name_id <`character`> Name identifier for a story, lowercased and
#' no spaces, e.g. `metro_evolution`.
#' @param title <`character`> Full title of the story, e.g.
#' `The Evolution of the Montreal Metro`
#' @param short_title <`character`> Shorter version of the title for places where
#' there won't be lots of spaces to show longer titles, e.g. `Evolution of the Metro`
#' @param preview_en <`character`> English teaser for the story to appear elsewhere
#' in Curbcut. Usually a single sentence.
#' @param preview_fr <`character`> French teaser for the story to appear elsewhere
#' in Curbcut. Usually a single sentence. Can be an empty character string if the
#' instance of Curbcut has no french version.
#' @param themes <`character vector`> Themes in which the story addresses, e.g.
#' `c("Green space", "Urban transformation", "Community activism", ...)`
#' @param lon <`numeric`> Longitude where the story should be placed on the map.
#' Must be in EPSG Projection 4326 - WGS 84
#' @param lat <`numeric`> Lattitude where the story should be placed on the map.
#' Must be in EPSG Projection 4326 - WGS 84
#'
#' @return The same `stories` data.frame fed, with the added row.
#' @export
stories_add_story <- function(stories, name_id, title, short_title, preview_en,
                              preview_fr, themes, lon, lat) {
  new_story <-
    tibble::tibble(
      name_id = name_id,
      title = title,
      short_title = short_title,
      preview_en = preview_en,
      preview_fr = preview_fr,
      themes = list(sapply(themes, stringr::str_to_sentence,
        USE.NAMES = FALSE
      )),
      lon = as.numeric(lon),
      lat = as.numeric(lat),
    )

  out <- rbind(stories[, names(stories)[names(stories) != "ID"]], new_story)

  out$ID <- seq_along(out$name_id)

  return(out)
}

#' Create an image for every story, used on the map
#'
#' @param stories <`dataframe`>A data frame containing the stories.
#' @param input_img_location <`character`> The location of the input images.
#' Defaults to `"dev/data/stories/main_img/"`
#'
#' @return A list containing the mapping for the stories. This will be an input
#' for the stories rdeck.
#' @export
stories_map_images <- function(stories,
                               input_img_location = "dev/data/stories/main_img/") {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop(
      "Package \"magick\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (!grepl("/$", input_img_location)) {
    input_img_location <- paste0(input_img_location, "/")
  }

  # Stop if missing images
  if (!all(sapply(
    paste0(stories$name_id, ".png"), `%in%`,
    list.files(input_img_location)
  ))) {
    stop(paste0(
      "Missing images in the `input_img_location`. Every story ",
      "(by their `name_id`) must have an PNG in the ",
      "`input_img_location`"
    ))
  }

  # Use the shadow image to build the bubble image on the map. This also
  # creates and saves the banner for each story, using the same image.
  bubbles <- sapply(stories$name_id, \(name_id) {
    list.files(input_img_location)

    path <- paste0(input_img_location, "/", name_id, ".png")
    img <- magick::image_read(path)
    shadow_right <-
      magick::image_read(system.file("dropshadow.png",
        package = "cc.buildr"
      ))

    # Get height, width and crop longer side to match shorter side
    img_info <- magick::image_info(img)
    smaller_side <- min(img_info$height, img_info$width)
    img1 <- magick::image_crop(img, geometry = paste0(
      smaller_side, "x",
      smaller_side, "!"
    ))

    # Resize to 100px
    img2 <- magick::image_resize(img1, "100x100!")

    # Resize shadow_right to fit with image size
    shadow_info <- magick::image_info(shadow_right)

    shadow_right <- magick::image_crop(shadow_right, paste0(
      {
        shadow_info$width - 334
      },
      "x",
      shadow_info$height,
      "+167"
    ))
    shadow_right <- magick::image_resize(shadow_right, "100x100!")

    # Create an image composite using both images
    round_img_shadow <-
      magick::image_composite(img2, shadow_right, operator = "copyopacity")

    # Return image for atlas
    return(round_img_shadow)
  }, simplify = FALSE, USE.NAMES = TRUE)

  # Save every image as base64
  stories$img_base64 <- sapply(bubbles, \(b) {
    temp <- tempfile(fileext = ".png")
    magick::image_write(b, temp)

    # Read the JPG image as raw binary data
    image_data <- readBin(temp, "raw", file.info(temp)$size)

    # Encode the image data to base64
    paste0("data:image/jpeg;base64,", base64enc::base64encode(image_data))
  })

  return(stories)
}


#' Render RMarkdown files as HTML with a custom CSS file.
#'
#' This function renders RMarkdown files as HTML with a custom CSS file. The
#' rendered HTML files are saved to a specified directory.
#'
#' @param file <`character`> Full path to the RMarkdown file to be rendered. File
#' should be have ".Rmd" extension.
#' @param css_path <`character`> A character string specifying the path to the
#' CSS file to be applied to the rendered HTML files. Default is "www/sus.css".
#' @param output_dir <`character`> A character string specifying the directory
#' where the rendered HTML files will be saved. Default is "www/stories/".
#'
#' @return NULL. The function saves the rendered HTML files to the specified
#' output directory.
#' @seealso
#' \code{\link[rmarkdown]{render}} for more details on rendering RMarkdown files.
#' @export
stories_knit_rmd <- function(file, css_path = system.file("styles", "fonts.css", package = "curbcut"),
                             output_dir = "www/stories/") {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop(
      "Package \"rmarkdown\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (!requireNamespace("here", quietly = TRUE)) {
    stop(
      "Package \"here\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (!grepl("/$", output_dir)) {
    output_dir <- paste0(output_dir, "/")
  }

  # Error handling
  if (!sum(grepl(".Rmd$", file)) == length(file)) {
    return(
      warning(sprintf("Skipped %s as it's not an Rmd document.", file))
    )
  }

  # Prep locations
  split_string <- strsplit(file, "/")[[1]]
  file_name <- split_string[length(split_string)]
  out_file <- gsub("Rmd", "html", file_name)
  out <- paste0(output_dir, out_file)

  # Custom output format with the CSS file
  custom_html_output_format <- rmarkdown::html_document(css = css_path)

  # Render RMarkdown document
  rmarkdown::render(
    file,
    output_file = out,
    output_format = custom_html_output_format,
    output_dir = output_dir,
    quiet = TRUE
  )
}


#' Knit All Stories
#'
#' This function knits all R Markdown story files located in a specified directory
#' and saves the HTML output in another specified directory. It uses
#' \code{\link{stories_knit_rmd}} to do so.
#'
#' @param stories_location <`character`> The path to the directory containing
#' the R Markdown story files. Default is "dev/Rmd/stories/".
#' @param css_path <`character`> A character string specifying the path to the
#' CSS file to be applied to the rendered HTML files. Default is "www/sus.css".
#' @param output_dir <`character`> A character string specifying the directory
#' where the rendered HTML files will be saved. Default is "www/stories/".
#'
#' @return A list of the knitted HTML files, with each item corresponding to the
#' output of a single R Markdown story file.
#' @export
stories_knit_all <- function(stories_location = "dev/Rmd/stories/",
                             css_path = system.file("styles", "fonts.css", package = "curbcut"),
                             output_dir = "www/stories/") {
  all_stories <- list.files(stories_location, full.names = TRUE)
  lapply(all_stories, stories_knit_rmd,
    css_path = css_path,
    output_dir = output_dir
  )
}

#' Resize images in a directory (Mostly used for stories)
#'
#' This function reduces the size of images in a directory if they exceed a
#' specified maximum size. The function works recursively and supports the
#' .jpg, .jpeg, and .png file formats. The reduction is done by rescaling the
#' images to 50% of their original dimensions iteratively until they fall
#' below the maximum specified size.
#'
#' @param folder <`character`> A string indicating the path to the directory with
#' images. The default is "www/stories/photos".
#' @param max_size_in_MB <`numeric`> Value defining the maximum allowed size for
#' the images in Megabytes (MB). Images larger than this size will be
#' rescaled. The default is 1 MB.
#'
#' @return NULL. The function works by side effects, resizing the images and
#' overwriting the original files in their original directory.
#' @export
resize_image <- function(folder = "www/stories/photos", max_size_in_MB = 1) {
  all_photos <- list.files(folder, recursive = TRUE, full.names = TRUE)
  all_photos <- all_photos[grepl("\\.jpg|\\.jpeg|\\.png|\\.JPG|\\.JPEG|\\.PNG", all_photos)]

  lapply(all_photos, \(x) {
    file_size <- file.info(x)$size / (1024 * 1024)

    if (file_size < max_size_in_MB) {
      return(NULL)
    }

    # While the file is too large, reduce the size by scaling
    while (file_size > max_size_in_MB) {
      image <- magick::image_read(x)
      rescaled <- magick::image_scale(image, "50%")
      magick::image_write(rescaled, path = x)

      # Update the size
      file_size <- file.info(x)$size / (1024 * 1024) # Size in MB
    }
  })
}
