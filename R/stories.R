#' Create an empty data table for stories
#'
#' @return A tibble with columns \code{id}, \code{title}, \code{preview},
#' \code{lon}, and \code{lat}.
#' @export
stories_empty_table <- function() {
  tibble::tibble(
    id = character(),
    title = character(),
    preview = character(),
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
#' @param preview <`character`> Teaser for the story to appear elsewhere
#' in Curbcut. Usually a single sentence.
#' @param lon <`numeric`> Longitude where the story should be placed on the map.
#' Must be in EPSG Projection 4326 - WGS 84
#' @param lat <`numeric`> Lattitude where the story should be placed on the map.
#' Must be in EPSG Projection 4326 - WGS 84
#'
#' @return The same `stories` data.frame fed, with the added row.
#' @export
stories_add_story <- function(stories, name_id, title, preview, lon, lat) {
  new_story <-
    tibble::tibble(
      name_id = name_id,
      title = title,
      preview = preview,
      lon = as.numeric(lon),
      lat = as.numeric(lat)
    )

  out <- rbind(stories[, names(stories)[names(stories) != "ID"]], new_story)

  out$ID <- seq_along(out$name_id)

  return(out)
}

#' Create an image atlas and mapping for a story
#'
#' @param stories <`dataframe`>A data frame containing the stories.
#' @param input_img_location <`character`> The location of the input images.
#' Defaults to `"dev/data/stories/main_img/"`
#' @param output_bandeau_location <`character`> The location to save the banner
#' images. Defaults to `"www/stories/bandeau_img.png"`
#' @param output_atlas_location <`character`> The location to save the image
#' atlas. Defaults to `"www/stories/image_atlas.png"`
#'
#' @return A list containing the mapping for the stories. This will be an input
#' for the stories rdeck.
#' @export
stories_atlas_mapping <- function(
    stories,
    input_img_location = "dev/data/stories/main_img/",
    output_bandeau_location = "www/stories/bandeau_img/",
    output_atlas_location = "www/stories/image_atlas.png") {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop(
      "Package \"magick\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (!grepl("/$", input_img_location)) {
    input_img_location <- paste0(input_img_location, "/")
  }
  if (!grepl("/$", output_bandeau_location)) {
    output_bandeau_location <- paste0(output_bandeau_location, "/")
  }

  # Stop if missing images
  if (!all(sapply(paste0(stories$name_id, ".png"), `%in%`,
                  list.files(input_img_location)))) {
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

    # Bandeau
    bandeau <- magick::image_resize(img, paste0(
      1000, "x", 1000 / img_info$width * img_info$height, "!"
    ))

    # Get the center of the image
    bandeau_height <- magick::image_info(bandeau)$height
    bandeau <- magick::image_crop(bandeau, paste0(
      1000, "x", 200, "+0+",
      bandeau_height / 2 - 100
    ))

    magick::image_write(bandeau, paste0(
      output_bandeau_location, name_id,
      ".png"
    ))

    # Return image for atlas
    return(round_img_shadow)
  }, simplify = FALSE, USE.NAMES = TRUE)

  # Construct atlas and save
  atlas <-
    magick::image_join(bubbles) |>
    magick::image_append()
  magick::image_write(atlas, output_atlas_location)

  # Construct stories mapping
  stories_mapping <-
    lapply(seq_along(stories$name_id), \(x) {
      list(
        x = (x - 1) * 100,
        y = 0,
        width = 100,
        height = 100
      )
    })
  names(stories_mapping) <- stories$name_id

  # Return mapping, input for rdeck
  return(stories_mapping)
}
