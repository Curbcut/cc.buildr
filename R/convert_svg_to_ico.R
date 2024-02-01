#' Convert SVG to ICO
#'
#' This function converts an SVG image to an ICO format. It also allows for
#' changing the color of the SVG and resizing it to a specified size. The
#' function can optionally save logo images in different sizes.
#'
#' @param new_color <`character`> Hexadecimal code for the new color.
#' @param svg_path <`character`> URL or path to the SVG file. Default is a
#' specific URL pointing to a Curbcut symbol.
#' @param ico_path <`character`> Path where the ICO file will be saved. Default
#' is "www/favicon.ico".
#' @param ico_size <`numeric`> Size for the ICO file in pixels. Default is 32.
#' @param save_logos <`logical`> Whether to save additional logo images at 192x192
#' and 512x512 pixels. Default is TRUE.
#'
#' @return The function does not return a value but saves the ICO file and
#' optionally the logo images at the specified paths.
#' @export
convert_svg_to_ico <- function(new_color,
                               svg_path = paste0("https://s3.amazonaws.com/curbcut.pu",
                                                 "blic.resources/Curbcut-symbol_c-imp",
                                                 "act-black.svg"),
                               ico_path = "www/favicon.ico", ico_size = 32,
                               save_logos = TRUE) {

  # Read the SVG file
  image <- magick::image_read(svg_path)

  # Change white background to transparent
  image <- magick::image_transparent(image, 'white')

  # Convert hex color to RGB
  rgb_values <- grDevices::col2rgb(new_color)

  # The result is a matrix where the first row is Red, second is Green, and third is Blue
  red_channel <- rgb_values[1,]
  green_channel <- rgb_values[2,]
  blue_channel <- rgb_values[3,]

  # Change the color
  img <- image[[1]]
  img[1,,] <- as.raw(red_channel)  # Red channel
  img[2,,] <- as.raw(green_channel)  # Green channel
  img[3,,] <- as.raw(blue_channel)   # Blue channel
  image <- magick::image_read(img)

  # Resize the image to the specified ICO size (default 32x32 pixels)
  image_svg <- magick::image_scale(image, paste(ico_size, "x", ico_size, sep = ""))

  # Convert the image to ICO format and save
  magick::image_write(image_svg, path = ico_path, format = "ico")

  # Save logos
  if (save_logos) {
    img_192 <- magick::image_scale(image, paste(192, "x", 192, sep = ""))
    img_512 <- magick::image_scale(image, paste(512, "x", 512, sep = ""))
    magick::image_write(img_192, path = "www/logo192.png", format = "png")
    magick::image_write(img_512, path = "www/logo512.png", format = "png")
  }
}
