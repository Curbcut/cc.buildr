#' Build color palettes for data visualization
#'
#' This function generates a set of color palettes for data visualization
#' purposes. It returns a list containing six data frames that can be used
#' for setting color scales in rdeck, ggplot or other visualization packages.
#'
#' @param left_5 <`colours vector`> for a sequential scale of 5 values
#' for the left side of the color scale.
#' @param left_3 <`colours vector`> for a sequential scale of 3 values
#' for the left side of the color scale.
#' @param right_5 <`colours vector`> for a sequential scale of 5 values
#' for the right side of the color scale.
#' @param right_3 <`colours vector`> for a sequential scale of 3 values
#' for the right side of the color scale.
#' @param delta_5 <`colours vector`> scale used for variation of 5 values.
#' By defaults starts with red and go to blue.
#' @param bivar <`colours vector`> for a bivariate scale of 9 values, by default
#' the stevens.greenblue palette.
#' @param qual <`colours vector`> for a qualitative scale of 6 values.
#' @param col_NA <`colours vector`> for missing values.
#' @param viridis <`colours vector`> for a viridis color scale.
#' @param brandbook <`colours vector`> for a brandbook color scale.
#'
#' @return A list of tibbles with color scales to be used for the rdeck map,
#' legend and other plots.
#' @export
build_colours <- function(
    left_5 = c("#C4CDE1", "#98A8CB", "#6C83B5", "#4C5C7F", "#2B3448"),
    left_3 = c("#E8E8E8", "#B5C0DA", "#6C83B5"),
    right_5 = c("#C7DFCC", "#9DC6A6", "#73AE80", "#517A5A", "#2E4633"),
    right_3 = c("#E8E8E8", "#B8D6BE", "#73AE80"),
    delta_5 = c("#6D5271", "#DCD3E2", "#E8E8E8", "#f9e699", "#F69036"),
    delta_neg_5 = c(grDevices::colorRampPalette(delta_5[c(1, 2)])(4), "#E8E8E8"),
    delta_pos_5 = c("#E8E8E8", grDevices::colorRampPalette(delta_5[c(4, 5)])(4)),
    bivar = c(
      "#E8E8E8", "#B5C0DA", "#6C83B5", "#B8D6BE", "#90B2B3", "#567994",
      "#73AE80", "#5A9178", "#2A5A5B"
    ),
    qual = c(
      "#73AE80", "#6C83B5", "#5B362A", "#B58A6C", "#2A5A5B",
      "#AE7673"
    ),
    col_NA = c("#B3B3BB"),
    viridis = scales::viridis_pal()(25)) {
  # rdeck colours -----------------------------------------------------------

  c_NA <- tibble::tibble(
    palette = "NA",
    group = "0",
    value = col_NA
  )

  c_q5 <- tibble::tibble(
    palette = "q5",
    group = as.character(1:5),
    value = left_5
  )

  c_bivar <- tibble::tibble(
    palette = "bivar",
    group = as.character(6:14),
    value = bivar
  )

  c_delta <- tibble::tibble(
    palette = "delta",
    group = as.character(15:19),
    value = delta_5
  )

  c_qual <- tibble::tibble(
    palette = "qual",
    group = as.character(20:25),
    value = qual
  )

  c_viridis <- tibble::tibble(
    palette = "viridis",
    group = as.character(26:50),
    value = viridis
  )

  colour_table <-
    do.call(rbind, list(c_NA, c_q5, c_bivar, c_delta, c_qual, c_viridis))


  # Other -------------------------------------------------------------------

  left_5 <-
    tibble::tibble(
      group = c(0:5, "NA"),
      y = 1,
      fill = c(col_NA, left_5, col_NA)
    )

  bivar_colors <- c(bivar, rep(col_NA, 7))
  bivar <-
    tibble::tibble(
      group = c(
        "1 - 1", "2 - 1", "3 - 1",
        "1 - 2", "2 - 2", "3 - 2",
        "1 - 3", "2 - 3", "3 - 3",
        "NA - 1", "NA - 2", "NA - 3",
        "1 - NA", "2 - NA", "3 - NA",
        "NA - NA"
      ),
      x = c(rep(c(1, 2, 3), 3), rep(NA, length(bivar_colors) - 9)),
      y = c(
        unlist(lapply(c(1, 2, 3), rep, 3)),
        rep(NA, length(bivar_colors) - 9)
      ),
      fill = c(bivar, rep(col_NA, 7))
    )

  delta <- tibble::tibble(
    group = c(1:5, "NA"),
    y = 1,
    fill = c(delta_5, col_NA)
  )

  delta_neg <- tibble::tibble(
    group = c(1:5, "NA"),
    y = 1,
    fill = c(delta_neg_5, col_NA)
  )

  delta_pos  <- tibble::tibble(
    group = c(1:5, "NA"),
    y = 1,
    fill = c(delta_pos_5, col_NA)
  )

  qual <- tibble::tibble(
    group = as.character(seq_along(qual) - 1),
    y = 1,
    fill = qual
  )

  variant_5 <- tibble::tibble(group = as.character(1:5), y = 1, fill = right_5)

  viridis <- tibble::tibble(
    group = as.character(1:10), y = 1,
    fill = scales::viridis_pal()(10)
  )

  brandbook <- tibble::tibble(
    group = c("greydata", "blueexplorer", "greenecology", "redhousing", 
              "pinkhealth", "purpletransport", "yellowclimate",
              "greenurbanlife", "browndemographics"),
    fill = c("#F0F0F0", "#A3B0D1", "#73AD80", "#E08565", "#CD718C", 
             "#C9C3FA", "#F5D574", "#ADB033", "#9E9090"))

  # Return ------------------------------------------------------------------

  return(list(
    table = colour_table,
    left_5 = left_5,
    bivar = bivar,
    delta = delta,
    delta_neg = delta_neg,
    delta_pos = delta_pos,
    variant_5 = variant_5,
    qual = qual,
    viridis = viridis,
    brandbook = brandbook
  ))
}
