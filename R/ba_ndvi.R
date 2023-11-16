#' Build and append NDVI (Normalized Difference Vegetation Index) data
#'
#' This function downloads and import Harmonized Landsat Sentinel-2 raster data
#' for the master_polygon at 30x30m. The NDVI is calculated from it and gets
#' averaged for every features in `all_scales`. It includes data retrieval, transformations,
#' filtering, merging, and calculation of region values and breaks.
#'
#' @param scales_variables_modules <`named list`> A list of length three.
#' The first is all the scales, the second is the variables table, and the
#' third is the modules table.
#' @param master_polygon <`sfc_MULTIPOLYGON`> Unioned multipolygon of all the boundary
#' for which NDVI information should be retrieved.
#' @param all_scales <`named list`> Named list of dataframed of all available scales,
#' previously to consolidate them. The 30x30 raster data will be intersected once for
#' every scale, and then left joined to the scales in `scales_variables_modules`.
#' @param data_output_path <`character`> String representing the directory path
#' to store the NDVI data (default is "dev/data/ndvi/").
#' @param years <`numeric vector`> Years for which to build the data. Defaults to
#' `cc.data::ndvi_years()`.
#' @param skip_scales <`character vector`> Scales to be skipped in the analysis.
#' Already skipped scales are building and street. Defaults to NULL.
#' @param scales_sequences <`list`> A list of scales sequences representing the
#' hierarchical ordering of scales on an auto-zoom.
#' @param crs <`numeric`> EPSG coordinate reference system to be assigned, e.g.
#' \code{32617} for Toronto.
#'
#' @return A list of length 3, similar to the one fed to
#' `scales_variables_modules` with the NDVI variable added, its addition
#' in the variables table and the module table.
#' @export
ba_ndvi <- function(scales_variables_modules,
                    master_polygon, all_scales,
                    data_output_path = "dev/data/ndvi/",
                    years = cc.data::ndvi_years(),
                    skip_scales = NULL, scales_sequences, crs) {
  # Create the folders if they don't exist
  dir.create(data_output_path) |> suppressWarnings()
  tmp_folder <- paste0(data_output_path, "tmp/")
  dir.create(tmp_folder) |> suppressWarnings()

  # Scales to go over
  skip_scales <- c(skip_scales, "building", "street")
  scales <- scales_variables_modules$scales[!names(scales_variables_modules$scales) %in% skip_scales]
  scales <- lapply(scales, `[`, "ID")


  # Get NDVI data saved on disk ---------------------------------------------

  possible_ndvi_years <- years
  all_vars <- sprintf("ndvi_%s", possible_ndvi_years)
  time_regex <- "_\\d{4}$"

  # Should data be reimported? Not if `data.qs` already exists!
  if (!"data.qs" %in% list.files(data_output_path)) {
    cc.data::ndvi_import_from_masterpolygon(master_polygon,
                                            years = possible_ndvi_years,
                                            output_path = data_output_path,
                                            temp_folder = tmp_folder,
                                            overwrite = FALSE,
                                            filter_cloudy_tiles = TRUE
    )
  }



  # Add it to all the scales ------------------------------------------------

  scales <- lapply(scales, sf::st_transform, crs)
  all_files <- list.files(data_output_path, recursive = TRUE, full.names = TRUE)

  # Is the data file already calculated?
  if (!"data.qs" %in% list.files(data_output_path)) {

    data <- lapply(as.character(possible_ndvi_years), \(year) {
      # Load all the data for that year
      years_data <- all_files[grepl(sprintf("ndvi/%s", year), all_files)]

      data_path <- sprintf("%s%s/data.qs", data_output_path, year)
      exists <- all_files[grepl(data_path, all_files)]
      if (length(exists) == 0) {
        # Load all the data for that year
        years_data <- all_files[grepl(sprintf("ndvi/%s", year), all_files)]

        data <- lapply(years_data, qs::qread)
        data <- lapply(data, sf::st_transform, crs)
        data <- Reduce(rbind, data)
        qs::qsave(data, data_path)
      }
      data <- qs::qread(data_path)

      lapply(scales, \(scale) {
        intersections <- sf::st_intersects(scale, data)
        col_name <- sprintf("ndvi_%s", year)
        scale[[col_name]] <-
          sapply(intersections, \(int) mean(data$ndvi[int], na.rm = TRUE))

        return(scale[c("ID", col_name)])
      })
    })
    names(data) <- possible_ndvi_years
    qs::qsave(data, file = sprintf("%sdata.qs", data_output_path))
  }

  data <- qs::qread(sprintf("%sdata.qs", data_output_path))

  # Are there scales missing in data?
  are_there_missing <- all(sapply(data, \(dat) {
    !all(names(scales) %in% names(dat))
  }))
  if (are_there_missing) {
    data <- mapply(\(year, dat) {
      if (all(names(scales) %in% names(dat))) return(dat)
      missing <- setdiff(names(scales), names(dat))

      data_path <- sprintf("%s%s/data.qs", data_output_path, year)
      data <- qs::qread(data_path)

      additional_dat <- lapply(missing, \(scale_name) {
        scale <- scales[[scale_name]]
        intersections <- sf::st_intersects(scale, data)
        col_name <- sprintf("ndvi_%s", year)
        scale[[col_name]] <-
          sapply(intersections, \(int) mean(data$ndvi[int], na.rm = TRUE))

        return(scale[c("ID", col_name)])
      })
      names(additional_dat) <- missing

      return(c(dat, additional_dat))
    }, names(data), data, SIMPLIFY = FALSE)
    qs::qsave(data, file = sprintf("%sdata.qs", data_output_path))
  }

  # Take out geometry
  data <- lapply(data, \(yr_l) {
    lapply(yr_l, \(df) {
      df <- sf::st_drop_geometry(df)
      is.na(df[[2]]) <- NA
      df
    })
  })
  # Get all scales to one df with the multiple years
  avail_scales <- names(data[[1]])
  ndvi_data <- sapply(avail_scales, \(sc) {
    out <- lapply(data, \(yr_l) {
      sf::st_drop_geometry(yr_l[[sc]])
    })
    Reduce(\(x, y) merge(x, y, by = "ID", all = TRUE), out)
  }, simplify = FALSE, USE.NAMES = TRUE)

  # Apply to our scales
  interpolated <- mapply(\(scale_name, scale_df) {
    if (!scale_name %in% names(ndvi_data)) return(scale_df)
    merge(scale_df, ndvi_data[[scale_name]], by = "ID", all.x = TRUE)
  }, names(scales_variables_modules$scales), scales_variables_modules$scales)

  # Data tibble -------------------------------------------------------------

  data <- data_construct(svm_data = scales_variables_modules$data,
                         scales_data = interpolated,
                         unique_var = "ndvi",
                         time_regex = time_regex)


  # Variables table ---------------------------------------------------------

  avail_scale <- names(ndvi_data)

  interpolated_ref <- tibble::tibble(
    scale = avail_scale,
    interpolated_from = rep("rasters (30m*30m)", length(avail_scale))
  )


  variables <-
    add_variable(
      variables = scales_variables_modules$variables,
      var_code = "ndvi",
      type = "ind",
      var_title = "Vegetation",
      var_short = "Vegetation",
      explanation = "the presence and intensity of vegetation",
      exp_q5 = "are living in areas with _X_ level of presence and intensity of vegetation",
      parent_vec = "households",
      theme = "Ecology",
      private = FALSE,
      pe_include = TRUE,
      dates = possible_ndvi_years,
      avail_scale = avail_scale,
      source = "Curbcut",
      interpolated = interpolated_ref
    )


  # Possible sequences ------------------------------------------------------

  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = avail_scale)


  # Modules table -----------------------------------------------------------

  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "ndvi",
      theme = "Ecology",
      nav_title = "Vegetation",
      title_text_title = "Vegetation",
      title_text_main = paste0(
        "<p>The Normalized Difference Vegetation Index (NDVI) measures the ",
        "average amount of vegetation present in an area during the growing ",
        "season. It is used for environmental conservation, urban planning, ",
        "and climate change mitigation."
      ),
      title_text_extra = paste0(
        "<p>NDVI plays a significant role in various applications, including ",
        "analyzing urban greenness, monitoring agricultural growth, and ",
        "assessing wildfire risks. Calculated from Harmonized Landsat ",
        "Sentinel-2 (HLS) data, NDVI represents average vegetation during ",
        "the growing season (May 1st through August 31st)."
      ),
      metadata = TRUE,
      dataset_info = paste0(
        "<p>The NDVI data on this page is derived from the HLSS30.v2.0 and HLSL30.v2.0 satellites, ",
        "spanning from 2013 to the present. The process includes the following detailed steps:</p>",
        "<ul>",
        "<li><strong>Data Retrieval:</strong> Specific NDVI bands are extracted based on the satellite collection. ",
        "These bands are used to analyze the vegetation intensity and are organized into a data frame.</li>",
        "<li><strong>Raster Stacking:</strong> Three bands (Red, Near-infrared (NIR), and Fmask) are extracted, cropped, and masked according ",
        "to the area of interest. These bands are essential for analyzing vegetation patterns and filtering out cloud contamination.</li>",
        "<li><strong>NDVI Calculation:</strong> NDVI is computed using the formula (NIR-Red)/(NIR+Red), quantifying the vegetation's health. ",
        "This index measures the difference between near-infrared (which vegetation strongly reflects) and red light (which vegetation absorbs).</li>",
        "<li><strong>Quality Filtering with Fmask:</strong> The Fmask (Function of Mask) band is utilized to filter out poor quality pixels. ",
        "Fmask is an algorithm that identifies and masks unwanted features like clouds, cloud shadows, water, or snow/ice. In the HLS data, ",
        "values of 0 and 64 in the Fmask layer indicate clean and useful pixels, ensuring that the resulting NDVI is free from these artifacts. ",
        "This filtering is vital for obtaining a true state of the vegetation and underlying surface.</li>",
        "<li><strong>Resampling:</strong> All NDVI rasters are resampled to a specific extent to align perfectly, allowing for accurate aggregation.</li>",
        "</ul>",
        "<p>This process, with its meticulous handling of NDVI bands and quality filtering using Fmask, provides a scientifically robust view of vegetation ",
        "trends, essential for urban sustainability studies and environmental justice analyses.</p>"
      ),
      var_left = c("ndvi"),
      dates = possible_ndvi_years,
      var_right = scales_variables_modules$variables$var_code[
        scales_variables_modules$variables$source == "Canadian census" &
          !is.na(scales_variables_modules$variables$parent_vec)
      ],
      default_var = "ndvi",
      avail_scale_combinations = avail_scale_combinations
    )


  # Return ------------------------------------------------------------------

  return(list(
    scales = interpolated,
    variables = variables,
    modules = modules,
    data = data
  ))
}
