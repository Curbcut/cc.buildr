#' Generate NDVI grids
#'
#' This function generates NDVI (Normalized Difference Vegetation Index) grids
#' based on given census scales and base polygons. It manages the import of NDVI
#' tiles, processing of grids, and saving of final grid data. The function allows
#' control over the output path, temporary directories, and overwrite options.
#'
#' @param census_scales <`data.frame`> Data frame containing census scales.
#' @param base_polygons <`sf object`> Simple Features (sf) object representing
#' base polygons. The output of `create_master_polygon`.
#' @param output_path <`character`> Path to the directory for output files.
#' Default is `"dev/data/ndvi/"`.
#' @param save_grids_dir <`character`> Directory path for saving processed grids.
#' Default is `"dev/data/built/"`.
#' @param overwrite_ndvi_tiles <`logical`> Flag to determine if existing NDVI
#' tiles should be overwritten. Default is `FALSE`.
#' @param overwrite_final_grids <`logical`> Flag to determine if existing final
#' grids should be overwritten. Default is `FALSE`.
#' @param grid_sizes <`character vector`> What should the size of the grid cells?
#' Defaults to `c(30, 60, 120, 300, 600)`. Must multiples of 30.
#' @param crs <`numeric`> EPSG
#'
#' @return <`list`> Returns a list of processed grid data at various cell sizes.
#' @export
ndvi_grids <- function(census_scales, base_polygons,
                       output_path = "dev/data/ndvi/",
                       save_grids_dir = "dev/data/built/",
                       overwrite_ndvi_tiles = FALSE,
                       overwrite_final_grids = FALSE,
                       crs = crs,
                       grid_sizes = c(30, 60, 120, 300, 600)) {

  output_path_tmp <- sprintf("%stmp/", output_path)
  dir.create(output_path, showWarnings = FALSE)
  dir.create(output_path_tmp, showWarnings = FALSE)
  dir.create(save_grids_dir, showWarnings = FALSE)

  # Import tiles
  cc.data::ndvi_import_from_masterpolygon(
    master_polygon = base_polygons$DA_carto,
    years = cc.data::ndvi_years(),
    output_path = output_path,
    temp_folder = output_path_tmp,
    overwrite = overwrite_ndvi_tiles,
    grid_sizes = grid_sizes,
    crs = crs)

  # Function to process and save grids
  process_and_save_grid <- function(grid_size, output_path, save_grids_dir, crs) {
    grid_file <- sprintf("%sgrd%s.qs", output_path, grid_size)
    save_file <- sprintf("%sgrd%s.qs", save_grids_dir, grid_size)

    # Check if file exists and overwrite is FALSE
    if (file.exists(save_file) && !overwrite_final_grids) {
      return(qs::qread(save_file))
    }

    grid <- qs::qread(grid_file)
    grid_geocode <- sf::st_transform(grid, 4326)
    grid_geocode <- sf::st_centroid(grid_geocode)
    grid_name <- future.apply::future_sapply(grid_geocode$geometry,
                                             cc.data::rev_geocode_localhost,
                                             future.seed = NULL)
    grid$name <- grid_name
    grid$ID <- sprintf("grd%s_%s", grid_size, seq_along(grid$geometry))
    grid <- grid[, c("ID", "name", "geometry")]
    grid <- cc.buildr::append_DA_ID(DA_table = census_scales$DA,
                                    df = grid,
                                    crs = crs)

    qs::qsave(grid, file = save_file)
  }

  # Process and save each grid
  grid_cells <- grid_sizes
  grids <- lapply(grid_cells, process_and_save_grid, output_path, save_grids_dir, crs)
  names(grids) <- sprintf("grd%s", grid_cells)

  if (overwrite_final_grids | !"population" %in% names(grids[[length(grids)]])) {
    size <- grid_cells[length(grid_cells)]

    # Add population and households to grd600
    grids[[length(grids)]] <- additional_scale(additional_table = grids[[length(grids)]]["name"],
                                               DB_table = census_scales$DB,
                                               ID_prefix = paste0("grd", size),
                                               name_2 = paste0(size, "-m"),
                                               DA_carto = base_polygons$DA_carto,
                                               switch_to_carto = FALSE,
                                               crs = crs)
    # Append DAs to grd600
    grids[[length(grids)]] <- cc.buildr::append_DA_ID(DA_table = census_scales$DA,
                                                      df = grids[[length(grids)]],
                                                      crs = crs)

    qs::qsave(grids[[length(grids)]], file = sprintf("%sgrd%s.qs", save_grids_dir, size))
  }


  # Return
  return(grids)
}

#' Append NDVI scales to the scales dictionary.
#'
#' This function adds multiple NDVI scale entries to a given dictionary. Each
#' entry contains details about a specific scale, such as its singular and
#' plural forms, slider title, and descriptive text.
#'
#' @param scales_dictionary <`data.frame`> A dictionary to which NDVI scales will be added.
#'
#' @return Updated scales dictionary with NDVI scale entries.
#' @export
scales_dictionary_ndvi <- function(scales_dictionary) {

  scales_dictionary <-
    append_scale_to_dictionary(scales_dictionary,
                               scale = "grd30",
                               sing = "area at the 30m scale",
                               sing_with_article = "the area at the 30m scale",
                               plur = "areas at the 30m scale",
                               slider_title = "30m",
                               place_heading = "{name}",
                               place_name = "the 30m grid area around {name}",
                               subtext = "Small square areas, each measuring 30 meters by 30 meters")

  scales_dictionary <-
    append_scale_to_dictionary(scales_dictionary,
                               scale = "grd60",
                               sing = "area at the 60m scale",
                               sing_with_article = "the area at the 60m scale",
                               plur = "areas at the 60m scale",
                               slider_title = "60m",
                               place_heading = "{name}",
                               place_name = "the 60m grid area around {name}",
                               subtext = "Small square areas, each measuring 60 meters by 60 meters")

  scales_dictionary <-
    append_scale_to_dictionary(scales_dictionary,
                               scale = "grd120",
                               sing = "area at the 120m scale",
                               sing_with_article = "the area at the 120m scale",
                               plur = "areas at the 120m scale",
                               slider_title = "120m",
                               place_heading = "{name}",
                               place_name = "the 120m grid area around {name}",
                               subtext = "Small square areas, each measuring 120 meters by 120 meters")

  scales_dictionary <-
    append_scale_to_dictionary(scales_dictionary,
                               scale = "grd300",
                               sing = "area at the 300m scale",
                               sing_with_article = "the area at the 300m scale",
                               plur = "areas at the 300m scale",
                               slider_title = "300m",
                               place_heading = "{name}",
                               place_name = "the 300m grid area around {name}",
                               subtext = "Small square areas, each measuring 300 meters by 300 meters")

  scales_dictionary <-
    append_scale_to_dictionary(scales_dictionary,
                               scale = "grd600",
                               sing = "area at the 600m scale",
                               sing_with_article = "the area at the 600m scale",
                               plur = "areas at the 600m scale",
                               slider_title = "600m",
                               place_heading = "{name}",
                               place_name = "the 600m grid area around {name}",
                               subtext = "Small square areas, each measuring 600 meters by 600 meters")

  return(scales_dictionary)
}
