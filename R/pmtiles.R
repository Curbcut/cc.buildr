input_shp <- "test_build_mtl/arrondissements_mtl.shp"
output_pmtiles <- "output.pmtiles"
output_dir = getwd()

# Define the function
pmtiles_create <- function(input_shp, output_pmtiles, output_dir = getwd()) {
  # Try running 'tippecanoe --version' to check if it's installed
  tippecanoe_check <- system("tippecanoe --version")

  # Check if the command executed successfully
  if (tippecanoe_check != 0) {
    # If Tippecanoe isn't found or doesn't work, guide the user
    message("Tippecanoe is not installed or not found in the system PATH.")
    message("Please download Tippecanoe from https://github.com/mapbox/tippecanoe.")
    message("If you have Tippecanoe installed via Cygwin, add its bin directory to your PATH.")
    message("To do this, update the PATH in .Renviron to include C:/cygwin64/usr/local/bin and C:/cygwin64/bin")
    return()
  }

  # Read the shapefile using the 'sf' package
  shape_data <- sf::st_read(input_shp)
  input_shp <- sf::st_transform(shape_data, crs = 4326)

  # Convert shapefile to a temporary GeoJSON file
  temp_geojson <- tempfile(fileext = ".geojson")
  sf::st_write(shape_data, temp_geojson, delete_dsn = TRUE)

  output_path <- file.path(output_dir, output_pmtiles)

  # Construct the Tippecanoe command to create the PMTiles
  tippecanoe_cmd <- paste(
    "tippecanoe",
    "--output", output_path,  # Specify the output pmtiles file
    "--no-tile-size-limit",      # Remove tile size limit (optional)
    "--no-feature-limit",        # Remove feature limit (optional)
    temp_geojson                 # Input GeoJSON file
  )

  # Run Tippecanoe to generate the PMTiles
  system(tippecanoe_cmd, intern = TRUE, ignore.stderr = TRUE)

}

