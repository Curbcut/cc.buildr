## OPENSTREETMAP PBF DOWNLOAD LINKS ############################################

provinces_pbf <- cancensus::get_census(cc.buildr::current_census,
  regions = list(C = 01),
  level = "PR",
  geo_format = "sf"
)

provinces_pbf <- provinces_pbf[, "name"]

provinces_pbf$link <- NA

provinces_pbf$link[provinces_pbf$name == "Newfoundland and Labrador"] <-
  "http://download.geofabrik.de/north-america/canada/newfoundland-and-labrador-latest.osm.pbf"

provinces_pbf$link[provinces_pbf$name == "Prince Edward Island"] <-
  "http://download.geofabrik.de/north-america/canada/prince-edward-island-latest.osm.pbf"

provinces_pbf$link[provinces_pbf$name == "Nova Scotia"] <-
  "http://download.geofabrik.de/north-america/canada/nova-scotia-latest.osm.pbf"

provinces_pbf$link[provinces_pbf$name == "New Brunswick"] <-
  "http://download.geofabrik.de/north-america/canada/new-brunswick-latest.osm.pbf"

provinces_pbf$link[provinces_pbf$name == "Quebec"] <-
  "http://download.geofabrik.de/north-america/canada/quebec-latest.osm.pbf"

provinces_pbf$link[provinces_pbf$name == "Ontario"] <-
  "http://download.geofabrik.de/north-america/canada/ontario-latest.osm.pbf"

provinces_pbf$link[provinces_pbf$name == "Manitoba"] <-
  "http://download.geofabrik.de/north-america/canada/manitoba-latest.osm.pbf"

provinces_pbf$link[provinces_pbf$name == "Saskatchewan"] <-
  "http://download.geofabrik.de/north-america/canada/saskatchewan-latest.osm.pbf"

provinces_pbf$link[provinces_pbf$name == "Alberta"] <-
  "http://download.geofabrik.de/north-america/canada/alberta-latest.osm.pbf"

provinces_pbf$link[provinces_pbf$name == "British Columbia"] <-
  "http://download.geofabrik.de/north-america/canada/british-columbia-latest.osm.pbf"


usethis::use_data(provinces_pbf, overwrite = TRUE)
