## IMPORT POPULATION-WEIGHTED DA CENTROIDS #####################################

da_centroids_popw <-
  utils::read.csv("data-raw/da_centroids_popw.csv")[
    , c("DAuid.ADidu", "DArplong.ADlong", "DArplat.Adlat")] |>
  unique() |>
  sf::st_as_sf(coords = c("DArplong.ADlong", "DArplat.Adlat"),
           crs = 4326)

names(da_centroids_popw) <- c("ID", "geometry")

usethis::use_data(da_centroids_popw, overwrite = TRUE)
