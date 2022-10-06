## IMPORT CANBICS DATA #########################################################

canbics_data <-
  utils::read.csv("data-raw/canbics.csv")

canbics_data <- canbics_data[, c("nhbic21_01", "nhbic21_09")]
names(canbics_data) <- c("DA_ID", "canbics_2021")
class(canbics_data$DA_ID) <- "character"

canbics_data <-
  stats::aggregate(canbics_data$canbics_2021,
                   by = list(DA_ID = canbics_data$DA_ID), FUN = mean)
names(canbics_data) <- c("DA_ID", "canbics_2021")

usethis::use_data(canbics_data, overwrite = TRUE)
