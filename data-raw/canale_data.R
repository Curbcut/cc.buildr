## IMPORT CANALE DATA ##########################################################

canale_data <-
  utils::read.csv("data-raw/canale.csv")

canale_data <- canale_data[, c("DAUID", "ale_index")]
names(canale_data) <- c("DA_ID", "canale_2016")
class(canale_data$DA_ID) <- "character"

usethis::use_data(canale_data, overwrite = TRUE)
