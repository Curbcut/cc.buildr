#' Pages Regions Function
#'
#' This function identifies and filters regions based on the availability and
#' coverage of data in different scales for each module in a given SVM object.
#' It evaluates each region's data presence and only keeps regions that have a
#' high enough percentage of features filled with data.
#'
#' @param svm <`list`> A list representing the SVM object, which must contain
#' `modules` and `data` elements. `scales_variables_modules`
#' @param regions_dictionary <`data.frame`> The regions dictionary,
#'
#' @return <`list`> An SVM object with the `modules` element updated to include
#' `regions`, which are the filtered regions based on the data coverage criteria.
#' @export
pages_regions <- function(svm, regions_dictionary) {
  modules <- svm$modules
  data <- svm$data

  modules$regions <- lapply(modules$id, \(id) {
    vl <- modules$var_left[modules$id == id][[1]]
    vl <- if (!is.data.frame(vl)) vl else vl$var_code
    if (length(vl) > 100) vl <- vl[sample(1:length(vl), 50)]

    # Go over all possible variables on one module
    every_val_reg <- lapply(vl, \(v) {

      # Which are the regions to keep
      avl_reg <- sapply(regions_dictionary$region, \(reg) {

        region_scales <- regions_dictionary$scales[regions_dictionary$region == reg][[1]]

        # Go over all scales in the region
        scales_presence <- mapply(\(scale_name, scale_IDs) {
          # If there are no features for a scale inside this region, remove it
          # from the count
          if (length(scale_IDs) == 0) return(NULL)

          dat <- data[[scale_name]][[v]]
          # If the data is not present as a certain scale, do not count it in
          # the count.
          if (is.null(dat)) return(NULL)

          # Length of IDs from this region present in the scales' data
          present_in_data <- dat[dat$ID %in% scale_IDs, ]

          # Do not count if there are missing values (NAs) for more than half of
          # the observations!
          present_in_data <- subset(present_in_data, select = -ID)
          only_nas <- apply(present_in_data, 1, \(x) all(is.na(x)))
          present_in_data <- present_in_data[!only_nas, ]

          # As a percentage of all the IDs needed to fill in the region
          nrow(present_in_data) / length(scale_IDs)
        }, names(region_scales), region_scales)

        # Remove the scales for which no data was calculated
        scales_presence <- scales_presence[!sapply(scales_presence, is.null)]
        # Get the average: what is the average percentage of features that are
        # from a region that are filled with data for this particular variable.
        scales_presence_avg <- mean(unlist(scales_presence))

        # For the region to be valid, it needs to have at least 80% of the
        # features filled with data.
        scales_presence_avg > .80

      }, simplify = TRUE, USE.NAMES = TRUE)

      names(avl_reg)[avl_reg]
    })

    # Only keep regions if they are present for 3/4 the variable on the page
    kept_regs <- table(unlist(every_val_reg)) / length(every_val_reg) > 0.75
    kept_regs <- names(kept_regs)[kept_regs]

    # Keep the order from the regions dictionary
    kept_regs <- kept_regs[order(match(kept_regs, regions_dictionary$region))]

    return(kept_regs)

  })
  svm$modules <- modules

  return(svm)
}

