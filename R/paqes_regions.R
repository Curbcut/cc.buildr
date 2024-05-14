#' Pages Regions Function
#'
#' This function identifies and filters regions based on the availability and
#' coverage of data in different scales for each module in a given SVM object.
#' It evaluates each region's data presence and only keeps regions that have a
#' high enough percentage of features filled with data.
#'
#' @param svm <`list`> List of scales, variables and modules.
#' @param regions_dictionary <`data.frame`> The regions dictionary,
#'
#' @return <`list`> An SVM object with the `modules` element updated to include
#' `regions`, which are the filtered regions based on the data coverage criteria.
#' @export
pages_regions <- function(svm, regions_dictionary, inst_prefix) {

  scales <- svm$scales
  modules <- svm$modules

  scales_in_db <- db_list_scales(inst_prefix)
  db_conn_prod <- db_connect_prod()

  progressr::with_progress({
    pb <- progressr::progressor(length(modules$id))

    modules$regions <- lapply(modules$id, \(id) {
      vl <- modules$var_left[modules$id == id][[1]]
      vl <- if (!is.data.frame(vl)) vl else vl$var_code
      if (length(vl) > 100) vl <- vl[sample(1:length(vl), 20)]

      # Go over all possible variables on one module
      every_val_reg <- lapply(vl, \(v) {

        # Which are the regions to keep
        avl_reg <- sapply(regions_dictionary$region, \(reg) {

          region_scales <- regions_dictionary$scales[regions_dictionary$region == reg][[1]]

          # Go over all scales in the region
          scales_presence <- mapply(\(scale_name, scale_IDs) {
            is_in_db <- scale_name %in% scales_in_db
            is_grid <- grepl("^grd", scale_name)
            # If there are no features for a scale inside this region, remove it
            # from the count
            if (length(scale_IDs) == 0) return(NULL)
            dat <- sprintf("data/%s/%s.qs", scale_name, v)

            # If grd and data is in SQLite database
            if (is_in_db & !file.exists(dat)) {

              column_names <- DBI::dbGetQuery(db_conn_prod,
                              sprintf(paste0("SELECT column_name FROM ",
                                             "information_schema.columns WHERE ",
                                             "table_schema = '%s' AND ",
                                             "table_name = '%s_%s'"),
                                      inst_prefix, scale_name, v))$column_name

              if (length(column_names) == 0) return(NULL)
              column_names <- column_names["ID" != column_names]


              na_check_conditions <- paste0(column_names, " IS NULL", collapse = " AND ")
              sql_query_template <- paste0("SELECT COUNT(*) AS total_rows, ",
                                           "SUM(CASE WHEN %s THEN 1 ELSE 0 END) ",
                                           'AS na_rows FROM %s."%s_%s" WHERE \"ID\" IN (%s);')


              # Assuming scale_IDs is a vector of IDs
              ids_placeholder <- paste0("'", scale_IDs, "'", collapse = ", ")
              final_query <- sprintf(sql_query_template, na_check_conditions,
                                     inst_prefix, scale_name, v, ids_placeholder)

              # Execute the query
              result <- DBI::dbGetQuery(db_conn_prod, final_query)

              # Calculate the percentage of NA rows
              percentage_na <- (result$total_rows - result$na_rows) / result$total_rows

              return(percentage_na)
            }

            # If there are no features for a scale inside this region, remove it
            # from the count
            if (length(scale_IDs) == 0) return(NULL)
            dat <- sprintf("data/%s/%s.qs", scale_name, v)

            # If the data is not present as a certain scale, do not count it in
            # the count.
            if (!file.exists(dat)) return(NULL)

            dat <- qs::qread(dat)
            scale <- qs::qread(sprintf("data/geometry_export/%s.qs", scale_name))

            # Length of IDs from this region present in the scales' data
            present_in_data <- dat[dat$ID %in% scale_IDs, ]
            if (!"area" %in% names(scale)) {
              scale$area <- if (is_grid) as.numeric(gsub("grd", "", scale_name)) else get_area(scale)
            }
            all_scale <- merge(scale[c("ID", "area")], present_in_data)

            # Do not count if there are missing values (NAs) for more than half of
            # the observations!
            only_values <- sf::st_drop_geometry(subset(all_scale, select = -c(ID, area)))
            only_nas <- apply(only_values, 1, \(x) all(is.na(x)))
            present_in_data <- all_scale[!only_nas, ]

            # As a percentage of all the IDs needed to fill in the region
            sum(present_in_data$area) / sum(all_scale$area)
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

      pb()
      return(kept_regs)

    })
  })

  svm$modules <- modules

  db_disconnect_prod(db_conn_prod)

  return(svm)

}
