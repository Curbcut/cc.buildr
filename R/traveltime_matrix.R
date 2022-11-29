tt_matrix <- function(DA_table,
                      r5r_data_path = "C:/Users/maxim/Unsync/Sus/dev/data/routing") {

  if (!requireNamespace("r5r", quietly = TRUE)) {
    stop(
      "Package \"r5r\" must be installed to use this function.",
      call. = FALSE
    )
  }

  setup_r5(data_path = r5r_data_path, verbose = FALSE)

}
