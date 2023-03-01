#' Create page script
#'
#' @param source_file <`character`> File where all inputs are like module `id`,
#' `var_left`, `var_right`, `time`, etc.
#' @param R_folder <`character`> Location where the new page script will be created.
#' Defaults to `"R/"`
#' @param overwrite <`logical`> If a file already exists, should it be overriden.
#' Defaults to `FALSE`. The file name will be made using the `id` object from
#' the `source_file`. `R/m_id.R`.
#' @param auto_left_vars <`logical`> Should the `auto_vars_UI` and
#' `auto_vars_server` module be used to dynamically create dropdowns for the
#' left variables.
#'
#' @return Opens the new page script created.
#' @export
create_page_script <- function(source_file, R_folder = "R/", overwrite = FALSE,
                               auto_left_vars = FALSE) {
  # Create some visual binding for global variable, to silence warnings
  # of devtools::check().
  id <- NULL
  var_left <- NULL
  var_right <- NULL
  time <- NULL
  group_name_label <- NULL


  # Source the file containing all the variables
  source(source_file, local = TRUE)


  # Error check
  if (is.null(id)) stop("Missing the `id` object in the `source_file`.")
  if (is.null(var_left)) {
    stop("Missing the `var_left` object in the `source_file`.")
  }
  if (is.null(var_right)) {
    stop("Missing the `var_right` object in the `source_file`.")
  }
  if (is.null(time)) {
    stop("Missing the `time` object in the `source_file`.")
  }


  new_file <- paste0("R/m_", id, ".R")

  # If file exists, just open it
  if (file.exists(new_file) && !overwrite) {
    return(file.show(new_file))
  }


  # Create file
  new_file <- paste0("R/m_", id, ".R")
  file.create(new_file)
  new_file_connection <- file(new_file)


  # Pre-fill the file
  uis <- c()
  more_args <- c()

  # Function to substitute as vectors
  write_as_vector <- function(x) {
    if (length(x) == 1) {
      return(paste0('"', x, '"'))
    }

    vec <-
      sapply(seq_along(x), \(z) paste0('"', x[[z]], '"', if (z %% 3 == 0) "\n")) |>
      paste0(collapse = ", ")
    vec <- gsub("\\\n,", ",\n", vec)
    return(paste0("c(", vec, ")"))
  }

  # ID
  template <- readLines(system.file("page_script_template.R",
    package = "cc.buildr"
  ))
  template[1] <- gsub("__id__", toupper(id), template[1])
  template[2:length(template)] <- gsub("__id__", id, template[2:length(template)])
  pound_nb <- 79 - nchar(template[1])
  template[1] <- paste(template[1], paste0(rep("#", pound_nb), collapse = ""))

  # var_left
  if (length(var_left) == 1) {
    template <- gsub("`__var_left__`", write_as_vector(var_left), template)
  } else {
    if (!auto_left_vars) {
      vl_ui <- readLines(system.file("modules/var_left_ui.R",
        package = "cc.buildr"
      ))
      vl_ui <- gsub("`__var_left__`", write_as_vector(var_left), vl_ui)
      uis <- c(uis, vl_ui)

      vl_serv <- readLines(system.file("modules/var_left_server.R",
        package = "cc.buildr"
      ))
      vl_serv <- gsub("`__var_left__`", write_as_vector(var_left), vl_serv)
      template <- gsub(
        paste0(
          "var_left <- reactive\\(paste\\(`__var_left__`, ",
          'time\\(\\), sep = "_"\\)\\)'
        ),
        paste0(vl_serv, collapse = "\n"), template
      )
    } else {
      if (is.null("group_name_label")) {
        stop(paste0(
          "Add a `group_name_label` to name the main left variable ",
          "dropdown."
        ))
      }
      au_vl_ui <- readLines(system.file("modules/auto_var_left_ui.R",
        package = "cc.buildr"
      ))
      au_vl_ui <- gsub("`__var_left__`", write_as_vector(var_left), au_vl_ui)
      au_vl_ui <- gsub(
        "`__group_name_label__`", write_as_vector(group_name_label),
        au_vl_ui
      )
      uis <- c(uis, au_vl_ui)

      au_vl_serv <- readLines(system.file("modules/auto_var_left_server.R",
        package = "cc.buildr"
      ))
      au_vl_serv <- gsub("`__var_left__`", write_as_vector(var_left), au_vl_serv)
      template <- gsub("# Time", "", template)
      template <- gsub("time <- reactive\\(`__time__`\\)", "", template)
      template <- gsub("# Left variable", "", template)
      template <- gsub(
        paste0(
          "var_left <- reactive\\(paste\\(`__var_left__`, ",
          'time\\(\\), sep = "_"\\)\\)'
        ),
        paste0(au_vl_serv, collapse = "\n"), template
      )
    }
  }

  # var_right
  template <- gsub("`__var_right__`", write_as_vector(var_right), template)

  # time
  if (length(time) == 1) {
    template <- gsub("`__time__`", write_as_vector(time), template)
  } else {
    ui <- readLines(system.file("modules/time_ui.R", package = "cc.buildr"))
    ui <- gsub("`__min__year`", min(time), ui)
    ui <- gsub("`__mid__year`", time[ceiling(length(time) / 2)], ui)
    ui <- gsub("`__max__year`", max(time), ui)
    uis <- c(uis, ui)

    template <- gsub(
      "    time <- reactive\\(`__time__`\\)",
      readLines(system.file("modules/time_server.R",
        package = "cc.buildr"
      )) |>
        paste0(collapse = "\n"),
      template
    )
  }

  # Add all uis
  template <-
    if (length(uis) == 0) gsub("`__widgets_UI__`,", "", template) else {
      gsub(
        "`__widgets_UI__`,",
        paste0(paste0(uis, collapse = ",\n"), ","),
        template
      )
    }

  # Write the latter to the file
  writeLines(template, con = new_file_connection)

  # Message
  message(paste0(
    "Make sure the page is refered in the id column of the ",
    "`modules` data.frame."
  ))

  close.connection(new_file_connection)

  if (requireNamespace("styler", quietly = TRUE)) styler::style_file(new_file)

  file.show(new_file)
}
