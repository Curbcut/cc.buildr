### DYK FUNCTIONS ##############################################################

#' Prepare a Variable Table for DYK Generation
#'
#' This function creates a table of variable combinations which serves as the
#' input to the "Did you know" generation process.
#'
#' @param svm <`list`> A list, usually `scales_variables_modules`, containing
#' the scales, modules, and variables tables.
#' @param scales_dictionary <`tibble`> scales_dictionary
#' @param n <`integer`> Optionally, an integer scalar specifying the number of
#' rows in `svm$modules` to be used for creating the DYK variable table. The
#' default (`NULL`) will use all rows; other values may be useful for testing.
#'
#' @return A data frame with six columns (`module`, `region`, `scale`, `date`,
#' `var_left` and `var_right`), each of which is a character vector.
#' @export
dyk_prep <- function(svm, scales_dictionary, n = NULL) {

  modules <- svm$modules
  variables <- svm$variables
  if (!missing(n) && !is.null(n)) modules <- modules[1:n,]

  # Remove the scales that we don't have a geometry export fort. These are directly
  # in the postgres table, and are too heavy. Unnecessary to calculate DYK for.
  sd <- scales_dictionary
  sd <- sd[sd$scale %in% gsub(".qs", "", list.files("data/geometry_export")), ]

  # Create a data.frame that mimics a tibble
  regions <- lapply(names(sd$regions), function(name) {
    tibble::tibble(
      scale = rep(name, length( sd$regions[[name]])),
      region = sd$regions[[name]]
    )
  })

  # Combine the data.frame list into a single data.frame
  regions <- do.call(rbind, regions)

  vars_dyk <-
    modules |>
    dplyr::select(module = id, region = regions, var_left, var_right,
                  date = dates) |>
    tidyr::unnest(region) |>
    dplyr::left_join(regions, by = "region", relationship = "many-to-many") |>
    dplyr::relocate(scale, .after = region) |>
    tidyr::unnest(scale) |>
    dplyr::mutate(date = lapply(date, as.character)) |>
    tidyr::unnest(date) |>
    # For now, filter away modules with huge numbers of variables; eventually we
    # should designate one variable per group to get a DYK for the whole group,
    # which would require a "group_default" logical field. TKTK
    dplyr::filter(!sapply(var_left, tibble::is_tibble)) |>
    tidyr::unnest(var_left) |>
    # Add a var_right = " " to each row to preserve simple univariate cases
    dplyr::mutate(var_right = lapply(var_right, \(x) c(x, " "))) |>
    tidyr::unnest(var_right)

  variables$dates <- lapply(variables$dates, as.character)

  var_left_check <-
    variables |>
    dplyr::filter(var_code %in% vars_dyk$var_left) |>
    dplyr::select(var_left = var_code, scale = avail_scale, date = dates) |>
    tidyr::unnest(date) |>
    tidyr::unnest(scale) |>
    dplyr::left_join(regions, by = "scale", relationship = "many-to-many")

  var_right_check <-
    variables |>
    dplyr::filter(var_code %in% vars_dyk$var_right) |>
    dplyr::select(var_right = var_code, scale = avail_scale, date = dates) |>
    reframe(
      var_right = c(" ", var_right),
      scale = c(list(unique(unlist(scale))), scale),
      date = c(list(as.character(
        min(as.numeric(unlist(date)) - 5):max(as.numeric(unlist(date)) + 5))),
        date)) |>
    tidyr::unnest(date) |>
    tidyr::unnest(scale) |>
    dplyr::left_join(regions, by = "scale", relationship = "many-to-many")

  vars_dyk <-
    vars_dyk |>
    dplyr::semi_join(var_left_check,
                     by = c("var_left", "region", "scale", "date")) |>
    dplyr::semi_join(var_right_check,
                     by = c("var_right", "region", "scale", "date")) |>
    # Manually remove grid region. TKTK maybe eventually this should be
    # data-driven instead of manually specified?
    dplyr::filter(region != "grid")

  vars_dyk <-
    vars_dyk |>
    dplyr::arrange(module, region, scale, var_left, var_right, date)

  return(vars_dyk)

}


# Univariate --------------------------------------------------------------

#' Create a Table of Univariate DYKs
#'
#' This function creates a table of univariate "Did you know" text strings.
#'
#' @param vars_dyk <`data.frame`> A data frame with columns `module`,
#' `region`, `scale`, `var_left`, `var_right` and `date`, probably created with
#' \code{\link[cc.buildr]{dyk_prep}}.
#' @param svm <`list`> A list, usually `scales_variables_modules`, containing
#' the scales, modules, and variables tables.
#' @param scales_dictionary <`data.frame`> A data frame with information about
#' scale names and text strings
#' @param regions_dictionary <`data.frame`> A data frame with information about
#' which feature of every scale fits in which region.
#' @param langs <`character`> A character vector specifying for which languages
#' DYK strings should be produced. Allowable values are "en" and "fr".
#' @param translation_df <`data.frame`> A data frame with columns for each
#' language present in the `langs` argument.
#'
#' @return A data frame with ten columns (the columns present in `vars_dyk`,
#' and then additionally `select_ID`, `dyk_type`, `dyk_text` and `dyk_weight`).
#' @export
dyk_uni <- function(vars_dyk, svm, scales_dictionary, regions_dictionary, langs,
                    translation_df = NULL) {
  first_scale <- scales_dictionary$scale[[1]]

  # Prepare translation_df
  assign("translation_df", value = translation_df, envir = as.environment(1))
  assign("variables", value = svm$variables, envir = as.environment(1))

  # Assign scales in global environment to use explore_text_region_val_df
  sapply(unique(vars_dyk$scale), \(x) {
    assign(x, qs::qread(sprintf("data/geometry_export/%s.qs", x)), envir = .GlobalEnv)
  })
  # Get back the scales that have been unloaded in the data building process
  svm$scales <- sapply(unique(vars_dyk$scale), \(x) {
    qs::qread(sprintf("data/geometry_export/%s.qs", x))
  }, simplify = FALSE, USE.NAMES = TRUE)

  # Get highest/lowest DYKs
  dyk_highest <- vars_dyk[vars_dyk$var_right == " ", ]
  dyk_highest_out <- dyk_uni_highest_lowest(
    var_left = dyk_highest$var_left,
    region = dyk_highest$region,
    scale = dyk_highest$scale,
    date = dyk_highest$date,
    svm = svm,
    scales_dictionary = scales_dictionary,
    regions_dictionary = regions_dictionary,
    langs = langs)
  dyk_highest <- dplyr::bind_cols(dyk_highest, dyk_highest_out)
  dyk_highest_in_build <- tibble::tibble()
  for (i in seq_along(langs)) {
    sel_vec <- paste0(c("highest_", "lowest_"), langs[i])

    out <-
      dyk_highest |>
      tidyr::pivot_longer(c(all_of(sel_vec)),
                          names_to = if (i == 1) "dyk_type" else NULL,
                          values_to = paste0("dyk_text_", langs[i])
      )

    # Remove the other languages from out
    if (i == 1) {
      other_langs <- langs[langs != langs[[i]]]
      if (length(other_langs) == 0) {
        dyk_highest_in_build <- out
        next
      }
      other_lang_cols <- paste0(sprintf("_%s$", other_langs), collapse = "|")
      dyk_highest_in_build <- out[!grepl(other_lang_cols, names(out))]
      next
    }

    # Bind current lang text
    dyk_highest_in_build <- cbind(dyk_highest_in_build, out[sprintf("dyk_text_%s", langs[[i]])])
  }
  dyk_highest <- tibble::as_tibble(dyk_highest_in_build)

  dyk_highest <- dyk_highest |>
    dplyr::mutate(dyk_weight = 0.5) |>
    dplyr::mutate(select_ID = if_else(
      dyk_type == "highest_en", highest_ID, lowest_ID), .after = date) |>
    dplyr::mutate(dyk_type = stringr::str_remove(
      dyk_type, paste0("_", paste(langs, collapse = "|")))) |>
    dplyr::select(-highest_ID, -lowest_ID) |>
    dplyr::mutate(date = lapply(date, \(x) x))

  # Get change DYKs
  dyk_change <-
    vars_dyk |>
    dplyr::filter(var_right == " ") |>
    dplyr::filter(n() > 1, .by = c(module, region, scale, var_left)) |>
    dplyr::filter(scale == first_scale) |>
    dplyr::summarize(date = list(as.character(c(min(as.numeric(date)),
                                                max(as.numeric(date))))),
                     .by = c(module, region, scale, var_left, var_right))
  dyk_change_out <- dyk_uni_change(
    var_left = dyk_change$var_left,
    region = dyk_change$region,
    scale = dyk_change$scale,
    date = dyk_change$date,
    svm = svm,
    langs = langs)
  dyk_change$scale <- NA_character_
  dyk_change <- dplyr::bind_cols(dyk_change, dyk_change_out)
  dyk_change <-
    dyk_change |>
    dplyr::filter(!is.infinite(dyk_weight)) |>
    dplyr::mutate(dyk_type = "change", .before = dyk_text_en) |>
    dplyr::mutate(select_ID = NA_character_, .after = date)

  # Get compare DYKs
  dyk_compare <- vars_dyk[vars_dyk$var_right != " ", ]
  dyk_compare_out <- dyk_uni_compare(
    var_left = dyk_compare$var_left,
    var_right = dyk_compare$var_right,
    region = dyk_compare$region,
    scale = dyk_compare$scale,
    date = dyk_compare$date,
    svm = svm,
    regions_dictionary = regions_dictionary,
    langs = langs)
  dyk_compare <- dplyr::bind_cols(dyk_compare, dyk_compare_out)
  dyk_compare <-
    dyk_compare |>
    dplyr::mutate(dyk_type = "compare", .before = dyk_text_en) |>
    # Only keep rows with correlation > 0.3
    dplyr::filter(abs(dyk_weight) > 0.09) |>
    dplyr::mutate(select_ID = NA_character_, .after = date) |>
    dplyr::mutate(date = lapply(date, \(x) x))

  dyk <-
    dplyr::bind_rows(dyk_highest, dyk_change, dyk_compare) |>
    dplyr::arrange(module, region, scale, var_left, var_right, dyk_type)

  return(dyk)

}


#' Generate Highest/Lowest Value DYKs
#'
#' This function creates "Did you know" text strings highlighting the highest
#' and lowest values from a combination of variable, region, scale and date.
#' The output is a data frame containing a `highest` and `lowest` column.
#'
#' @param var_left <character> A string representing the name of the variable
#' for which the DYK should be calculated.
#' @param region <`character`> A string representing the name of the region
#' for which the DYK should be calculated.
#' @param scale <`character`> A string representing the name of the scale
#' for which the DYK should be calculated.
#' @param date <`character`> A string representing the name of the date
#' for which the DYK should be calculated.
#' @param svm <`list`> A list, usually `scales_variables_modules`, containing
#' the scales, modules, and variables tables.
#' @param scales_dictionary <`data.frame`> A data frame with information about
#' scale names and text strings
#' @param regions_dictionary <`data.frame`> A data frame with information about
#' which feature of every scale fits in which region.
#' @param langs <`character`> A character vector specifying for which languages
#' DYK strings should be produced. Allowable values are "en" and "fr".
#'
#' @return A data frame with four columns (`highest`, `lowest`, `highest_ID`,
#' and `lowest_ID`), each of which contains a character vector of DYK outputs.
#' @export
dyk_uni_highest_lowest <- function(var_left, region, scale, date, svm,
                                   scales_dictionary, regions_dictionary, langs) {

  # Get class
  vars <- mapply(curbcut::vars_build,
                 var_left = paste(var_left, date, sep = "_"),
                 scale = scale,
                 time = date,
                 MoreArgs = list(
                   check_choropleth = FALSE,
                   variables = svm$variables),
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)
  vars <- lapply(vars, `[[`, "vars")

  # Initial region mention (one per lang)
  region_start <- lapply(langs, \(lang) {
    out <- mapply(\(x, y) curbcut:::explore_context(
      region = x, select_id = NA, scale = y, switch_DA = FALSE,
      lang = lang), x = region, y = scale, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    out <- sapply(out, \(x) gsub(" $", "", x$p_start))
    curbcut::s_sentence(out)
  })

  # Scale name (one per lang)
  scale_name <- scales_dictionary$sing_with_article[sapply(
    scale, \(x) which(scales_dictionary$scale == x), USE.NAMES = FALSE)]
  scale_name <- lapply(langs, \(x) sapply(scale_name, curbcut::cc_t, lang = x))

  # Highest value (only one for now)
  highest_val <- mapply(\(vl, sc, dt, reg) {
    ids <- regions_dictionary$scales[regions_dictionary$region == reg][[1]][[sc]]
    tb <- qs::qread(sprintf("data/%s/%s.qs", sc, vl))
    tb <- tb[tb$ID %in% ids, ]
    max(tb[[paste(vl, dt, sep = "_")]], na.rm = TRUE)
  }, var_left, scale, date, region, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Highest ID (only one)
  highest_ID <- mapply(\(vl, sc, dt, reg) {
    ids <- regions_dictionary$scales[regions_dictionary$region == reg][[1]][[sc]]
    tb <- qs::qread(sprintf("data/%s/%s.qs", sc, vl))
    tb <- tb[tb$ID %in% ids, ]
    tb$ID[which.max(tb[[paste(vl, dt, sep = "_")]])]
  }, var_left, scale, date, region, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Highest name (only one)
  highest_name <- mapply(\(sc, id) {
    tb <- svm$scales[[sc]]
    tb$name[tb$ID == id]
  }, scale, highest_ID, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Lowest value (only one for now)
  lowest_val <- mapply(\(vl, sc, dt, reg) {
    ids <- regions_dictionary$scales[regions_dictionary$region == reg][[1]][[sc]]
    tb <- qs::qread(sprintf("data/%s/%s.qs", sc, vl))
    tb <- tb[tb$ID %in% ids, ]
    min(tb[[paste(vl, dt, sep = "_")]], na.rm = TRUE)
  }, var_left, scale, date, region, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Lowest ID (only one)
  lowest_ID <- mapply(\(vl, sc, dt, reg) {
    ids <- regions_dictionary$scales[regions_dictionary$region == reg][[1]][[sc]]
    tb <- qs::qread(sprintf("data/%s/%s.qs", sc, vl))
    tb <- tb[tb$ID %in% ids, ]
    tb$ID[which.min(tb[[paste(vl, dt, sep = "_")]])]
  }, var_left, scale, date, region, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Lowest name (only one)
  lowest_name <- mapply(\(var_left, scale, lowest_ID) {
    tb <- svm$scales[[scale]]
    tb$name[tb$ID == lowest_ID]
  }, var_left, scale, lowest_ID, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Name prefix (one per lang)
  name_pre <- mapply(\(highest_ID, var_left, region, scale, date) {

    # tb <- qs::qread(sprintf("data/%s/%s.qs", scale, var_left))
    # tb_top_name <- names(svm$scales[[region]][1])
    #
    # if (scale != tb_top_name) {
    #
    #   scales_dictionary |>
    #     dplyr::filter(scale == !!scale) |>
    #     dplyr::pull(sing_with_article)
    #
    # } else
    ""

  }, highest_ID, var_left, region, scale, date, USE.NAMES = FALSE,
  SIMPLIFY = TRUE)
  name_pre <- sapply(langs, \(x) sapply(name_pre, curbcut::cc_t, lang = x),
                     simplify = FALSE, USE.NAMES = TRUE)
  name_pre <- lapply(name_pre, \(x) ifelse(x != "", paste0(x, " "), x))
  name_pre$en <- gsub("^the ", "", name_pre$en)

  # Name suffix (one per lang)
  name_suf <- mapply(\(highest_ID, var_left, region, scale, date) {

    # tb <- qs::qread(sprintf("data/%s/%s.qs", scale, var_left))
    # tb_top_name <- names(svm$scales[[region]][1])
    #
    # if (scale != tb_top_name) {
    #   tb_top <- svm$scales[[region]][[1]]
    #   top_ID <- tb[[paste0(tb_top_name, "_ID")]][tb$ID == highest_ID]
    #   tb_top$name[tb_top$ID == top_ID]
    # } else
    ""

  }, highest_ID, var_left, region, scale, date, USE.NAMES = FALSE,
  SIMPLIFY = TRUE)
  name_suf <- lapply(langs, \(x) {
    ifelse(name_suf != "",
           paste("", if (x == "en") sprintf("in %s", name_suf) else sprintf("(%s)", name_suf)),
           name_suf)
  })

  # Variable explanation (one per lang)
  var_exp <- svm$variables$explanation_nodet[sapply(
    var_left, \(x) which(x == svm$variables$var_code),
    USE.NAMES = FALSE)]
  var_exp <- lapply(langs, \(x) sapply(var_exp, curbcut::cc_t, lang = x))

  # Get max date for each variable
  max_date <-
    tibble::tibble(var_left = var_left, date = date) |>
    dplyr::summarize(max_date = max(date), .by = var_left)

  # Extra date (one per lang)
  # Use max_date to decide on date handling
  extra_date <- mapply(\(x, y) ifelse(
    y == max_date$max_date[max_date$var_left == x], "", paste0(" in ", y)),
    var_left, date, USE.NAMES = FALSE)
  extra_date <- lapply(langs, \(x) {
    if (x == "en") extra_date else str_replace(extra_date, "in", "en")})

  # Is/was (one per lang)
  is_was <- mapply(\(x, y) ifelse(
    y == max_date$max_date[max_date$var_left == x], "is", "was"),
    var_left, date, USE.NAMES = FALSE)
  is_was <- lapply(langs, \(x) {
    if (x == "en") is_was else str_replace(str_replace(is_was, "is", "est"),
                                           "was", "était")})

  # Convert values
  highest_val <- dyk_val_convert(var_left, vars, region, scale, highest_val, svm, langs)
  lowest_val <- dyk_val_convert(var_left, vars, region, scale, lowest_val, svm, langs)

  # Assemble output
  highest <- dyk_assemble_highest(
    region_start, extra_date, highest_name, is_was, scale_name, var_exp,
    highest_val, name_pre, name_suf, langs)
  lowest <- dyk_assemble_lowest(
    region_start, extra_date, lowest_name, is_was, scale_name, var_exp,
    lowest_val, name_pre, name_suf, langs)

  highest_df <-
    do.call(tibble::tibble, set_names(highest, paste0("highest_", langs)))
  lowest_df <-
    do.call(tibble::tibble, set_names(lowest, paste0("lowest_", langs)))

  dplyr::bind_cols(highest_df, lowest_df) |>
    dplyr::mutate(highest_ID = highest_ID,
                  lowest_ID = lowest_ID)

}

dyk_val_convert <- function(var_left, vars, region, scale, vals, svm, langs, variables) {

  lapply(langs, \(lang) {
    mapply(\(vl, var, reg, scl, val) {
      if (inherits(var$var_left, "ind")) {

        ind_raw_dat <- qs::qread(sprintf("data/%s/%s.qs", scl, vl))
        breaks <- attr(ind_raw_dat, "breaks")

        rn <- svm$variables$rank_name[svm$variables$var_code == vl][[1]]
        out <- (\(x) {
          if (val >= breaks[6]) return(5)
          if (val >= breaks[5]) return(5)
          if (val >= breaks[4]) return(4)
          if (val >= breaks[3]) return(3)
          if (val >= breaks[2]) return(2)
          return(1)
        })()

        curbcut::cc_t(rn[out], lang = lang)

      } else curbcut::convert_unit(var = var$var_left, x = as.numeric(val), decimal = 1,
                                   compact = FALSE)
    }, var_left, vars, region, scale, vals, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  })

}

dyk_assemble_highest <- function(
    region_start, extra_date, highest_name, is_was, scale_name, var_exp,
    highest_val, name_pre, name_suf, langs) {

  lapply(langs, \(lang) {

    if (lang == "en") {
      paste0(region_start[[1]], extra_date[[1]], ", ", name_pre[[1]],
             highest_name, name_suf[[1]], " ", is_was[[1]], " ",
             scale_name[[1]], " with the highest ", var_exp[[1]], " (",
             highest_val[[1]], ").")

    } else if (lang == "fr") {
      paste0(region_start[[2]], extra_date[[2]], ", ", name_pre[[2]],
             highest_name, name_suf[[2]], " ", is_was[[2]], " ",
             scale_name[[2]], " avec le/la plus haut/e ", var_exp[[2]], " (",
             highest_val[[2]], ").")
    }})

}

dyk_assemble_lowest <- function(
    region_start, extra_date, lowest_name, is_was, scale_name, var_exp,
    lowest_val, name_pre, name_suf, langs) {

  lapply(langs, \(lang) {

    if (lang == "en") {
      paste0(region_start[[1]], extra_date[[1]], ", ", name_pre[[1]],
             lowest_name, name_suf[[1]], " ", is_was[[1]], " ",
             scale_name[[1]], " with the lowest ", var_exp[[1]], " (",
             lowest_val[[1]], ").")

    } else if (lang == "fr") {
      paste0(region_start[[2]], extra_date[[2]], ", ", name_pre[[2]],
             lowest_name, name_suf[[2]], " ", is_was[[2]], " ",
             scale_name[[2]], " avec le/la plus bas/se ", var_exp[[2]], " (",
             lowest_val[[2]], ").")
    }})

}


#' Generate Change-over-time DYKs
#'
#' This function creates "Did you know" text strings highlighting the change
#' over time from a combination of variable, region and scale. The output is a
#' data frame containing a `change_text` and `change_val` column.
#'
#' @param var_left <character> A string representing the name of the variable
#' for which the DYK should be calculated.
#' @param region <`character`> A string representing the name of the region
#' for which the DYK should be calculated.
#' @param scale <`character`> A string representing the name of the scale
#' for which the DYK should be calculated.
#' @param date <`list of character vector`> Reprenseting the dates for which
#' the DYK should be calculated2
#' @param svm <`list`> A list, usually `scales_variables_modules`, containing
#' the scales, modules, and variables tables.
#'
#' @return A data frame with two columns (`change_text` and `change_val`),
#' which contain a character vector of DYK outputs and a numeric vector of the
#' values respectively.
#' @export
dyk_uni_change <- function(var_left, region, scale, date, svm, langs) {

  # Get class
  vars <- mapply(curbcut::vars_build,
                 var_left = var_left,
                 scale = scale,
                 time = date,
                 MoreArgs = list(
                   check_choropleth = FALSE,
                   variables = svm$variables),
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)
  time <- lapply(vars, `[[`, "time")
  vars <- lapply(vars, `[[`, "vars")
  data <- mapply(curbcut::data_get,
                 vars = vars,
                 scale = scale,
                 region = region,
                 time = time,
                 MoreArgs = list(
                   check_choropleth = FALSE,
                   variables = svm$variables),
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)

  values <-mapply(\(var, dat, tim, sc, reg) {

    # Grab values for single years. Allow for `apply`
    var_lr <- sprintf("var_%s", "left")
    times <- sapply(tim[[var_lr]], \(x) setNames(list(x), var_lr),
                    simplify = FALSE, USE.NAMES = TRUE)
    names(times) <- tim[[var_lr]]

    # Grab the region values
    region_vals <-
      lapply(times, \(t) {
        curbcut:::explore_text_region_val_df(
          var = var,
          region = reg,
          select_id = NA,
          data = dat,
          scale = sc,
          col = "left",
          lang = NULL,
          time = t
        )
      })

  }, lapply(vars, `[[`, "var_left"), data, time, scale, region,
  SIMPLIFY = FALSE, USE.NAMES = FALSE)

  first_val <- sapply(values, \(x) x[[1]]$val)
  last_val <- sapply(values, \(x) x[[2]]$val)
  change <- mapply(\(x, y) (y - x) / x, x = first_val, y = last_val)
  first_date <- sapply(values, \(x) names(x)[[1]])
  last_date <- sapply(values, \(x) names(x)[[2]])

  # Produce warnings for infinite values
  inf <- is.infinite(change)
  if (sum(inf) > 0) {
    inf_var <- paste(unique(var_left[inf]), collapse = ", ")
    warning(paste0("Infinite dyk_uni_change values: ", inf_var),
            call. = FALSE)
  }

  # Initial region mention (one per lang)
  region_start <- lapply(langs, \(lang) {
    out <- mapply(\(x, y) curbcut:::explore_context(
      region = x, select_id = NA, scale = y,
      switch_DA = FALSE, lang = lang), x = region, y = scale, SIMPLIFY = FALSE,
      USE.NAMES = FALSE)
    out <- sapply(out, \(x) gsub(" $", "", x$p_start))
    curbcut::s_sentence(out)
  })

  # Variable explanation (one per lang)
  var_exp <- svm$variables$explanation[sapply(
    var_left, \(x) which(x == svm$variables$var_code),
    USE.NAMES = FALSE)]
  var_exp <- lapply(langs, \(x) sapply(var_exp, curbcut::cc_t, lang = x))

  # Convert values
  first_val <- dyk_val_convert(var_left, vars, region, scale, first_val, svm, langs)
  last_val <- dyk_val_convert(var_left, vars, region, scale, last_val, svm, langs)
  change_txt <- sapply(
    change, \(x) curbcut:::convert_unit.pct(x = abs(x), decimal = 1))

  # Increasing/decreasing (one per lang)
  inc_dec <- lapply(langs, \(x) {

    if (x == "en") {
      dplyr::case_when(
        change >= 0.2 ~ paste0("increased rapidly (", change_txt, ")"),
        change >= 0.05 ~ paste0("increased ", change_txt),
        change < -0.2 ~ paste0("decreased rapidly (", change_txt, ")"),
        change < -0.05 ~ paste0("decreased ", change_txt),
        change < 0.05 ~ "barely changed")
    } else if (x == "fr") dplyr::case_when(
      change >= 0.2 ~ paste0("a rapidement augmenté (", change_txt, ")"),
      change >= 0.05 ~ paste0("a augmenté de ", change_txt),
      change < -0.2 ~ paste0("a rapidement diminué (", change_txt, ")"),
      change < -0.05 ~ paste0("a diminué de ", change_txt),
      change < 0.05 ~ "a à peine changé")

  })

  # Date range (one per lang)
  current_year <- substr(Sys.Date(), 1, 4)
  date_spread <- as.numeric(last_date) - as.numeric(first_date)
  within_five <- as.numeric(current_year) - as.numeric(last_date) < 6
  date_ref <- lapply(langs, \(x) {

    if (x == "en") {
      ifelse(within_five,
             paste0("over the last ", date_spread, " years"),
             paste0("over ", date_spread, " years"))

    } else if (x == "fr") ifelse(
      within_five, paste0("au cours des ", date_spread, " dernières annés"),
      paste0("au cours des ", date_spread, " annés"))
  })

  # Get weighting factor
  change_val <- dplyr::case_when(
    abs(change) < 0.05 ~ 1 - abs(change) ^ 0.4,
    abs(change) >= 0.05 ~ (abs(change) * 0.5) ^ 0.4)

  # Assemble output
  change_vec <- dyk_assemble_change(region_start, var_exp, inc_dec, date_ref,
                                    first_val, first_date, last_val, last_date,
                                    langs = langs)

  change_df <-
    do.call(tibble::tibble, set_names(change_vec, paste0("dyk_text_", langs)))

  dplyr::mutate(change_df, dyk_weight = change_val)

}


dyk_assemble_change <- function(region_start, var_exp, inc_dec, date_ref,
                                first_val, first_date, last_val, last_date,
                                langs) {

  lapply(langs, \(lang) {

    if (lang == "en") {
      first_piece <- sprintf("%s, %s %s %s.", region_start[[1]], var_exp[[1]],
                             inc_dec[[1]], date_ref[[1]])
      second_piece <- mapply(\(one, two, fv, fd, lv, ld) {
        if (one == two) "" else {
          sprintf(" It was %s in %s and %s in %s.", fv, fd, lv, ld)
        }
      }, first_val[[1]], last_val[[1]], first_val[[1]], first_date, last_val[[1]],
      last_date)
      sprintf("%s%s", first_piece, second_piece)

    } else if (lang == "fr") {
      first_piece <- sprintf("%s, %s %s %s.", region_start[[2]], var_exp[[2]],
                             inc_dec[[2]], date_ref[[2]])
      second_piece <- mapply(\(one, two, fv, fd, lv, ld) {
        if (one == two) "" else {
          sprintf(" C'était %s en %s et %s en %s.", fv, fd, lv, ld)
        }
      }, first_val[[1]], last_val[[1]], first_val[[1]], first_date, last_val[[1]],
      last_date)
      sprintf("%s%s", first_piece, second_piece)

    }})

}


#' Generate Bivariate Comparison DYKs
#'
#' This function creates "Did you know" text strings highlighting bivariate
#' comparisons from a combination of variables, region, scale and date.
#' The output is a data frame containing a `highest` and `lowest` column.
#'
#' @param var_left <character> A string representing the name of the first
#' variable for which the DYK should be calculated.
#' @param var_right <character> A string representing the name of the second
#' variable for which the DYK should be calculated.
#' @param region <`character`> A string representing the name of the region
#' for which the DYK should be calculated.
#' @param scale <`character`> A string representing the name of the scale
#' for which the DYK should be calculated.
#' @param date <`character`> A string representing the name of the date
#' for which the DYK should be calculated.
#' @param regions_dictionary <`data.frame`> A data frame with information about
#' which feature of every scale fits in which region.
#' @param svm <`list`> A list, usually `scales_variables_modules`, containing
#' the scales, modules, and variables tables.
#'
#' @return A data frame with two columns (`compare_text` and `compare_val`),
#' which contain a character vector of DYK outputs and a numeric vector of the
#' values respectively.
#' @export
dyk_uni_compare <- function(var_left, var_right, region, scale, date, svm,
                            regions_dictionary, langs) {

  # Get class
  vars <- mapply(curbcut::vars_build,
                 var_left = var_left,
                 var_right = var_right,
                 scale = scale,
                 time = date,
                 MoreArgs = list(
                   check_choropleth = FALSE,
                   variables = svm$variables),
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)
  vars <- lapply(vars, `[[`, "vars")

  # Initial region mention (one per lang)
  region_start <- lapply(langs, \(lang) {
    out <- mapply(\(x, y) curbcut:::explore_context(
      region = x, select_id = NA, scale = y,
      switch_DA = FALSE, lang = lang), x = region, y = scale, SIMPLIFY = FALSE,
      USE.NAMES = FALSE)
    out <- sapply(out, \(x) gsub(" $", "", x$p_start))
    curbcut::s_sentence(out)
  })

  # Scale name (one per lang)
  scale_name <- scales_dictionary$plur[sapply(
    scale, \(x) which(scales_dictionary$scale == x), USE.NAMES = FALSE)]
  scale_name <- sapply(langs, \(x) sapply(scale_name, curbcut::cc_t, lang = x),
                       simplify = FALSE, USE.NAMES = TRUE)
  if ("fr" %in% langs) scale_name$fr <- paste0("les ", scale_name$fr)

  # Values
  val_1 <- mapply(\(vl, reg, sc, dt) {
    ids <- regions_dictionary$scales[regions_dictionary$region == reg][[1]][[sc]]
    tb <- qs::qread(sprintf("data/%s/%s.qs", sc, vl))
    tb <- tb[tb$ID %in% ids, ]
    tb[[paste(vl, dt, sep = "_")]]
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  val_2 <- mapply(\(vr, reg, sc, dt) {
    ids <- regions_dictionary$scales[regions_dictionary$region == reg][[1]][[sc]]
    tb <- qs::qread(sprintf("data/%s/%s.qs", sc, vr))
    tb <- tb[tb$ID %in% ids, ]
    out <- tb[[paste(vr, dt, sep = "_")]]
    if (is.null(out)) {
      time_regex <- attr(tb, "schema")$time
      bv <- attr(tb, "breaks_var")
      bv <- gsub(time_regex, "", bv)

      tb[[paste(bv, dt, sep = "_")]]
    } else out
  }, var_right, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = FALSE)

  # Correlation
  corr <- mapply(cor, x = val_1, y = val_2,
                 MoreArgs = list(use = "na.or.complete"), USE.NAMES = FALSE)
  positive <- corr > 0

  # Frequency qualifier (one per lang)
  freq <- lapply(langs, \(x) {

    if (x == "en") {
      dplyr::case_when(
        abs(corr) > 0.7 ~ "almost always",
        abs(corr) > 0.3 ~ "usually",
        abs(corr) > 0.1 ~ "often",
        .default = "sometimes")
    } else if (x == "fr") dplyr::case_when(
      abs(corr) > 0.7 ~ "presque toujours",
      abs(corr) > 0.3 ~ "généralement",
      abs(corr) > 0.1 ~ "souvent",
      .default = "parfois")

  })

  # High/low (one per lang)
  high_low_1 <- lapply(langs, \(lang) {
    sapply(vars, \(x) curbcut::explore_text_bivar_adjective(
      x$var_left, TRUE, TRUE, FALSE, lang))})
  high_low_2 <- lapply(langs, \(lang) {
    mapply(\(x, y) {
      if (is.na(y)) return(NA_character_)
      curbcut::explore_text_bivar_adjective(
        x$var_right, FALSE, y, FALSE, lang)}, vars, positive)})

  # Variable explanations (one per lang)
  var_exp_1 <- svm$variables$explanation_nodet[sapply(
    var_left, \(x) which(x == svm$variables$var_code),
    USE.NAMES = FALSE)]
  var_exp_1 <- lapply(langs, \(x) sapply(var_exp_1, curbcut::cc_t, lang = x))
  var_exp_2 <- svm$variables$explanation_nodet[sapply(
    var_right, \(x) which(x == svm$variables$var_code),
    USE.NAMES = FALSE)]
  var_exp_2 <- lapply(langs, \(x) sapply(var_exp_2, curbcut::cc_t, lang = x))

  # Get max date for each variable
  max_date <-
    tibble::tibble(var_left = var_left, var_right = var_right, date = date) |>
    dplyr::summarize(max_date = max(date), .by = c(var_left, var_right))

  # Use max_date to decide on date handling (one per lang)
  extra_date <- lapply(langs, \(lang) {
    mapply(\(x, y, z) ifelse(
      z == max_date$max_date[max_date$var_left == x & max_date$var_right == y],
      "", paste0(if (lang == "en") " in " else if (lang == "fr") " en ", z)),
      var_left, var_right, date, USE.NAMES = FALSE)})
  have_had <- lapply(langs, \(lang) {
    mapply(\(x, y, z) ifelse(z == max_date$max_date[
      max_date$var_left == x & max_date$var_right == y],
      if (lang == "en") "have" else if (lang == "fr") "ont",
      if (lang == "en") "had" else if (lang == "fr") "avaient"),
      var_left, var_right, date, USE.NAMES = FALSE)})

  # Assemble output
  compare_vec <- dyk_assemble_compare(
    region_start, extra_date, scale_name, high_low_1, var_exp_1, freq, have_had,
    high_low_2, var_exp_2, langs)

  compare_df <-
    do.call(tibble::tibble, set_names(compare_vec, paste0("dyk_text_", langs)))

  dplyr::mutate(compare_df, dyk_weight = abs(corr) ^ 2)

}


dyk_assemble_compare <- function(region_start, extra_date, scale_name,
                                 high_low_1, var_exp_1, freq, have_had,
                                 high_low_2, var_exp_2, langs) {

  lapply(langs, \(lang) {

    if (lang == "en") {
      paste0(region_start[[1]], extra_date[[1]], ", ", scale_name[[1]],
             " with ", high_low_1[[1]], " ", var_exp_1[[1]], " ", freq[[1]],
             " ", have_had[[1]], " ", high_low_2[[1]], " ", var_exp_2[[1]], ".")

    } else if (lang == "fr") {
      paste0(region_start[[2]], extra_date[[2]], ", ", scale_name[[2]],
             " avec ", high_low_1[[2]], " ", var_exp_1[[2]], " ", have_had[[2]],
             " ", freq[[2]], " ", high_low_2[[2]], " ", var_exp_2[[2]], ".")

    }})

}



# Bivariate ---------------------------------------------------------------

#' Generate Outlier DYKs
#'
#' This function creates "Did you know" text strings highlighting outlier
#' values from a combination of left and right variables, region, scale and
#' date. The output is a data frame containing an `outlier` column.
#'
#' @param var_left <character> A string representing the name of the variable
#' for which the DYK should be calculated.
#' @param region <`character`> A string representing the name of the region
#' for which the DYK should be calculated.
#' @param scale <`character`> A string representing the name of the scale
#' for which the DYK should be calculated.
#' @param date <`character`> A string representing the name of the date
#' for which the DYK should be calculated.
#' @param svm <`list`> A list, usually `scales_variables_modules`, containing
#' the scales, modules, and variables tables.
#'
#' @return A data frame with two columns (`highest` and `lowest`), each of
#' which contains a character vector of DYK outputs.
#' @export
dyk_bivar_outlier <- function(var_left, var_right, region, scale, date,
                              svm) {

  # Get class
  vars <- mapply(curbcut::vars_build,
                 var_left = paste(var_left, date, sep = "_"),
                 var_right = paste(var_right, date, sep = "_"),
                 df = paste(region, scale, sep = "_"),
                 MoreArgs = list(
                   check_choropleth = FALSE,
                   variables = svm$variables),
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)

  # Initial region mention
  region_start <- mapply(\(x, y) curbcut:::explore_context(
    region = x, select_id = NA, df = paste(x, y, sep = "_"), switch_DA = FALSE),
    x = region, y = scale, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  region_start <- sapply(region_start, \(x) curbcut::s_sentence(x$p_start))

  # Scale name
  scale_name <- scales_dictionary$plur[sapply(
    scale, \(x) which(scales_dictionary$scale == x), USE.NAMES = FALSE)]

  # Values
  val_1 <- mapply(\(var_left, region, scale, date) {
    tb <- qs::qread(sprintf("data/%s/%s.qs", scale, var_left))
    tb[[paste(var_left, date, sep = "_")]]
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  val_2 <- mapply(\(var_right, region, scale, date) {
    tb <- qs::qread(sprintf("data/%s/%s.qs", scale, var_left))
    tb[[paste(var_right, date, sep = "_")]]
  }, var_right, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = FALSE)

  # Correlation
  corr <- mapply(cor, x = val_1, y = val_2,
                 MoreArgs = list(use = "na.or.complete"), USE.NAMES = FALSE)
  positive <- corr > 0

  # Frequency qualifier
  freq <- dplyr::case_when(
    abs(corr) > 0.7 ~ "almost always",
    abs(corr) > 0.3 ~ "usually",
    abs(corr) > 0.1 ~ "often",
    .default = "sometimes"
  )

  # High/low
  high_low_1 <- sapply(vars, \(x) curbcut::explore_text_bivar_adjective(
    x$var_left, TRUE, TRUE, FALSE, "en"))
  high_low_2 <- mapply(\(x, y) {
    if (is.na(y)) return(NA_character_)
    curbcut::explore_text_bivar_adjective(
      x$var_right, FALSE, y, FALSE, "en")}, vars, positive)

  # Variable explanations
  var_exp_1 <- svm$variables$explanation_nodet[sapply(
    var_left, \(x) which(x == svm$variables$var_code),
    USE.NAMES = FALSE)]
  var_exp_2 <- svm$variables$explanation_nodet[sapply(
    var_right, \(x) which(x == svm$variables$var_code),
    USE.NAMES = FALSE)]

  # Get max date for each variable
  max_date <-
    tibble::tibble(var_left = var_left, var_right = var_right, date = date) |>
    dplyr::summarize(max_date = max(date), .by = c(var_left, var_right))

  # Use max_date to decide on date handling
  extra_date <- mapply(\(x, y, z) ifelse(
    z == max_date$max_date[max_date$var_left == x & max_date$var_right == y],
    "", paste0(" in ", z)), var_left, var_right, date, USE.NAMES = FALSE)
  have_had <- mapply(\(x, y, z) ifelse(z == max_date$max_date[
    max_date$var_left == x & max_date$var_right == y], "have", "had"),
    var_left, var_right, date, USE.NAMES = FALSE)
  is_was <- mapply(\(x, y, z) ifelse(z == max_date$max_date[
    max_date$var_left == x & max_date$var_right == y], "is", "was"),
    var_left, var_right, date, USE.NAMES = FALSE)

  # Make a model to determine outliers
  md <- mapply(\(x, y) {
    lm(y ~ x, data = data.frame(scale(tibble(x = x, y = y))))
  }, val_1, val_2, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  which_out <- lapply(md, \(x) which(abs(x$residuals) > 1, useNames = FALSE))

  # Get outlier place names
  outlier_place <- mapply(\(x, region, scale) {
    svm$scales[[scale]]$name[x]
  }, which_out, region, scale, USE.NAMES = FALSE, SIMPLIFY = FALSE)

  # Get outlier values
  out_val_1 <- mapply(\(x, val_1) val_1[x], which_out, val_1, USE.NAMES = FALSE,
                      SIMPLIFY = FALSE)
  out_val_2 <- mapply(\(x, val_2) val_2[x], which_out, val_2, USE.NAMES = FALSE,
                      SIMPLIFY = FALSE)

  # High/low
  high_low_1 <- sapply(vars, \(x) curbcut::explore_text_bivar_adjective(
    x$var_left, TRUE, TRUE, FALSE, "en"))
  high_low_2 <- mapply(\(x, y) {
    if (is.na(y)) return(NA_character_)
    curbcut::explore_text_bivar_adjective(
      x$var_right, FALSE, y, FALSE, "en")}, vars, positive)

  # Convert values
  out_val_1 <- mapply(curbcut::convert_unit,
                      var = lapply(vars, \(x) x$var_left),
                      x = out_val_1,
                      MoreArgs = list(decimal = 1, compact = FALSE),
                      SIMPLIFY = FALSE, USE.NAMES = FALSE)
  out_val_2 <- mapply(curbcut::convert_unit,
                      var = lapply(vars, \(x) x$var_right),
                      x = out_val_2,
                      MoreArgs = list(decimal = 1, compact = FALSE),
                      SIMPLIFY = FALSE, USE.NAMES = FALSE)

  mapply(\(region_start, extra_date, scale_name, high_low_1, var_exp_1, freq,
           have_had, high_low_2, var_exp_2, outlier_place, is_was, out_val_1,
           out_val_2) {

    paste0(
      region_start, extra_date, ", although ", scale_name, " with ", high_low_1,
      " ", var_exp_1, " ", freq, " ", have_had, " ", high_low_2, " ", var_exp_2,
      ", ", outlier_place, " ", is_was, " ", "<outlier>", " with ",
      "<high_low_out_1>", " ", var_exp_1, " (", out_val_1, ") and ",
      "<high_low_out_2>", " ", var_exp_2, " (", out_val_2, ").")


  }, region_start, extra_date, scale_name, high_low_1, var_exp_1, freq,
  have_had, high_low_2, var_exp_2, outlier_place, is_was, out_val_1, out_val_2,
  SIMPLIFY = FALSE, USE.NAMES = FALSE)





  # Assemble output
  # outlier_vec <-
  paste0(
    region_start, extra_date, ", although ", scale_name, " with ", high_low_1,
    " ", var_exp_1, " ", freq, " ", have_had, " ", high_low_2, " ", var_exp_2,
    ", ", "<outlier_place>", " ", is_was, " ", "<outlier>", " with ",
    "<high_low_out_1>", " ", var_exp_1, " (", "<out_val_1>", ") and ",
    "<high_low_out_2>", " ", var_exp_2, " (", "<out_val_2>", ").")

  tibble::tibble(outlier_text = outlier_vec, outlier_val = abs(sd_dif))


  scale(tibble(x = val_1[[1]], y = val_2[[1]]))

  which.max(abs(md$residuals))

  val_1
  val_2

  tibble(x = abs(md[[1]]$residuals)) |>
    ggplot(aes(x)) + geom_histogram()

  val_1[[1]][51]
  val_2[[1]][51]

  # Highest value
  highest_val <- mapply(\(var_left, region, scale, date) {
    tb <- qs::qread(sprintf("data/%s/%s.qs", scale, var_left))
    max(tb[[paste(var_left, date, sep = "_")]], na.rm = TRUE)
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Highest name
  highest_name <- mapply(\(var_left, region, scale, date) {
    tb <- qs::qread(sprintf("data/%s/%s.qs", scale, var_left))
    tb$name[which.max(tb[[paste(var_left, date, sep = "_")]])]
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Second highest value
  second_highest_val <- mapply(\(var_left, region, scale, date) {
    tb <- qs::qread(sprintf("data/%s/%s.qs", scale, var_left))
    max_val <- which.max(tb[[paste(var_left, date, sep = "_")]])
    max(tb[[paste(var_left, date, sep = "_")]][-max_val], na.rm = TRUE)
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Second highest name
  second_highest_name <- mapply(\(var_left, region, scale, date) {
    tb <- qs::qread(sprintf("data/%s/%s.qs", scale, var_left))
    val_vec <- tb[[paste(var_left, date, sep = "_")]]
    # Remove top value
    val_vec[which.max(val_vec)] <- -Inf
    tb$name[which.max(val_vec)]
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Lowest value
  lowest_val <- mapply(\(var_left, region, scale, date) {
    tb <- qs::qread(sprintf("data/%s/%s.qs", scale, var_left))
    min(tb[[paste(var_left, date, sep = "_")]], na.rm = TRUE)
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Lowest name
  lowest_name <- mapply(\(var_left, region, scale, date) {
    tb <- qs::qread(sprintf("data/%s/%s.qs", scale, var_left))
    tb$name[which.min(tb[[paste(var_left, date, sep = "_")]])]
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Second lowest value
  second_lowest_val <- mapply(\(var_left, region, scale, date) {
    tb <- qs::qread(sprintf("data/%s/%s.qs", scale, var_left))
    min_val <- which.min(tb[[paste(var_left, date, sep = "_")]])
    min(tb[[paste(var_left, date, sep = "_")]][-min_val], na.rm = TRUE)
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Second lowest name
  second_lowest_name <- mapply(\(var_left, region, scale, date) {
    tb <- qs::qread(sprintf("data/%s/%s.qs", scale, var_left))
    val_vec <- tb[[paste(var_left, date, sep = "_")]]
    # Remove bottom value
    val_vec[which.min(val_vec)] <- Inf
    tb$name[which.min(val_vec)]
  }, var_left, region, scale, date, USE.NAMES = FALSE, SIMPLIFY = TRUE)

  # Variable explanation
  var_exp <- svm$variables$explanation_nodet[sapply(
    var_left, \(x) which(x == svm$variables$var_code),
    USE.NAMES = FALSE)]

  # Get max date for each variable
  max_date <-
    tibble::tibble(var_left = var_left, date = date) |>
    dplyr::summarize(max_date = max(date), .by = var_left)

  # Use max_date to decide on date handling
  extra_date <- mapply(\(x, y) ifelse(
    y == max_date$max_date[max_date$var_left == x], "", paste0(" in ", y)),
    var_left, date, USE.NAMES = FALSE)
  is_was <- mapply(\(x, y) ifelse(
    y == max_date$max_date[max_date$var_left == x], "is", "was"),
    var_left, date, USE.NAMES = FALSE)

  # Convert values
  highest_val <- mapply(curbcut::convert_unit,
                        var = lapply(vars, \(x) x$var_left),
                        x = highest_val,
                        MoreArgs = list(decimal = 1, compact = FALSE),
                        SIMPLIFY = TRUE, USE.NAMES = FALSE)

  second_highest_val <- mapply(curbcut::convert_unit,
                               var = lapply(vars, \(x) x$var_left),
                               x = second_highest_val,
                               MoreArgs = list(decimal = 1, compact = FALSE),
                               SIMPLIFY = TRUE, USE.NAMES = FALSE)

  lowest_val <- mapply(curbcut::convert_unit,
                       var = lapply(vars, \(x) x$var_left),
                       x = lowest_val,
                       MoreArgs = list(decimal = 1, compact = FALSE),
                       SIMPLIFY = TRUE, USE.NAMES = FALSE)

  second_lowest_val <- mapply(curbcut::convert_unit,
                              var = lapply(vars, \(x) x$var_left),
                              x = second_lowest_val,
                              MoreArgs = list(decimal = 1, compact = FALSE),
                              SIMPLIFY = TRUE, USE.NAMES = FALSE)

  # Assemble output
  highest <- paste0(
    region_start, extra_date, ", ", highest_name, " ", is_was, " the ",
    scale_name, " with the highest ", var_exp, " (", highest_val,
    "), followed by ", second_highest_name, " (", second_highest_val, ").")

  lowest <- paste0(
    region_start, extra_date, ", ", lowest_name, " ", is_was, " the ",
    scale_name, " with the lowest ", var_exp, " (", lowest_val,
    "), followed by ", second_lowest_name, " (", second_lowest_val, ").")

  tibble::tibble(highest = highest, lowest = lowest)

}
