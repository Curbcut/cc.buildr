### __id__ PAGE

# GLOBAL ------------------------------------------------------------------

`__id___default_region` <- unlist(modules$regions[modules$id == "__id__"])[1]
`__id___mzp` <-
  eval(parse(text = paste0("map_zoom_levels_", `__id___default_region`)))
default_region <- modules$regions[modules$id == "__id__"][[1]][1]

# UI ----------------------------------------------------------------------

`__id___UI` <- function(id) {

  shiny::tagList(
    # Sidebar
    curbcut::sidebar_UI(
      id = shiny::NS(id, id),
      `__widgets_UI__`,
      curbcut::warnuser_UI(shiny::NS(id, id)),
      bottom = shiny::tagList(
        curbcut::legend_UI(shiny::NS(id, id)),
        curbcut::zoom_UI(shiny::NS(id, id), `__id___mzp`)
      )),

    # Map
    curbcut::map_UI(NS(id, id)),

    # Tutorial
    curbcut::tutorial_UI(id = shiny::NS(id, id)),

    # Change view (Map/Data/Place explorer)
    curbcut::panel_view_UI(id = NS(id, id)),

    # Right panel
    curbcut::right_panel(
      id = id,
      curbcut::compare_UI(
        id = NS(id, id),
        var_list = curbcut::dropdown_make(vars = " ", compare = TRUE)),
      curbcut::explore_UI(NS(id, id)),
      curbcut::dyk_UI(NS(id, id))
    )
  )
}


# Server ------------------------------------------------------------------

`__id___server` <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    # Initial reactives
    rv_zoom_string <- reactiveVal(
      curbcut::zoom_get_string(zoom = map_zoom,
                               zoom_levels = `__id___mzp`,
                               region = default_region))

    # Zoom and POI reactives when the view state of the map changes.
    observeEvent(map_viewstate(), {
      r[[id]]$zoom(curbcut::zoom_get(zoom = map_viewstate()$zoom))
      r[[id]]$poi(curbcut::update_poi(id = id, poi = r[[id]]$poi(),
                                      map_viewstate = map_viewstate()))
    })

    # Map zoom levels change depending on r$region()
    zoom_levels <-
      reactive(curbcut::zoom_get_levels(id = id, region = r$region()))

    # Zoom string reactive
    observe({
      rv_zoom_string({
        curbcut::zoom_get_string(
          zoom = r[[id]]$zoom(),
          zoom_levels = zoom_levels()$zoom_levels,
          region = zoom_levels()$region)
      })
    })

    # Update selected ID
    curbcut::update_select_id(id = id, r = r, data = data)

    # Choose tileset
    tile <- curbcut::zoom_server(
      id = id,
      r = r,
      zoom_string = rv_zoom_string,
      zoom_levels = zoom_levels,
      no_autozoom = shiny::reactive(`__no_autozoom__`)
    )

    # Get df
    observeEvent({
      tile()
      rv_zoom_string()}, {
        r[[id]]$df(curbcut::update_df(tile = tile(),
                                      zoom_string = rv_zoom_string()))
      })

    # Time
    time <- reactive(`__time__`)

    # Left variable
    var_left <- reactive(paste(`__var_left__`, time(), sep = "_"))

    # Right variable / compare panel
    var_right <- curbcut::compare_server(
      id = id,
      r = r,
      var_list = curbcut::dropdown_make(
        vars = `__var_right__`,
        compare = TRUE
      ),
      time = time
    )

    # Update the `r[[id]]$vars` reactive
    curbcut::update_vars(id = id, r = r, var_left = var_left,
                         var_right = var_right)

    # Sidebar
    curbcut::sidebar_server(id = id, r = r)

    # Data
    data <- reactive(curbcut::data_get(
      vars = vars(),
      df = r[[id]]$df()
    ))

    # Data for tile coloring
    data_colours <- reactive(curbcut::data_get_colours(
      vars = vars(),
      region = zoom_levels()$region,
      zoom_levels = zoom_levels()$zoom_levels
    ))

    # Warn user
    curbcut::warnuser_server(
      id = id,
      r = r,
      vars = vars,
      time = time,
      data = data)

    # Tutorial
    curbcut::tutorial_server(
      id = id,
      r = r
    )

    # Legend
    curbcut::legend_server(
      id = id,
      r = r,
      vars,
      data = data,
      df = r[[id]]$df
    )

    # Did-you-know panel
    curbcut::dyk_server(
      id = id,
      r = r,
      vars = vars,
      poi = r[[id]]$poi,
      df = r[[id]]$df
    )

    # Update map in response to variable changes or zooming
    map_viewstate <- curbcut::map_server(
      id = id,
      tile = tile,
      data_colours = data_colours,
      select_id = r[[id]]$select_id,
      zoom_levels = reactive(zoom_levels()$zoom_levels),
      zoom = r[[id]]$zoom,
      coords = r[[id]]$coords)

    # Update map labels
    curbcut::label_server(
      id = id,
      tile = tile,
      zoom = r[[id]]$zoom,
      zoom_levels = reactive(zoom_levels()$zoom_levels),
      region = reactive(zoom_levels()$region))

    # Explore panel
    curbcut::explore_server(
      id = id,
      r = r,
      data = data,
      region = reactive(zoom_levels()$region),
      vars = vars,
      df = r[[id]]$df,
      select_id = r[[id]]$select_id)

    # Bookmarking
    curbcut::bookmark_server(
      id = id,
      r = r,
      select_id = r[[id]]$select_id,
      map_viewstate = map_viewstate)

    # Change view
    curbcut::panel_view_server(
      id = id,
      r = r,
      vars = vars,
      data = data,
      zoom_levels = reactive(zoom_levels()$zoom_levels))

  })
}
