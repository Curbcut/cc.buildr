    slider_uni <- slider_server(id = id, slider_id = "slu")
    slider_bi <- slider_server(id = id, slider_id = "slb")
    slider_switch <- checkbox_server(id = id)
    time <- reactive(if (slider_switch()) slider_bi() else slider_uni())

    # Enable or disable first and second slider
    observeEvent(slider_switch(), {
      toggle(NS(id, "slu"), condition = !slider_switch())
      toggle(NS(id, "slb"), condition = slider_switch())
    })
