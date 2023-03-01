slider_uni <- curbcut::slider_server(id = id, slider_id = "slu")
slider_bi <- curbcut::slider_server(id = id, slider_id = "slb")
slider_switch <- curbcut::checkbox_server(id = id, r = r, label = shiny::reactive("Compare dates"))
time <- shiny::reactive(if (slider_switch()) slider_bi() else slider_uni())

# Enable or disable first and second slider
shiny::observeEvent(slider_switch(), {
  shinyjs::toggle(NS(id, "ccslider_slu"), condition = !slider_switch())
  shinyjs::toggle(NS(id, "ccslider_slb"), condition = slider_switch())
})
