# Time variable
time <- eventReactive(var_left_1(), {
  tb <- tables_in_sql[[r[[id]]$df()]]
  # Get the available table
  avl_table <- tb[grepl(var_left_1(), tb)]
  # Flag if there's more than one time
  if (length(avl_table) != 1)
    stop(paste0("There are 0 or more than 1 table corresponding to `",
                var_left_1(), "`. The assumption is that there is only one ",
                "possible time()."))
  # Return the time
  return(gsub(paste0(var_left_1(), "_"), "", avl_table))
})

# Left variable server
var_left_1 <- auto_vars_server(id = id,
                               r = r,
                               var_list = make_dropdown(
                                 only = NULL,
                                 only_vars = `__var_left__`),
                               auto_disable = FALSE)
var_left <- reactive(paste(var_left_1(), time(), sep = "_"))
