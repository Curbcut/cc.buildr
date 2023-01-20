var_left <- select_var_server(
  id = id,
  r = r,
  var_list = reactive(make_dropdown(only_vars = `__var_left__`,
                                    only = NULL)),
  time = time)
