var_left <- curbcut::picker_server(
  id = id,
  r = r,
  var_list = curbcut::dropdown_make(vars = `__var_left__`,
                                    compare = FALSE),
  time = time)
