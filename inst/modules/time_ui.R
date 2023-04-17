curbcut::slider_UI(id = NS(id, id), slider_id = "slu", min = `__min__year`, max = `__max__year`, step = `__step__`)
curbcut::slider_UI(id = NS(id, id), slider_id = "slb", min = `__min__year`, max = `__max__year`, step = `__step__`, label = curbcut::cc_t("Select two years"), value = c(`__mid__year`, `__max__year`))
curbcut::checkbox_UI(id = NS(id, id), label = curbcut::cc_t("Compare dates"), value = FALSE)
