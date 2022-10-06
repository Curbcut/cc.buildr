## LINKS TO GET A PROVINCE'S ADDRESSES DATABASE ################################

prov_links <- rvest::read_html("https://www.statcan.gc.ca/en/lode/databases/oda") |>
  rvest::html_elements("ul") |>
  rvest::html_elements("li") |>
  rvest::html_elements("a") |>
  stringr::str_subset(".zip")

addresses_db_links <-
  lapply(prov_links, \(x) {
    link <-
      rvest::read_html(x) |>
      rvest::html_element("a") |>
      rvest::html_attr("href")
    txt <- rvest::read_html(x) |>
      rvest::html_text()
    prov_code <-
      rvest::read_html(x) |>
      rvest::html_element("a") |>
      rvest::html_attr("href") |>
      stringr::str_extract("(?<=ODA_).*(?=_v)")
    data.frame(province_code = prov_code, province = txt, link = link)
  }) |> (\(x) do.call(rbind, x))()

usethis::use_data(addresses_db_links, overwrite = TRUE)
