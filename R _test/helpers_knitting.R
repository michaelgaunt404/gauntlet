#' Automate knitting of RMD FlexDashboards to HTML.
#'
#' Use this function to knit RMD FlexDashboards to HTMLs and write them to new location and with a different name.
#'
#' @param dashboard_name string of RMD to knit - do not include `.RMD` extension.
#' @param dashboard_folder string of folder name where RMD is located - default is `analysis`, do not include trailing `(/)`.
#' @param doc_name string of knitted RMD name - default is `markdown_name`.
#' @param write_to_folder string of folder name where RMD is written to - default is `public`.
#' @param overwrite boolean (`TRUE`/`FALSE`) indicating if file should be rewritten or if date suffix should be added to file name - default is `FALSE` which adds date suffix.
#'
#' @return a knitted HTML document
#' @export
#'
#' @examples
#' #none
knit_dashboard =  function(dashboard_name = "dashboard", dashboard_folder = "dashboard",
                           doc_name = dashboard_name, overwrite = F, write_to_folder = "public"){
  dashboard_path_rmd = stringr::str_glue("{dashboard_folder}/{dashboard_name}.Rmd")
  dashboard_path_html = stringr::str_glue("{dashboard_folder}/{dashboard_name}.html")

  if (overwrite) {
    file_rename = stringr::str_glue("{write_to_folder}/{doc_name}.html")
  } else {
    file_rename = stringr::str_glue("{write_to_folder}/{doc_name}_{Sys.Date()}.html")
  }

  rmarkdown::render(dashboard_path_rmd)
  file.rename(dashboard_path_html,
              file_rename)

}

#' Automate knitting of RMD to HTML.
#'
#' Use this function to knit RMDs to HTMLs and write them to new location and with a different name.
#'
#' @param markdown_name string of RMD to knit - do not include `.RMD` extension.
#' @param markdown_folder string of folder name where RMD is located - default is `analysis`.
#' @param doc_name string of knitted RMD name - default is `markdown_name`.
#' @param write_to_folder string of folder name where RMD is written to - default is `public`.
#' @param overwrite boolean (`TRUE`/`FALSE`) indicating if file should be rewritten or if date suffix should be added to file name - default is `FALSE` which adds date suffix.
#' @param clean boolean (`TRUE`/`FALSE`) - keep as default `TRUE`
#'
#' @return a knitted HTML document
#' @export
#'
#' @examples
#' #none
knit_markdown =  function(markdown_name, markdown_folder = "analysis",
                          doc_name = markdown_name, overwrite = F, clean = TRUE,
                          write_to_folder = "public"){
  markdown_path_rmd = stringr::str_glue("{markdown_folder}/{markdown_name}.Rmd") %>%
    here()
  markdown_path_html = stringr::str_glue("{markdown_folder}/{markdown_name}.html") %>%
    here()

  print(markdown_path_rmd)
  print(markdown_path_html)

  if (overwrite) {
    file_rename = stringr::str_glue("{write_to_folder}/{doc_name}.html") %>%
      here()
  } else {
    file_rename = stringr::str_glue("{write_to_folder}/{doc_name}_{Sys.Date()}.html") %>%
      here()
  }

  rmarkdown::render(markdown_path_rmd,
                    clean = clean)

  file.rename(markdown_path_html,
              file_rename)

}
