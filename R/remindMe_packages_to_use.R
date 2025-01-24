#' Print basic packages that to use template
#'
#' @return None (prints to console).
#'
#' @export
#' @examples
#' \dontrun{
#' remindMe_packages_to_use()
#'
#' }
remindMe_packages_to_use = function(){
  message(stringr::str_glue("{gauntlet::strg_make_space_2()}Printing basic packages{gauntlet::strg_make_space_2(last = F)}"))

  cat('
#basic_munging/basic_tasks
# library(tidyverse)
# library(gauntlet)
# library(data.table)
# library(targets)
# library(furrr)
# library(qs)
# library(here)

#html/plotting
# library(crosstalk)
# library(plotly)
# library(reactable)
# library(reactablefmtr)
# library(gauntletReactable)
# library(grDevices)

#spatial
# library(sf)
# library(SpatialKDE)
# library(sfhotspot)
# library(mapview)
# library(leaflet)
# library(leafem)
# library(gauntletMap)
'
  )
}


