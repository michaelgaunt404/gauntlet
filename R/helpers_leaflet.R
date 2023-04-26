#' Make a quick leaflet plot.
#'
#' This function creates a leaflet map with various options for displaying spatial data.
#'
#' @param data A spatial object.
#' @param markers A boolean value indicating whether to display marker symbols on the map. The default value is false.
#' @param lines A boolean value indicating whether to display lines on the map. The default value is false.
#' @param polys A boolean value indicating whether to display polygons on the map. The default value is false.
#'
#' @return A leaflet map.
#' @export
#'
#' @examples
#' mapview::breweries %>%
#' quick_leaflet(markers = TRUE)
#'
#' @importFrom leaflet addTiles addCircleMarkers addPolylines addPolygons
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom maps::map
quick_leaflet = function(data, markers = F, lines = F, polys = F){
  data %>%
    leaflet::leaflet() %>%
    addTiles() %>%
    { if (markers) (.) %>% leaflet::addCircleMarkers(color = "black", weight = 2, fillColor = "blue") else .} %>%
    { if (lines) (.) %>% leaflet::addPolylines() else .} %>%
    { if (polys) (.) %>% leaflet::addPolygons(color = "black", weight = 2, fillColor = "blue") else .}
}


#' Default tiles for a leaflet map.
#'
#' @param object a blank leaflet object
#'
#' @return a leaflet object
#' @export
#'
#' @examples
#' leaflet() %>%
#'   leaflet_default_tiles()
leaflet_default_tiles = function(object){
  object %>%
    addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$Esri, group = "Esri") %>%
    addProviderTiles(providers$CartoDB, group = "CartoDB")
}


#' Commonly used Leaflet tiles.
#'
#' @return Creates leaflet tiles for leaflet map.
#' @export
#'
#' @examples
#' #none
leaflet_default_tiles_index = function(){
  c("OSM (default)", "Esri", "CartoDB")
}


#' Create a popup table with formatted content
#'
#' This function takes a spatial object and creates a popup table with formatted content.
#'
#' @param data A spatial object.
#'
#' @return A leaflet popup table.
#'
#' @importFrom leafpop popupTable
#' @importFrom janitor clean_names
#' @importFrom sf st_set_geometry
#' @importFrom dplyr %>%
popup_tbl_pretty = function(data){
  data %>%
    janitor::clean_names() %>%
    st_set_geometry(NULL) %>%
    leafpop::popupTable()
}

















