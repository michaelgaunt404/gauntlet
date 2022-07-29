#' Make a quick leaflet plot.
#'
#' @param data a spatial object
#' @param markers boolean to indicate if markers should be used - default value is `false`
#' @param lines boolean to indicate if lines should be used - default value is `false`
#' @param polys boolean to indicate if polygons should be used - default value is `false`
#'
#' @return a leaflet map
#' @export
#'
#' @examples
#'mapview::breweries %>%
#'  quick_leaflet(markers = T)
quick_leaflet = function(data, markers = F, lines = F, polys = F){
  data %>%
    leaflet() %>%
    addTiles() %>%
    { if (markers) (.) %>% addCircleMarkers(color = "black", weight = 2, fillColor = "blue") else .} %>%
    { if (lines) (.) %>% addPolylines() else .} %>%
    { if (polys) (.) %>% addPolygons(color = "black", weight = 2, fillColor = "blue") else .}
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




















