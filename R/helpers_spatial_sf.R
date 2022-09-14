#' quick_buffer
#'
#' Easily buffer a spatial object. Primarily used to help convert between lat/long that cannot be buffered to CRS that can be and then back to lat/long.
#'
#' @param geo_data an sf object (generally in lat/long format)
#' @param to a numeric CRS
#' @param with a  numeric CRS which allows for buffering
#' @param radius a numeric value of the buffer radius
#'
#' @return an sf mulitpolygon object
#' @export
#'
#' @examples
#' #work in progress
quick_buffer = function(geo_data, to = 4326, with = 2781, radius = NA){
  geo_data %>%
    st_transform(crs = with) %>%
    st_buffer(dist = radius) %>%
    st_transform(crs = to)
}

#' self_buffer
#'
#' Creates two buffer polygons around a line or polygon that act together to make a negative space buffer - image a donut shape around a line. It can be used to help geo-reference/identify unknown links to a known network. It creates a poly buffer AROUND a line (roadway) and can be used to filter things EXCEPT the line
#'
#' @param object an sf object (generally in lat/long format)
#' @param rad_bg a numeric value
#' @param rad_sm a numeric value
#' @param nm
#'
#' @return a list containing two sf buffer polygons
#' @export
#'
#' @examples
#' #work in progress
self_buffer = function(object, rad_bg = NA, rad_sm = NA, nm = NULL){

  on_bg = object %>%
    quick_buffer(radius = rad_bg) %>%
    st_union() %>%
    st_as_sf() %>%
    mutate(buffer = nm) %>%
    suppressWarnings() %>%
    suppressMessages()

  on_sm = object %>%
    quick_buffer(radius = rad_sm) %>%
    st_union() %>%
    st_as_sf() %>%
    suppressWarnings() %>%
    suppressMessages

  ob_diff = st_difference(on_bg, on_sm) %>%
    mutate(rm_flag = 1) %>%
    suppressWarnings() %>%
    suppressMessages

  list(on_bg, ob_diff)
}

#' link_locator
#'
#' A function that references/codes/identifies referenced network links using a known spatial network.
#'
#' @param search_list list of known links to use to reference unknown links with - used to map over reference network with known links
#' @param geo_data_known an sf object depicting network of known links
#' @param geo_data_known_col name of column that contains names contained in search_list
#' @param geo_data_unknown an sf object depicting network of unknown links that you want identified and located
#' @param geo_data_unknown_id_col name of primary key column in unknown link sf object
#' @param large_buffer a numeric value - should be sufficiently large
#' @param small_buffer a numeric value - should be small to ensure only link of interest is geolocated
#'
#' @return an sf object of referenced links
#' @export
#'
#' @examples
#' #work in progress
link_locator = function(search_list
                        ,geo_data_known, geo_data_known_col
                        ,geo_data_unknown, geo_data_unknown_id_col
                        ,large_buffer = 10000, small_buffer = 30){
  search_list %>%
    map(~{
      temp_buffer = geo_data_known %>%
        filter({{geo_data_known_col}} == .x) %>%
        self_buffer(rad_bg = large_buffer, rad_sm = small_buffer, nm = ".x")

      geo_data_unknown %>%
        select({{geo_data_unknown_id_col}}) %>%
        st_filter(temp_buffer[[1]]) %>%
        st_join(temp_buffer[[2]]) %>%
        filter(is.na(buffer)) %>%
        mutate(link_id = .x %>%
                 as.factor())
    }) %>%
    reduce(rbind)
}

show_identified_links = function(){
  link_ids = identified_links$link_id

  binpal = colorFactor(
    rev(viridis::plasma(
      length(
        levels( link_ids ))))
    ,link_ids)

  leaflet(identified_links #%>% st_jitter(factor = 0.00005)
  ) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addPolylines(color = ~binpal(link_id)
                 ,opacity = .5
                 ,label = ~str_glue("{link_id}")
                 ,labelOptions = labelOptions(noHide = F, textOnly = F)
                 ,popup = popupTable(identified_links)) %>%
    addLegend(
      position = "bottomright",
      title = "Link_id",
      pal = binpal,
      opacity = 0.7,
      values = ~link_id)
}


#' Spatially aggregate spatial data into counts via honeycombs or squares.
#'
#' @param data an SF object
#' @param map_back_crs an integer indicating which crs for the aggregated counts to be returned in - default is `4326`
#' @param honey_crs an integer indicating which crs to use in aggregation, aggregation cannot be perfomed useing lat/long, must be ft or m - default is `2285`
#' @param honey_dim a integer (in honey_crs units) indicating size of honeycomb or square
#' @param honey_type a boolean indicating what type of honeycomb to use - honeycomb (`F`) or square (`T`)
#'
#' @return an SF object (multipolygon) with counts per honeycomb unit
#' @export
#'
#' @examples
#'mapview::breweries %>%
#'  make_honeycomb_counts(honey_type = T) %>%
#' filter(count > 0) %>%
#'  mapview::mapview(zcol = "count")
make_honeycomb_counts = function(data, map_back_crs = 4326
                                 ,honey_crs = 2285, honey_dim = 10000, honey_type = F){

  area_honeycomb_grid =
    data %>%
    st_transform(honey_crs) %>%
    st_make_grid(c(dim, dim), what = "polygons", square = honey_type) %>%
    st_sf() %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(grid_id = row.names(.)) %>%
    st_transform(map_back_crs)

  temp = st_join(area_honeycomb_grid, data %>%
                   mutate(flag = 1)) %>%
    mutate(flag = replace_na(flag, 0)) %>%
    st_drop_geometry() %>%
    group_by(grid_id) %>%
    summarise(count = sum(flag), .groups = "drop")


  area_honeycomb_grid_object = area_honeycomb_grid %>%
    merge(temp, by = "grid_id", all = T) %>%
    arrange(desc(count))

  return(area_honeycomb_grid_object)
}


st_true_midpoint = function(sf_object){
  #gets the true midpoint along a curved line
  temp = sf_object %>%
    mutate(merge_id = row_number())

  #new CRS, cast to linestring, selects cols
  sf_object_linestring = temp %>%
    st_transform(2781) %>%
    st_cast("LINESTRING") %>%
    mutate(linestring_id = row_number()) %>%
    select(merge_id, linestring_id)

  #make coords df, pull middle point
  coords_extract = sf_object_linestring %>%
    st_line_sample(n = 5) %>%
    st_transform(4326) %>%
    st_coordinates() %>%
    data.frame() %>%
    merge(sf_object_linestring %>%
            st_drop_geometry(),
          by.x = "L1", by.y = "linestring_id") %>%
    group_by(merge_id) %>%
    mutate(n = ceiling(n()/2),
           index = row_number()) %>%
    filter(n == index) %>%
    ungroup() %>%
    select(X, Y, merge_id)

  #convert df to spatial
  temp %>%
    st_drop_geometry() %>%
    merge(coords_extract,
          by = "merge_id") %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326)
}

st_extract_coords = function(spatial_object){
  #only should be used for points for now
  spatial_object %>%
    mutate(lon = st_coordinates(geometry)[,1]
           ,lat = st_coordinates(geometry)[,2])
}

