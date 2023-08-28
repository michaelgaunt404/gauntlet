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

#' Quickly create a buffer around spatial objects
#'
#' This function creates a buffer around a spatial object with a specified radius.
#' By default, the buffer is created using a projected coordinate reference system (CRS)
#' and then transformed back to a geographic CRS.
#'
#' @param geo_data a spatial object
#' @param to the CRS to transform to, default is WGS84 (EPSG code 4326)
#' @param with the CRS to transform from, default is UTM zone 55S (EPSG code 2781)
#' @param radius the radius of the buffer in the unit of the input CRS, default is NA
#' @param endCapStyle the style of the end caps, defaults to "ROUND"
#' @param joinStyle the style of the line joints, defaults to "ROUND"
#'
#' @return a spatial object representing the buffer
#'
#' @examples
#' # Load required libraries
#' library(sf)
#' curved_line = st_sfc(
#'   st_linestring(
#'     rbind(
#'       c(-122.68, 45.52)
#'       ,c(-122.63, 45.55)
#'       ,c(-122.58, 45.57)
#'       ,c(-122.55, 45.54)
#'     ))
#'   ,crs = 4326) %>%
#'   st_as_sf()
#'
#' mapview::mapview(curved_line) +
#'   (quick_buffer(curved_line, radius = 1000) %>%
#'      mapview::mapview())
quick_buffer = function(geo_data, to = 4326, with = 2781, radius = NA
                       ,endCapStyle = "ROUND"
                       ,joinStyle = "ROUND"){
  geo_data %>%
    st_transform(crs = with) %>%
    st_buffer(dist = radius
              ,endCapStyle = endCapStyle
              ,joinStyle = joinStyle) %>%
    st_transform(crs = to)
}

#' Make a doughnut polygon around a spatial object.
#'
#' Use an object and make a buffer around itself. Works well if you want to isolate an object but can only do it spatially.
#'
#' @param object A spatial object of class 'sf', 'sfc' or 'sfg'.
#' @param rad_bg Numeric. The radius for the background buffer.
#' @param rad_sm Numeric. The radius for the smaller buffer.
#' @param nm A string indicating the name of the buffer. Defaults to NULL.
#' @return A list with two elements: a background buffer and a difference buffer.
#' @examples
#' curved_line = st_sfc(
#'   st_linestring(
#'     rbind(
#'       c(-122.68, 45.52)
#'       ,c(-122.63, 45.55)
#'       ,c(-122.58, 45.57)
#'       ,c(-122.55, 45.54)
#'     ))
#'   ,crs = 4326) %>%
#'   st_as_sf()
#'
#' curved_line %>%
#'   self_buffer(rad_bg = 1000, rad_sm = 50, nm = "arbitary_name") %>%
#'   .[[2]] %>%
#'   mapview::mapview()
#' @importFrom sf st_as_sf st_difference st_union
#' @export
#'
#' @keywords spatial
#' @seealso \code{\link{st_buffer}}, \code{\link{st_difference}}, \code{\link{st_as_sf}}, \code{\link{st_union}}
self_buffer = function(spatial_object, rad_bg = NA, rad_sm = NA, nm = NULL){

  on_bg = spatial_object %>%
    quick_buffer(radius = rad_bg) %>%
    st_union() %>%
    st_as_sf() %>%
    mutate(buffer = nm) %>%
    suppressWarnings() %>%
    suppressMessages()

  on_sm = spatial_object %>%
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


#' Make a bounding box of an sf object
#'
#' @param base_geo an sf object - can be a singular ploygon or multiple polygons.
#'
#' @return a dataframe of coordinates describing extent of supplied sf object
#' @export
#'
#' @examples
#' #none
st_make_bounding_box = function(base_geo){
  stopifnot("CRS must be 4326, please convert..." = (st_crs(base_geo)$input == "EPSG:4326"))

  base_geo %>%
    st_bbox()
}

#' Get the true midpoint along a curved line
#'
#' This function gets the true midpoint along a curved line represented by a spatial object.
#' It transforms the object to a new CRS, casts it to a linestring, selects the relevant columns,
#' extracts the coordinates, finds the middle point, and converts it to a spatial object with the
#' original CRS.
#'
#' @param sf_object A spatial object representing a curved line
#' @param crs An integer input indicating which CRS to use when extracting the midpoint - default is 2781
#' @return A spatial object representing the true midpoint along the curved line
#' @importFrom sf st_transform st_cast st_line_sample st_coordinates st_drop_geometry st_as_sf
#' @export
#' @examples
#' library(sf)
#'
#' curved_line = st_sfc(
#'   st_linestring(
#'     rbind(
#'       c(-122.68, 45.52)
#'       ,c(-122.63, 45.55)
#'       ,c(-122.58, 45.57)
#'       ,c(-122.55, 45.54)
#'     ))
#'   ,crs = 4326) %>%
#'   st_as_sf()
#'
#' mapview::mapview(curved_line) +
#'   mapview::mapview(st_true_midpoint(curved_line))
#'
#' @importFrom sf st_coordinates
#' @export
#'
#' @rdname st_extract_coords
#' @aliases extract_coords
#' @keywords spatial
#' @seealso \code{\link{st_coordinates}}, \code{\link{st_set_geometry}}
st_true_midpoint = function(sf_object, crs = 2781){
  temp = sf_object %>%
    mutate(merge_id = row_number())

  sf_object_linestring = temp %>%
    st_transform(crs) %>%
    st_cast("LINESTRING") %>%
    mutate(linestring_id = row_number()) %>%
    select(merge_id, linestring_id)

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

  temp %>%
    st_drop_geometry() %>%
    merge(coords_extract,
          by = "merge_id") %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326)
}


#' Extract the longitude and latitude coordinates of a spatial object
#'
#' This function extracts the longitude and latitude coordinates of a spatial object
#' representing points.
#'
#' @param spatial_object A spatial object representing points
#' @return A data frame with two columns, "lon" for longitude and "lat" for latitude
#' @importFrom sf st_coordinates
#' @export
#' @examples
#'mapview::breweries %>%
#'  st_extract_coords()
#'
st_extract_coords = function(spatial_object){
  spatial_object %>%
    mutate(lon = st_coordinates(geometry)[,1]
           ,lat = st_coordinates(geometry)[,2])
}

#' Convert LineString to Sampled Points
#'
#' Converts a LineString spatial object into a set of sampled points along the LineString.
#'
#' @param sf_object A spatial object of class 'sf' representing LineString geometry.
#' @param samp_dist The sampling distance, specifying the distance between consecutive sampled points along the LineString. The default value is 100.
#' @param crs The Coordinate Reference System (CRS) for the LineString geometry. If not specified, the function assumes the CRS of the input sf_object.
#'
#' @return A spatial object of class 'sf' representing the sampled points along the LineString.
#'
#' @details The `st_line_sample_to_points` function takes a LineString spatial object and samples points along the LineString at a specified distance (`samp_dist`). The function first converts the input `sf_object` to LineString geometry by using the `st_cast` function and assigns a unique ID (`linestring_id`) to each LineString. It also creates a merge ID (`merge_id`) that corresponds to each LineString.
#'
#' Next, the function samples points along each LineString using the `st_line_sample` function. The density of sampling is determined by the inverse of the `samp_dist` parameter. The resulting sampled points are then transformed to EPSG code 4326 (WGS84) and converted to a spatial object of class 'sf'.
#'
#' Additional attributes, such as the `shape_id` from the original `sf_object_linestring`, are added to the sampled points using the `bind_cols` function. The resulting spatial object is then converted to Point geometry using the `st_cast` function and assigns an index (`index`) to each point. Finally, the function uses the `gauntlet::st_extract_coords` function to extract the longitude and latitude coordinates of the points, renaming them as `samp_lon` and `samp_lat`.
#'
#' This function simplifies the process of sampling points along LineString geometries, which can be useful for various spatial analysis and visualization tasks.
#'
#' @import sf
#' @import gauntlet
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr bind_cols
#' @importFrom dplyr rename
#' @importFrom dplyr row_number
#' @importFrom dplyr st_drop_geometry
#'
#' @examples
#' # Convert LineString to sampled points
#' sampled_points <- st_line_sample_to_points(line_string_data, samp_dist = 100)
#'
#' @export
st_line_sample_to_points = function(sf_object, samp_dist = 100, crs){
  sf_object_linestring = sf_object %>%
    mutate(merge_id = row_number()) %>%
    st_transform(crs) %>%
    st_cast("LINESTRING") %>%
    mutate(linestring_id = row_number()) %>%
    select(merge_id, linestring_id, shape_id)

  sf_object_points = sf_object_linestring %>%
    st_line_sample(density = 1/samp_dist) %>%
    st_transform(4326) %>%
    st_as_sf() %>%
    bind_cols(sf_object_linestring %>%
                st_drop_geometry() %>%
                select(shape_id)) %>%
    st_cast("POINT") %>%
    mutate(index = row_number()) %>%
    rename(geometry = x) %>%
    gauntlet::st_extract_coords() %>%
    rename(samp_lon = lon, samp_lat = lat)
}
