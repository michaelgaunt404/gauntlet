#' Calculate quantiles for groups of pre-aggregated metrics.
#'
#' Use this function to calculate qunatiles for data that has already been aggregated and collapsed into counts. Avoids the need to expand data then compute quantiles.
#'
#' @param df a dataframe of data
#' @param value string of column that qunatiles will be calculated for
#' @param weight string of column that will be used to calculate quantiles with
#' @param quantiles vector of quantiles that should be returned - default is `c(0, .25, .5, .75, 1)` quantiles
#'
#' @return a dataframe with qunatiles - in wide format
#' @import purrr
#' @importFrom DescTools Quantile
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'
#' \dontrun{
#' df = data.frame(group = c(rep(1, 4)
#'                           ,rep(2, 3))
#'                 ,days = c(round(runif(4, 0, 20), 0)
#'                           ,round(runif(3, 0, 20), 0))
#'                 ,count = c(round(runif(4, 0, 5), 0)
#'                            ,round(runif(3, 0, 5), 0)))
#'
#' df %>%
#'   group_by(group) %>%
#'   nest() %>%
#'   mutate(qauntiles = map(data, ~group_wtd_quantiles(.x, value = "days", weight = "count"))) %>%
#'   unnest(cols = c(qauntiles)) %>%
#'   select(!data)
#'
#' #alternative calucaltion by expansion
#' rep(df$days[1:4], df$count[1:4]) %>%  summary()
#' }
group_wtd_quantiles = function(df, value, weight = "count", quantiles = c(0, .25, .5, .75, 1)){
  purrr::map(quantiles, ~DescTools::Quantile(df[[value]], df[[weight]], .x, na.rm = T)) %>%
    purrr::reduce(bind_cols) %>%
    purrr::set_names(
      purrr::map_chr(quantiles, ~paste0(value, "_", .x*100, "%")))
}
