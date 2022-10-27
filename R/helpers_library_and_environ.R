#' Load many packages.
#'
#' Function loads R libraries listed in string vector. Compatable with targets formatting style.
#'
#' @param pkg vector of string libraries.
#'
#' @return message detailing successful or unsuccessful loaded packages
#' @export
#'
#' @examples
#' packages = c("dplyr", "ggplot")
#'
#' package_load(packages)
package_load =  function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}




