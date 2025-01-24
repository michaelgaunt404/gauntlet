#' Print basic to console basic tryCatch template
#'
#' @return None (prints to console).
#' @importFrom stringr str_glue
#'
#' @export
#' @examples
#' \dontrun{
#' remindMe_gitIgnore()
#'
#' }
remindMe_gitIgnore = function(){
  message(stringr::str_glue("{gauntlet::strg_make_space_2()}Printing common R .gitignore file{gauntlet::strg_make_space_2(last = F)}"))

  cat('
# History files
.Rhistory
.Rapp.history

# Session Data files
.RData
.RDataTmp

# User-specific files
.Ruserdata

# Example code in package build process
*-Ex.R

# Output files from R CMD build
/*.tar.gz

# Output files from R CMD check
/*.Rcheck/

# RStudio files
.Rproj.user/

# produced vignettes
vignettes/*.html
vignettes/*.pdf

# OAuth2 token, see https://github.com/hadley/httr/releases/tag/v0.3
.httr-oauth

# knitr and R markdown default cache directories
*_cache/
/cache/

# Temporary files created by R markdown
*.utf8.md
*.knit.md

# R Environment Variables
.Renviron

# pkgdown site
docs/

# translation temp files
po/*~

# RStudio Connect folder
rsconnect/
_targets/

# analysis folder html files
analysis/*.html

# Ignore everything in the data folder
data/*

# Ignore specific data file types
*.qs
*.rds
*.parquet
*.pgkg
*.gpkg
*.zip
*.shp
*.csv
*.json
'
  )
}


