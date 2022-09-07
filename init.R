# R script to run author supplied code, typically used to install additional R packages
# contains placeholders which are inserted by the compile script
# NOTE: this script is executed in the chroot context; check paths!

r <- getOption('repos')
r['CRAN'] <- 'http://cloud.r-project.org'
options(repos=r)

# ======================================================================

# packages go here

install.packages('remotes')

install.packages(c('jsonlite', 'curl', 'triebeard', 'Rcpp', 'magrittr', 'rlang', 'later', 'R6', 'fastmap', 'base64enc', 'digest',
                   'lazyeval', 'htmltools', 'utf8', 'cli', 'glue', 'vctrs', 'pkgconfig', 'pillar', 'lifecycle', 'fansi', 'purrr',
                   'ellipsis', 'tidyselect', 'tibble', 'generics', 'cpp11', 'dplyr', 'yaml', 'sys', 'askpass', 'openssl', 'mime', 'colorspace', 'viridisLite',
                   'RColorBrewer', 'munsell', 'labeling', 'farver', 'withr', 'scales', 'isoband', 'gtable', 'webutils', 'xml2', 'brotli', 'urltools', 'stringi',
                   'assertthat', 'parallelly', 'listenv', 'globals', 'promises', 'data.table', 'crosstalk', 'tidyr', 'htmlwidgets', 'httr', 'ggplot2', 'reqres',
                   'uuid', 'crayon', 'future', 'httpuv', 'plotly', 'routr', 'fiery','tidyverse','bit', 'ps', 'sass', 'cachem', 'memoise', 'rappdirs', 'rematch',
                   'bit64', 'prettyunits', 'processx', 'evaluate', 'highr', 'xfun', 'bslib', 'jquerylib', 'tinytex', 'backports', 'blob', 'DBI', 'gargle',
                   'cellranger', 'ids', 'rematch2', 'clipr', 'vroom', 'tzdb', 'progress', 'callr', 'fs', 'knitr', 'rmarkdown', 'selectr', 'broom', 'dbplyr',
                   'dtplyr', 'forcats', 'googledrive', 'googlesheets4', 'haven', 'hms', 'lubridate', 'modelr', 'readr', 'readxl', 'reprex', 'rstudioapi', 'rvest',
                   'stringr'),type="binary")
remotes::install_github('plotly/dashR')
# remotes::install_github('cran/dash', upgrade=TRUE)
# install.packages("dash")
# remotes::install_github('plotly/dash-daq', upgrade = TRUE)
remotes::install_github('facultyai/dash-bootstrap-components@r-release')


# install.packages('dashCoreComponents')
# install.packages('dashHtmlComponents')
# install.packages('tidyverse')
# install.packages('plotly')
# install.packages('gapminder')
# install.packages('base64enc')
