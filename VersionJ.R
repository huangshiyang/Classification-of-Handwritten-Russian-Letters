install.packages('reticulate')
#vois ce site https://cran.r-project.org/web/packages/reticulate/vignettes/package.html
#celui la aussi https://www.r-bloggers.com/run-python-from-r/
install.packages('Rcpp')
library(reticulate)
use_python("/usr/local/bin/python")
use_virtualenv("myenv")

#difflib est un package de python
difflib <- import("difflib")
difflib$ndiff(foo, bar)

