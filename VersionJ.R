#vois ce site https://cran.r-project.org/web/packages/reticulate/vignettes/package.html
#celui la aussi https://www.r-bloggers.com/run-python-from-r/
library(reticulate)
use_python(Sys.which('python'))
use_virtualenv("myenv")

#difflib est un package de python
difflib <- import("difflib")
foo<-c("a","b","c")
bar<-c("a","b","d")
difflib$ndiff(foo, bar)
