
# -----------------------------------------------------------------------------
#
# key steps in pkg development
#
# -----------------------------------------------------------------------------


usethis::use_data_raw()

## follow instructions in the file DATASET.R

## at the same time write script for running the analysis

## before running the analysis script do the following:

# 1) specify the packages needed
usethis::use_package("PACKAGE NAME", type = "Imports")

# or for a development version
usethis::use_dev_package("PACKAGE NAME", type = "Imports")

# 2) write minimal functions documentation
# i.e. after each roxygen edit hit
devtools::document()

## run the analysis script and debug - have fun!

## create a new git repo
# 1) git init
# 2) git add R/
# 3) git commit -m 'initial commit'
# 4) git remote add origin https://github.com/lorecatta/DENVfoiMap.git
# 5) git push -u origin master

usethis::use_vignette("how-to-do-stuff", "How to do stuff")
