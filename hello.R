# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(devtools)
library(roxygen2)
library(testthat)
library(knitr)
library(usethis)
library(rmarkdown)

use_mit_license() # pick a licence

devtools::document()
devtools::test()
devtools::check()
devtools::load_all()

# Vignettes
usethis::use_vignette("autoLIMR_vignette")

# Package description
usethis::use_tidy_description()

# start adding functions
usethis::use_version()

use_git()
use_github()

# Dependencies
usethis::use_package("readxl", "Imports") # Dependencies
usethis::use_package("openxlsx", "Imports") # Dependencies

usethis::use_package("knitr", "Suggests") # Dependencies
usethis::use_package("rmarkdown", "Suggests") # Dependencies
usethis::use_package("testthat", "Suggests") # Dependencies

# Add functions and tests
use_r("error_print")
use_test("error_print")

use_r("demo_data")
use_test("demo_data")

use_r("read_all_sheets")
use_test("read_all_sheets")

use_r("NLNode_mat")
use_test("NLNode_mat")

use_r("sci_notation_off")
use_test("sci_notation_off")

use_r("net_data_tidy")
use_test("net_data_tidy")

use_r("adj_mat_tidy")
use_test("adj_mat_tidy")

use_r("net_data_node_list")
use_r("net_data_external_list")
use_r("net_data_node")
use_r("variable_def")
use_r("qvar")
use_r("search_cols")
use_r("pvar")
use_r("pvar_wo_ex")
use_r("pvar_w_ex")
use_r("uvar")
use_r("aevar")
use_r("pp_true")
use_r("uvar_wo_ex")
use_r("uvar_w_ex")
use_r("net_data_resp_flows")
use_r("net_data_inex_flows")
use_r("adj_mat_flows")
use_r("net_data_ineq")
use_r("adj_mat_ineq")
use_r("meta1")
use_r("meta2")
use_r("merge_sections")
use_r("matrix_def")


use_r("autoGen")
use_test("autoGen")
