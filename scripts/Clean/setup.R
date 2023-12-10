#############################################
## The following loads the needed packages ##
#############################################

# load the required packages and install them if they are not.
packages <- c(
  "here", # for the project's organization
  "tidyverse", # for wrangling
  "ggrepel", "gghighlight", "patchwork", "maps", "scales", # for plotting
  "stringr",
  "tidyr",
  "lubridate",
  "gridExtra",
  "readr",
  "readxl",
  "ggplot2",
  "countrycode", 
  "stringi",
  "forecast",
  "tibble",
  "reshape2",
  "corrplot",
  "stargazer",
  "tinytex",
  "cowplot",
  "sf",
  "dplyr",
  "rnaturalearth",
  "knitr",
  "kableExtra",
  "DT",
  "FactoMineR",
  "factoextra",
  "dplyr",
  "visdat",
  "huxtable",
  "plm",
  "plotly",
  "stargazer",
  "patchwork",
  "e1071",
  "car",
  "naniar",
  "ggridges",
  "shiny"
)

for (pkg in packages) {
  if (!pkg %in% installed.packages()) {
    install.packages(pkg)}}

for (pkg in packages) {
  library(pkg, character.only = TRUE)}


######################################################
## The following sets a few option for nice reports ##
######################################################

# general options
options(
  digits = 3,
  str = strOptions(strict.width = "cut"),
  width = 69,
  tibble.width = 69,
  cli.unicode = FALSE
)

# ggplot options
theme_set(theme_light())

# knitr options
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  fig.retina = 0.8, # figures are either vectors or 300 dpi diagrams
  dpi = 300,
  # out.width = "70%",
  out.width = "100%",
  fig.align = "center",
  fig.width = 6,
  # fig.asp = 0.618,
  fig.show = "hold",
  message = FALSE,
  echo = FALSE
)

# cleaning of the environment
rm(packages, pkg)

   