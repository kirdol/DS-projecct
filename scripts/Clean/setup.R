#############################################
## The following loads the needed packages ##
#############################################

# load the required packages and install them if they are not.
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

packages <- c(
  "here", # for the project's organization
  "tidyverse", # for wrangling
  "ggrepel", "gghighlight", "patchwork", "maps", "scales", # for plotting
  "stringr",
  "dplyr",
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
  "sf",
  "rnaturalearth"
)

for (i in packages) {
  install_if_missing(i)
}

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
  out.width = "70%",
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,
  fig.show = "hold",
  message = FALSE,
  echo = FALSE
)

# cleaning of the environment
rm(packages,i,install_if_missing)

   