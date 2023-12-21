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
  "grid",
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
  "shiny",
  "leaps",
  "shinydashboard",
  "broom",
  "viridis",
  "paletteer"
)

for (pkg in packages) {
  if (!pkg %in% installed.packages()) {
    install.packages(pkg)}}

for (pkg in packages) {
  library(pkg, character.only = TRUE)}

######################################################
## The following sets a few colors for nice reports ##
######################################################

# Create a color palette for the percentage of missing values
MPer_0_10 <- viridis(1, begin = 0.5)
MPer_10_20 <- "#e5e419"
MPer_20_30 <- "#fba537"
MPer_30_100 <- "#e34e64"

# Create a fix color for graphs that need only one color
Fix_color <- viridis(1, # specifying to the viridis function that we want 1 color
                     option = "D", # we use the color from the viridis palette
                     begin = 0.5) # we select the middle color

# Creation of a discrete color palette for the categorical variables
Discrete_colors <- scale_color_paletteer_d("Polychrome::dark")


colors <- c("red", "blue", "green", "orange", "purple", "pink","lightblue",
            "gray", "cyan", "magenta", "yellow","darkgreen", "darkblue",
            "darkred", "darkgrey", "darkcyan")

colors_pal <- scale_color_manual(values = colors)

# Creation of a continuous color palette for the percentage of missing values
MPer_pal <- colorRampPalette(c(MPer_0_10,
                               MPer_10_20,
                               MPer_20_30,
                               MPer_30_100))


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

   