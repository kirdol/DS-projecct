#############################################
## The following loads the needed packages ##
#############################################

# load the required packages and install them if they are not.
packages <- c(
  "here", # Path management
  "tidyverse", # Data manipulation
  "ggrepel", # Labeling
  "gghighlight", # Highlighting
  "patchwork", # Plot layout management
  "maps", # Geographical map data
  "scales", # Graph scaling tools
  "stringr", # Strings manipulation
  "tidyr", # Data tidying
  "lubridate", # Date/time manipulation
  "gridExtra", # Grid graphics enhancements
  "grid", # Grids
  "readr", # Data Reading
  "readxl", # Excel file reading
  "ggplot2", # Visualization (plot)
  "countrycode", # Country code conversion
  "stringi", # Strings manipulation
  "forecast", # Forecasting
  "tibble", # Data frames
  "reshape2", # Reshaping
  "corrplot", # Correlations
  "stargazer", # Formatting
  "tinytex", # LaTeX
  "cowplot", # Plotting
  "sf", # Simple features for spatial data
  "dplyr", # Manipulation
  "rnaturalearth", # Maps
  "knitr", # Reporting
  "kableExtra", # Tables
  "DT", # Tables
  "FactoMineR", # Multivariate analysis
  "factoextra", # FactoMineR visualization
  "dplyr", # Manipulation
  "visdat", # Visualization
  "huxtable", # Tables
  "plm", # Models
  "plotly", # Interactive
  "stargazer", # Formatting
  "patchwork", # Layouts
  "e1071", # Functions
  "car", # Regression
  "naniar", # Missing data
  "ggridges", # Plots
  "shiny", # Apps
  "leaps", # Regression subset selection
  "shinydashboard", # Dashboards
  "broom", # Tidy model outputs
  "viridis", # Colors palettes
  "paletteer", # Colors palettes
  "RColorBrewer" # Colors palettes
)

# Function that install the packages if not already installed on your computer
for (pkg in packages) {
  if (!pkg %in% installed.packages()) {
    install.packages(pkg)}}

# load the packages
for (pkg in packages) {
  library(pkg, character.only = TRUE)}

#############################
## Setting some parameters ##
#############################

# Threshols
threshold_heatmap <- 0.75
p_value_threshold <- 0.05

######################################################
## The following sets a few colors for nice reports ##
######################################################

# Create a color palette for the percentage of missing values
MPer_0_10 <- viridis(1, begin = 0.5)
MPer_10_20 <- "#e5e419"
MPer_20_30 <- "#fba537"
MPer_30_100 <- "#e34e64"

# Create a fix color for graphs that need only one color
Fix_color <- viridis(1,
                     option = "D",
                     begin = 0.5)

# # Creation of a discrete color palette for the categorical variables
# Discrete_colors <- scale_color_paletteer_d("Polychrome::dark")

# Creation of a discrete color palette for the categorical variables
colors <- c("#20908c","#e5e419","#e34e64","lightblue","#8dce3e","darkgreen"
            ,"darkred","darkblue","red","blue","#fba537","purple","pink",
            "lightgray","magenta","cyan","darkgrey")

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

   