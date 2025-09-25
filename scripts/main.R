##############
#Packages installation
##############

if(!require(renv)){install.packages("renv")}
renv::init(force = TRUE)
renv::activate()

# Install and load required packages with explanations

# 'sf' (Simple Features) is used for handling and analyzing spatial data.
library(sf)
# 'dplyr' is a popular package for data manipulation and transformation using a grammar of data manipulation.
library(dplyr)
# 'ggplot2' is a widely used visualization package for creating complex and customizable graphics.
library(ggplot2)
# 'raster' is used for reading, writing, and analyzing raster (gridded) spatial data.
library(raster)
# 'lattice' is a visualization package for creating multi-panel plots and conditioning plots.
library(lattice)
# 'RColorBrewer' provides color palettes for visualizations, often used in maps and graphs.
library(RColorBrewer)
# 'rasterVis' extends the 'raster' package by providing advanced visualization methods for raster data.
library(rasterVis)
# 'vegan' is used for ecological data analysis, especially for multivariate community analysis.
library(vegan)
# 'hilldiv' is a package for diversity analysis, particularly for calculating Hill numbers.
library(hilldiv)
# 'iNEXT' is used for rarefaction and extrapolation analysis of species diversity.
library(iNEXT)
# 'tidyr' is a data manipulation package used to reshape and tidy data into a consistent format.
library(tidyr)
# 'worrms' provides access to the World Register of Marine Species (WoRMS) API for taxonomy data.
library(worrms)
# 'purrr' is used for functional programming in R, particularly for working with lists.
library(purrr)
# 'writexl' allows writing data frames to Excel files without dependencies on Java.
library(writexl)
# 'inborutils' is a set of utility functions from the INBO (Research Institute for Nature and Forest).
library(inbo/inborutils)
# 'readxl' is used to read Excel files (`.xls` and `.xlsx`) into R.
library(readxl)
# 'corrplot' is used for visualizing correlation matrices in an easy-to-read format.
library(corrplot)

source("./functions/functions.R")

options(scipen=9999)


