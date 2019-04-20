################################################################
# Final Project
################################################################
library(pROC)
library(dplyr)
library(e1071)
library(MASS)
library(readxl)

fires <- read_xls("P3_kitchen_area_air_quality_03_20_2019.xlsx")
