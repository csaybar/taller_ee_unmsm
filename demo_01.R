# DEMO 01: Use multiple account to solve the same problem
library(mapedit)
library(future)
library(raster)
library(rgee)
library(sf)
source("utils.R")

ee_users()

# 1. Create a geometry
roi_area <- mapedit::editMap()$geometry

# 2. Select users
ee_users <- c("csaybar", "aybar1994")

# 3.
ndvi_raster <- download_ndvi_batch(
  ee_user = ee_users,
  area = roi_area,
  scale = 2500,
  bucket = "rgee_dev"
)

Reduce(function(x,y) mosaic(x, y, fun=mean), ndvi_raster) %>% plot()
