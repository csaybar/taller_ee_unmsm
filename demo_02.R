library(rgee)

ee_Initialize()

# 1. Considerations
# -------------------------------

# 1.1 Use ee_utils_pyfunc to avoid erros when map over a ee$List
ee_double_fn <- function(x) ee$Number(x)$add(x)
ee_SimpleList <- ee$List$sequence(0, 12)
ee_NewList <- ee_SimpleList$map(ee_double_fn) # error
ee_NewList <- ee_SimpleList$map(ee_utils_pyfunc(ee_double_fn))
ee_NewList$getInfo()

# 1.2 Dates

## Load an Image Sentinel2 level-1C
ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2016-01-01", "2016-01-31")$
  first()

## From GEE to R
ee_s2$get("system:time_start")$getInfo() # bad
ee_s2$get("system:time_start") %>% eedate_to_rdate() # good!

## From R to GEE
rdate_to_eedate("1981-01-01")
rdate_to_eedate(315532800000) # float number

## Get the date of a ee$Image
ee_get_date_img(ee_s2)

## Get the date of a ee$ImageCollection
library(sf)
nc <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
  st_transform(4326) %>%
  sf_as_ee()

ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2016-01-01", "2016-01-31")$
  filterBounds(nc)

ee_get_date_ic(ee_s2)



# 2. Dates
# -------------------------------

# Load an Image Sentinel2 level-1C
ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2016-01-01", "2016-01-31")$
  first()

# 1. From GEE to R
ee_s2$get("system:time_start")$getInfo() # bad
ee_s2$get("system:time_start") %>% eedate_to_rdate() # good!

# 2. From R to GEE
rdate_to_eedate("1981-01-01")
rdate_to_eedate(315532800000) # float number

# 3. Get the date of a ee$Image
ee_get_date_img(ee_s2)

# 4. Get the date of a ee$ImageCollection
nc <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
  st_transform(4326) %>%
  sf_as_ee()

ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2016-01-01", "2016-01-31")$
  filterBounds(nc)

ee_get_date_ic(ee_s2)


# 3. GeoVIZ
# -------------------------------

# Case 1: Display an FeatureCollection
roi <- ee$Geometry$Point(c(-122.2575, 37.8795)) %>%
  ee$Geometry$buffer(10000)
blocks <- ee$FeatureCollection("TIGER/2010/Blocks")
subset <- blocks$filterBounds(roi)
Map$centerObject(roi)
Map$addLayer(subset)

# Case 2: Display an Image
image <- ee$Image("CGIAR/SRTM90_V4")
band_viz = list(
  min = 0,
  max = 4000,
  palette = c(
    '0D0887', '5B02A3',
    '9A179B', 'CB4678',
    'EB7852', 'FBB32F',
    'F0F921'
  )
)
Map$setCenter()
Map$addLayer(image, band_viz)

# Case 3: Display an ImageCollection
ee_search_display("COPERNICUS/S2")
ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2016-01-01", "2016-01-31")$
  filterBounds(nc) %>%
  ee_get(0:5)

rgbVis <- list(
  min = 0,
  max = 3000,
  bands = c('B4', 'B3', 'B2')
)
Map$centerObject(nc)
Map$addLayers(ee_s2, rgbVis)

# Case 4: Edit
library(mapedit)
m1 <- Map$addLayer(image, band_viz)
my_geometry <- m1 %>% ee_as_mapview() %>% editMap()