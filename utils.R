cloudMaskL573 <- function(image) {
  qa <- image$select("pixel_qa")
  # If the cloud bit (5) is set and the cloud confidence (7) is high
  # or the cloud shadow bit is set (3), then it's a bad pixel.
  cloud <- qa$bitwiseAnd(bitwShiftL(1, 5))$
    And(qa$bitwiseAnd(bitwShiftL(1, 7)))$
    Or(qa$bitwiseAnd(bitwShiftL(1, 3)))
  # Remove edge pixels that don't occur in all bands
  mask2 <- image$mask()$reduce(ee$Reducer$min())
  image$updateMask(cloud$Not())$updateMask(mask2)
}

download_ndvi <- function(ee_user, area, scale, bucket) {
  ee_Initialize(ee_user, gcs = TRUE)
  # Map the function over the collection and take the median.
  l5_composite_ndvi <- ee$ImageCollection("LANDSAT/LT05/C01/T1_SR") %>% 
    ee$ImageCollection$filterDate("2010-04-01", "2010-07-30") %>% 
    ee$ImageCollection$map(cloudMaskL457) %>% 
    ee$ImageCollection$median() %>% 
    ee$Image$normalizedDifference(c("B4", "B3"))
  ee_area <- area %>% sf_as_ee()
  img_to_gcs <- ee_image_to_gcs(
    image = l5_composite, 
    region = ee_area,
    scale = scale,
    bucket = bucket 
  )
  img_to_gcs$start()
  img_to_gcs
}

download_ndvi_batch <- function(ee_user, area, scale, bucket) {
  roi_area_batch <- st_make_grid(area, n = c(length(ee_users), 1))
  task_list <- NULL
  for (index in seq_along(ee_users)) {
    gen_task <- download_ndvi(
      ee_user = ee_users[index],
      area =  roi_area_batch[index,],
      scale = scale,
      bucket = "rgee_dev"
    )
    task_list <- append(x = task_list, values = gen_task) 
  }
  ee_monitoring()
  lapply(task_list, function(x) raster::raster(ee_gcs_to_local(x)))
}

#' @author Leandro Parente
plot.samples <- function(samples, property, colors, layers = list()) {
  for(class.value in names(colors)) {
    color <- toString(colors[class.value])
    layer.fc <- samples$filterMetadata(property, 'Equals', as.numeric(class.value))
    layer.name <- paste0('Class ', class.value, ' (' , layer.fc$size()$getInfo(), ' samples)')
    print(paste0('Preparing ', layer.name))
    layers[[class.value]] <- Map$addLayer(layer.fc$draw(color), {}, layer.name)
  }
  
  Map$centerObject(samples$geometry(),6)
  Reduce('+', layers)
}

#' @author Leandro Parente
gen.vizParams <- function(image, bands, geometry, p1=2, p2=98, scale = 1000) {
  percent.stats <- image$reduceRegion(
    reducer = ee$Reducer$percentile(c(p1, p2)), 
    geometry = geometry, 
    bestEffort = TRUE,
    scale = scale,
    maxPixels = 1E13,
    tileScale = 2
  )$getInfo()
  
  viz <- list(
    bands = bands, 
    min = as.numeric(unlist(percent.stats[sprintf('%s_p%s', bands, rep(p1,3))])),
    max = as.numeric(unlist(percent.stats[sprintf('%s_p%s', bands, rep(p2,3))]))
  )
  return(viz)
}

#' @author Leandro Parente
reduce.images <- function(imgCollectionId, startDt, endDt, bands, geometry, valid.expr, prefix='', reducer = ee$Reducer$median()) {
  cloud.remove <- function(image) {
    valid.mask <- image$expression(valid.expr)
    image <- image$mask(valid.mask)$select(bands)
    return(image)
  }
  
  result <- ee$ImageCollection(imgCollectionId)$
    filterDate(startDt, endDt)$
    filterBounds(geometry)$
    map(cloud.remove)$
    reduce(reducer)$
    clip(geometry)$
    regexpRename('^',paste0(prefix))
  
  return(result)
}

#' @author Leandro Parente
overlayer.samples <- function(samples, image) {
  samples.ov <- samples$map(function(feat) {
    geom <- feat$geometry()
    result <- feat$set(image$reduceRegion(ee$Reducer$first(), geom, 30))
    return(result)
  })
  task.name <- paste0('overlay_', format(Sys.time(), format='%Y%m%d_%H%M%S'), '_', toString(sample(1:10000, 1)))
  task.params <- list(driveFolder =  'RGEE', fileFormat = 'CSV')
  
  print(paste0("Exporting to ", task.name))
  
  task <- ee$batch$Export$table(samples.ov, task.name, task.params)
  task$start()
  
  ee_monitoring()
  
  result.file <- ee_drive_to_local(task)
  result <- read.csv(result.file)
  
  return(result)
}

