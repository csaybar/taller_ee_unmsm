# DEMO_03: Random Forest with hyperparameter optimization
library(mlr3verse)
library(ranger)
library(raster)
library(rgee)
library(sf)
source("utils.R")
ee_Initialize()

# 1. Copy an external asset to your own account 
# -------------------------------------------------------

leandro_feature_points <- 'users/Leandro_OpenGeoHub/land_cover_toy_samples'
csaybar_feature_points <-  sprintf("%s/land_cover_toy_samples", ee_get_assethome())

ee_manage_copy(leandro_feature_points, csaybar_feature_points)
ee_manage_asset_access(csaybar_feature_points, all_users_can_read = TRUE)
samples <- ee$FeatureCollection(csaybar_feature_points)

# Visualize the samples extent and sample values
samples.extent <- samples$geometry()$bounds()
layers <- list(Map$addLayer(samples.extent, {}, "ROI"))
plot.samples(samples, 'land_cover_2019', colors, layers)
# ee_as_sf(samples)


# 2. Temporal Reduction
# -------------------------------------------------------

# We will use this expression to filter the cloud and cloud shadow pixels
# See Table 6-3 https://prd-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/atoms/files/LSDS-1368_L8_C1-LandSurfaceReflectanceCode-LASRC_ProductGuide-v3.pdf
valid.expr <- "(b('pixel_qa') >= 322) && (b('pixel_qa') <= 324)"

# The reduction will calculate percentile 25, 50 and 75 through time
reducer <- ee$Reducer$percentile(c(25,50,75))

# Reduce considering all the Landsat images inside samples.extent
l8.percs <- reduce.images('LANDSAT/LC08/C01/T1_SR', "2019-01-01", "2020-01-01", list('B2','B3','B4','B5','B6','B7','B10'),
                          samples.extent, valid.expr, prefix='L8_', reducer=reducer)

# What is the output ?
l8.percs.bands <- l8.percs$bandNames()$getInfo()

# Don't forget to configure the image visualization parameters
l8.percs.vizP50 <- gen.vizParams(l8.percs, list('L8_B4_p50','L8_B3_p50','L8_B2_p50'), samples.extent)

layers <- list(Map$addLayer(l8.percs, l8.percs.vizP50, "L8/2019 - P50"))
plot.samples(samples, 'land_cover_2019', colors, layers)

# 3. Overlay samples
# -------------------------------------------------------

# This function extracts the Landsat percentile values for the samples,
# exports a CSV file to Google Drive, download and load it in R
samples.l8.percs <- overlayer.samples(samples, l8.percs)

# If it's taking too long Load the existing results file in shared folder
# samples.l8.percs <- read.csv('/home/leandro/Code/OpenGeoHub/SummerSchool/results/overlay_20200817_193727_6279.csv')

# Filter only the columns of interest
samples.l8.percs <- samples.l8.percs[,c(l8.percs.bands, 'land_cover_2019')]

# Convert the predict variable to factor 
# avoiding mlr3 complains
samples.l8.percs$land_cover_2019 <- as.factor(samples.l8.percs$land_cover_2019)


# 4. Random Forest time!
# -------------------------------------------------------

set.seed(1, "L'Ecuyer")

# Create a new classification task
task <- TaskClassif$new("l8.percs", samples.l8.percs, target = "land_cover_2019")

# We will use Random Forest, but mlr3 support several learners
# See https://mlr3learners.mlr-org.com
lrn <- lrn("classif.ranger", importance = "impurity")


# Here we will specify only the hyperparameters supported by 
# GEE implementation (ee$Classifier$smileRandomForest)
mtry.upper <- as.integer((ncol(samples.l8.percs)-1) / 2)
prms <- ParamSet$new(list(
  ParamInt$new("min.node.size", lower = 1, upper = 10), ## minLeafPopulation in GEE
  ParamInt$new("num.trees", lower = 50, upper = 300), ## numberOfTrees in GEE
  ParamInt$new("mtry", lower = 1, upper = mtry.upper) ## variablesPerSplit in GEE
))

# Define the hyperparameters values
design = expand.grid(
  min.node.size = seq(1,11,3),
  mtry = seq(1, mtry.upper, 3),
  num.trees = seq(50, 301, 25)
)


# Create a tuning instance
tnr.config <- TuningInstanceSingleCrit$new(
  task = task,
  learner = lrn,
  resampling = rsmp("cv", folds = 5)$instantiate(task), # 5-Fold crossvalidation
  measure = msr(c("classif.acc")), # Maximize the overall accuracy
  search_space = prms,
  terminator = trm("none") # all design should be evaluated
)

# Load tuner
tuner <- tnr("design_points", design = as.data.table(design))

# Trigger the optimization
tuner$optimize(tnr.config)

# What are the most important variables ?
filter <- flt("importance", learner = lrn)
filter$calculate(task)

# Let's see the result of optimization
best.bands <- as.data.table(filter)
best.hparams <- tnr.config$result_learner_param_vals


# 5. GEE Prediction
# -------------------------------------------------------
training <- l8.percs$sampleRegions(
  collection = samples,
  properties = list("land_cover_2019"),
  scale = 30,
  tileScale=2  
)

# RF with default hyperparameters
rf1 <- ee$Classifier$smileRandomForest(100)

# RF with MLR3-optimized hyperparameters
rf2 <- ee$Classifier$smileRandomForest(
  numberOfTrees=best.hparams$num.trees, 
  variablesPerSplit=best.hparams$mtry, 
  minLeafPopulation=best.hparams$min.node.size
)

# Let's train the models
model1 <- rf1$train(training, 'land_cover_2019', l8.percs.bands)
model2 <- rf2$train(training, 'land_cover_2019', l8.percs.bands)

# ..and classify the whole area
classification1 <- l8.percs$classify(model1)
classification2 <- l8.percs$classify(model2)

# Let's compare the classification results
layers = list(Map$addLayer(l8.percs, l8.percs.vizP50, "L8/2019 - P50"))
layers = append(layers, Map$addLayer(classification1, list(min=1, max=5, palette=toString(colors)), name = "Land-cover classification - Default"))
layers = append(layers, Map$addLayer(classification2, list(min=1, max=5, palette=toString(colors)), name = "Land-cover classification - MLR3-optim."))
plot.samples(samples, 'land_cover_2019', colors, layers)

final_results <- ee_as_raster(classification2, scale = 30)

plot(final_results)
