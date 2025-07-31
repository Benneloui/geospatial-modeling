# Automate Data Loading from config.yml
library(terra)
library(yaml)

# Load config file
config <- yaml::read_yaml("config.yml")

# Helper to load rasters from a folder and file list
dynamic_load_rasters <- function(folder, file_list) {
  rasters <- list()
  for (f in file_list) {
    var <- gsub("\\.tif$", "", f)
    path <- file.path(folder, f)
    if (file.exists(path)) {
      rasters[[var]] <- rast(path)
    } else {
      warning(paste("Missing raster:", path))
    }
  }
  rasters
}

# Load current bio rasters
data_dir <- "data/climate/current/bio"
bio_current <- dynamic_load_rasters(data_dir, config$climate_files$current$bio)

# Load future bio rasters
data_dir_future <- "data/climate/future/bio"
bio_future <- dynamic_load_rasters(data_dir_future, config$climate_files$future$bio)

# Print loaded raster names
cat("Loaded current bio rasters:\n", names(bio_current), "\n")
cat("Loaded future bio rasters:\n", names(bio_future), "\n")

# Load elevation raster
if (!is.null(config$elevation_file)) {
  elevation <- rast(file.path("data", config$elevation_file))
} else {
  warning("Elevation file not specified in config!")
}

# Calculate slope and aspect from elevation
if (exists("elevation")) {
  slope <- terrain(elevation, v = "slope", unit = "degrees")
  aspect <- terrain(elevation, v = "aspect", unit = "degrees")
}

# Load irrigation raster
if (!is.null(config$environmental$irrigation_layer)) {
  irrigation <- rast(config$environmental$irrigation_layer)
} else {
  warning("Irrigation raster not specified in config!")
}

# Load soil pH raster
if (!is.null(config$environmental$soil_ph_raster)) {
  soil_ph <- rast(config$environmental$soil_ph_raster)
} else {
  warning("Soil pH raster not specified in config!")
}

# Ensure all rasters are in the same CRS
target_crs <- crs(template)
slope <- project(slope, target_crs)
aspect <- project(aspect, target_crs)
irrigation <- project(irrigation, target_crs)
soil_ph <- project(soil_ph, target_crs)

# Then resample as before
template <- elevation
slope <- resample(slope, template)
aspect <- resample(aspect, template)
irrigation <- resample(irrigation, template)
soil_ph <- resample(soil_ph, template)


plot(elevation)
plot(slope)
