# Rule-based GIS suitability analysis combining terrain, soil, and climate
# Suitability mapping
# Load the packages with library()
# Use official EPSG parameters for GeoTIFF CRS
Sys.setenv(GTIFF_SRS_SOURCE = "EPSG")

# Load required libraries
library(terra)
library(sf)
library(tmap)
library(spatialEco)
library(BAMMtools)
library(cols4all) 

# Check file existence before loading rasters
if (!file.exists("output/rasters/bio12_current_swiss.tif")) stop("❌ Precipitation raster missing!")
if (!file.exists("data/climate/current/bio/CHELSA_bio17_1981-2010_V.2.1.tif")) stop("❌ bio17 raster missing!")
if (!file.exists("data/climate/current/bio/CHELSA_rsds_1981-2010_mean_V.2.1.tif")) stop("❌ Solar radiation raster missing!")
if (!file.exists("output/rasters/slope_swiss.tif")) stop("❌ Slope raster missing!")
if (!file.exists("output/rasters/elevation_swiss.tif")) stop("❌ Elevation raster missing!")
if (!file.exists("data/landuse/irrigation_LGRIP30_2015_N40E00_001_2023014175240.tif")) stop("❌ Irrigation raster missing!")
if (!file.exists("output/rasters/bio1_current_swiss.tif")) stop("❌ Temperature raster missing!")

# Load rasters with terra
# 🌧️ Precipitation: Kein Optimum berücksichtigt! Höher ≠ immer besser.
precipitation <- rast("output/rasters/bio12_current_swiss.tif")
names(precipitation) <- "precipitationValues"
# 💧 bio17 (precipitation of driest quarter): Kein Optimum berücksichtigt!
bio17 <- rast("data/climate/current/bio/CHELSA_bio17_1981-2010_V.2.1.tif")
names(bio17) <- "bio17Values"
# ☀️ Solar radiation: Höher = besser. Keine Umkehr nötig.
solar_radiation <- rast("data/climate/current/bio/CHELSA_rsds_1981-2010_mean_V.2.1.tif")
names(solar_radiation) <- "solar_radiationValues"
# 🏔️ Slope: Flacher = besser. Umkehrung erfolgt korrekt.
slope <- rast("output/rasters/slope_swiss.tif")
names(slope) <- "slopeValues"
# ⛰️ Elevation: Kein Optimum berücksichtigt! Höher ≠ besser.
elevation <- rast("output/rasters/elevation_swiss.tif")
names(elevation) <- "elevationValues"
# 💧 Irrigation: Höher = besser angenommen. Keine Transformation nötig.
Irrigation <- rast("data/landuse/irrigation_LGRIP30_2015_N40E00_001_2023014175240.tif")
names(Irrigation) <- "IrrigationValues"
# 🌡️ Temperature: Kein Optimum berücksichtigt! Höher ≠ immer besser.
temperature <- rast("output/rasters/bio1_current_swiss.tif")
names(temperature) <- "temperatureValues"


# Ensure all rasters have the same CRS as precipitation
# Use terra::crs and terra::project
target_crs <- crs(precipitation)

if (!compareGeom(solar_radiation, precipitation, stopOnError = FALSE)) {
  if (!is.na(crs(solar_radiation))) {
    solar_radiation <- project(solar_radiation, target_crs)
  } else {
    stop("❌ 'solar_radiation' has no CRS; cannot project.")
  }
}
if (!compareGeom(slope, precipitation, stopOnError = FALSE)) {
  if (!is.na(crs(slope))) {
    slope <- project(slope, target_crs)
  } else {
    stop("❌ 'slope' has no CRS; cannot project.")
  }
}
if (!compareGeom(Irrigation, precipitation, stopOnError = FALSE)) {
  if (!is.na(crs(Irrigation))) {
    Irrigation <- project(Irrigation, target_crs)
  } else {
    stop("❌ 'Irrigation' has no CRS; cannot project.")
  }
}
if (!compareGeom(bio17, precipitation, stopOnError = FALSE)) {
  if (!is.na(crs(bio17))) {
    bio17 <- project(bio17, target_crs)
  } else {
    stop("❌ 'bio17' has no CRS; cannot project.")
  }
}
if (!compareGeom(elevation, precipitation, stopOnError = FALSE)) {
  if (!is.na(crs(elevation))) {
    elevation <- project(elevation, target_crs)
  } else {
    stop("❌ 'elevation' has no CRS; cannot project.")
  }
}
if (!compareGeom(temperature, precipitation, stopOnError = FALSE)) {
  if (!is.na(crs(temperature))) {
    temperature <- project(temperature, target_crs)
  } else {
    stop("❌ 'temperature' has no CRS; cannot project.")
  }
}

# Ensure all rasters have the same extent and resolution
template_raster <- precipitation  # choose a reference raster

# Resample rasters
solar_radiation <- resample(solar_radiation, template_raster, method = "bilinear")
slope <- resample(slope, template_raster, method = "bilinear")
Irrigation <- resample(Irrigation, template_raster, method = "bilinear")
bio17 <- resample(bio17, template_raster, method = "bilinear")
elevation <- resample(elevation, template_raster, method = "bilinear")
temperature <- resample(temperature, template_raster, method = "bilinear")
# If proximity_rivers is used, ensure it's loaded with terra and resampled
# proximity_rivers <- rast("<path_to_proximity_rivers>")
# proximity_rivers <- resample(proximity_rivers, template_raster, method = "bilinear")

# Read Swiss boundary shapefile
swiss_boundary <- st_read("data/boundaries/country_boundary.geojson")
swiss_boundary <- st_transform(swiss_boundary, crs(precipitation))

# Crop and mask all rasters to Swiss boundary
# Mask/crop rasters
precipitation <- mask(crop(precipitation, vect(swiss_boundary)), vect(swiss_boundary))
solar_radiation <- mask(crop(solar_radiation, vect(swiss_boundary)), vect(swiss_boundary))
slope <- mask(crop(slope, vect(swiss_boundary)), vect(swiss_boundary))
Irrigation <- mask(crop(Irrigation, vect(swiss_boundary)), vect(swiss_boundary))
bio17 <- mask(crop(bio17, vect(swiss_boundary)), vect(swiss_boundary))
temperature <- mask(crop(temperature, vect(swiss_boundary)), vect(swiss_boundary))
# If proximity_rivers is used:
# proximity_rivers <- mask(crop(proximity_rivers, vect(swiss_boundary)), vect(swiss_boundary))

# To visualize raster data, for instance solar_radiation
# Spectral colours are useful for diverging scales "Spectral" is Rd-Or-Yl-Gr-Bu. "-Spectral" reverses the order
tm_shape(solar_radiation) + tm_raster(style = "cont", title = "solar radiation", palette= "-Spectral")+ tm_shape(swiss_boundary) +
  tm_polygons(fill_alpha = 0, border.col = "black") + tm_layout(frame = FALSE, legend.outside = TRUE)


# ☀️ Solar radiation: Höher = besser. Keine Umkehr nötig.
# cleaning for temp
# Extract values from Raster
solar_radiationValues <- values(solar_radiation) 

# Change the values from vector object to data.frame object
solar_radiationDF <- as.data.frame(solar_radiationValues)

# Remove missing values and reapply column name
solar_radiationDF <- as.data.frame(solar_radiationDF[!is.na(solar_radiationDF$solar_radiationValues),])
colnames(solar_radiationDF) <- "solar_radiationValues"

# Use the getJenksBreaks() function. Sample 0.010 (1%) of the pixels 
#---at random and base the categorisation on this. 
# NOTE: Doing this on the full data will take forever - so use the subset argument. 
solar_radiation_Jenks <- getJenksBreaks(solar_radiationDF$solar_radiationValues, 10, subset = nrow(solar_radiationDF)*0.01)

# See value in vector
solar_radiation_Jenks

# shows first element
solar_radiation_Jenks[1] 

# shows second element
solar_radiation_Jenks[2] 
min_val <- min(values(solar_radiation), na.rm = TRUE)
min_val
max_val <- max(values(solar_radiation), na.rm = TRUE)# so on and so further...
max_val
# Create categorisation by using the Jenks values in the vector
solar_radiation_Jenks_cl <- c(0, solar_radiation_Jenks[1], 1,
                   solar_radiation_Jenks[1], solar_radiation_Jenks[2], 2,
                   solar_radiation_Jenks[2], solar_radiation_Jenks[3], 3,
                   solar_radiation_Jenks[3], solar_radiation_Jenks[4], 4,
                   solar_radiation_Jenks[4], solar_radiation_Jenks[5], 5,
                   solar_radiation_Jenks[5], solar_radiation_Jenks[6], 6,
                   solar_radiation_Jenks[6], solar_radiation_Jenks[7], 7,
                   solar_radiation_Jenks[7], solar_radiation_Jenks[8], 8,
                   solar_radiation_Jenks[8], solar_radiation_Jenks[9], 9,
                   solar_radiation_Jenks[9], max_val, 10) 

solar_radiation_Jenks_cl
# create matrix
solar_radiation_Jenks_cl_mat <- matrix(solar_radiation_Jenks_cl, ncol = 3, byrow = TRUE)
# view categorisation in matrix
solar_radiation_Jenks_cl_mat
solar_radiation_Jenks_cl_mat
# reclassify original raster using the jenks classifications
solar_radiation_jenks_recl <- classify(solar_radiation, solar_radiation_Jenks_cl_mat)
solar_radiation_jenks_recl
# 🔍 DIAGNOSTIC: Check for NA in solar_radiation_jenks_recl
message("🔍 Checking for NA values in solar_radiation_jenks_recl:")
print(global(solar_radiation_jenks_recl, c("min", "max", "mean", "notNA"), na.rm = FALSE))
tm_shape(solar_radiation_jenks_recl) + tm_raster(
  col.scale = tm_scale_continuous(values = cols4all::c4a("brewer.spectral")),
  col.legend = tm_legend(title = "solar_radiation (on Jenks scale)"),
  midpoint = NA
) +
  tm_shape(swiss_boundary) + tm_polygons(fill_alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# 🌧️ Precipitation: Kein Optimum berücksichtigt! Höher ≠ immer besser.
#We need to repeat this process of reclassification and standardization using natural breaks for the remaining raster grids
# Extract values from Raster
precipitationValues <- values(precipitation) 

# Change the values from vector object to data.frame object
precipitationDF <- as.data.frame(precipitationValues)

# Remove missing values and reapply column name
precipitationDF <- as.data.frame(precipitationDF[!is.na(precipitationDF$precipitationValues),])
colnames(precipitationDF) <- "precipitationValues"

# Use the getJenksBreaks() function. Sample 0.010 (1%) of the pixels 
#---at random and base the categorisation on this. 
# NOTE: Doing this on the full data will take forever - so use the subset argument. 
precipitation_Jenks <- getJenksBreaks(precipitationDF$precipitationValues, 10, subset = nrow(precipitationDF)*0.01)

# See value in vector
precipitation_Jenks

# shows first element
precipitation_Jenks[1] 

# shows second element
precipitation_Jenks[2] 
min_val <- min(values(precipitation), na.rm = TRUE)
min_val
precipitation_Jenks[1]
max_val <- max(values(precipitation), na.rm = TRUE)# so on and so further...
max_val
# Create categorisation by using the Jenks values in the vector
precipitation_Jenks_cl <- c(0, precipitation_Jenks[1], 1,
                                 precipitation_Jenks[1], precipitation_Jenks[2], 2,
                                 precipitation_Jenks[2], precipitation_Jenks[3], 3,
                                 precipitation_Jenks[3], precipitation_Jenks[4], 4,
                                 precipitation_Jenks[4], precipitation_Jenks[5], 5,
                                 precipitation_Jenks[5], precipitation_Jenks[6], 6,
                                 precipitation_Jenks[6], precipitation_Jenks[7], 7,
                                 precipitation_Jenks[7], precipitation_Jenks[8], 8,
                                 precipitation_Jenks[8], precipitation_Jenks[9], 9,
                                 precipitation_Jenks[9], max_val, 10) 

precipitation_Jenks_cl
# create matrix
precipitation_Jenks_cl_mat <- matrix(precipitation_Jenks_cl, ncol = 3, byrow = TRUE)
# view categorisation in matrix
precipitation_Jenks_cl_mat

# reclassify original raster using the jenks classifications
precipitation_Jenks_recl <- classify(precipitation, precipitation_Jenks_cl_mat)
precipitation_Jenks_recl
# 🔍 DIAGNOSTIC: Check for NA in precipitation_Jenks_recl
message("🔍 Checking for NA values in precipitation_Jenks_recl:")
print(global(precipitation_Jenks_recl, c("min", "max", "mean", "notNA"), na.rm = FALSE))
tm_shape(precipitation_Jenks_recl) + tm_raster(
  col.scale = tm_scale_continuous(values = cols4all::c4a("brewer.spectral")),
  col.legend = tm_legend(title = "precipitation (on Jenks scale)"),
  midpoint = NA
) +
  tm_shape(swiss_boundary) + tm_polygons(fill_alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

#🏔️ Slope: Flacher = besser. Umkehrung erfolgt korrekt.
#We need to repeat this process of reclassification and standardization using natural breaks for the remaining raster grids
# Extract values from Raster
slopeValues <- values(slope) 

# Change the values from vector object to data.frame object
slopeDF <- as.data.frame(slopeValues)

# Remove missing values and reapply column name
slopeDF <- as.data.frame(slopeDF[!is.na(slopeDF$slopeValues),])
colnames(slopeDF) <- "slopeValues"

# Use the getJenksBreaks() function. Sample 0.010 (1%) of the pixels 
#---at random and base the categorisation on this. 
# NOTE: Doing this on the full data will take forever - so use the subset argument. 
slope_Jenks <- getJenksBreaks(slopeDF$slopeValues, 10, subset = nrow(slopeDF)*0.01)

# See value in vector
slope_Jenks

# shows first element
slope_Jenks[1] 

# shows second element
slope_Jenks[2] 
min_val <- min(values(slope), na.rm = TRUE)
min_val
max_val <- max(values(slope), na.rm = TRUE)# so on and so further...
max_val
## Create categorisation by using the Jenks values in the vector
slope_Jenks_cl <- c(min(slope_Jenks[1], min_val), max(slope_Jenks[1], min_val), 1,
                          slope_Jenks[1], slope_Jenks[2], 2,
                          slope_Jenks[2], slope_Jenks[3], 3,
                          slope_Jenks[3], slope_Jenks[4], 4,
                          slope_Jenks[4], slope_Jenks[5], 5,
                          slope_Jenks[5], slope_Jenks[6], 6,
                          slope_Jenks[6], slope_Jenks[7], 7,
                          slope_Jenks[7], slope_Jenks[8], 8,
                          slope_Jenks[8], slope_Jenks[9], 9,
                          slope_Jenks[9], max_val, 10) 

slope_Jenks_cl
# create matrix
slope_Jenks_cl_mat <- matrix(slope_Jenks_cl, ncol = 3, byrow = TRUE)
# view categorisation in matrix
slope_Jenks_cl_mat
slope_Jenks_cl_mat
# Remove zero-width intervals from slope classification matrix
slope_Jenks_cl_mat <- slope_Jenks_cl_mat[slope_Jenks_cl_mat[,1] < slope_Jenks_cl_mat[,2], ]
# reclassify original raster using the jenks classifications
  slope_Jenks_cl <- classify(slope, slope_Jenks_cl_mat)
# 🔍 DIAGNOSTIC: Check for NA in slope_Jenks_cl
message("🔍 Checking for NA values in slope_Jenks_cl:")
print(global(slope_Jenks_cl, c("min", "max", "mean", "notNA"), na.rm = FALSE))

#For slope, the risk of suitability of solar_powered_irrigation decreases with higher values for elevation. Therefore, after applying the Jenks intervals, we need to flip the raster values accordingly.
slope_Jenks_cl
tm_shape(slope_Jenks_cl) + tm_raster(
  col.scale = tm_scale_continuous(values = cols4all::c4a("brewer.spectral")),
  col.legend = tm_legend(title = "slope (on Jenks scale)"),
  midpoint = NA
) +
  tm_shape(swiss_boundary) + tm_polygons(fill_alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# Flip slope values for suitability (terra syntax)
max_val <- global(slope_Jenks_cl, "max", na.rm=TRUE)[1,1]
rev_slope_jenks_cl <- slope_Jenks_cl
values(rev_slope_jenks_cl) <- max_val - values(slope_Jenks_cl)
# 🔍 DIAGNOSTIC: Check for NA in rev_slope_jenks_cl
message("🔍 Checking for NA values in rev_slope_jenks_cl:")
print(global(rev_slope_jenks_cl, c("min", "max", "mean", "notNA"), na.rm = FALSE))
# see plot flipped/inverted
plot(rev_slope_jenks_cl)

# 💧 Irrigation: Höher = besser angenommen. Keine Transformation nötig.
#Irrigation
#We need to repeat this process of reclassification and standardization using natural breaks for the remaining raster grids
# Extract values from Raster
IrrigationValues <- values(Irrigation) 

# Change the values from vector object to data.frame object
IrrigationDF <- as.data.frame(IrrigationValues)

# Remove missing values and reapply column name
IrrigationDF <- as.data.frame(IrrigationDF[!is.na(IrrigationDF$IrrigationValues),])
colnames(IrrigationDF) <- "IrrigationValues"

# Use the getJenksBreaks() function. Sample 0.010 (1%) of the pixels 
#---at random and base the categorisation on this. 
# NOTE: Doing this on the full data will take forever - so use the subset argument. 
Irrigation_Jenks <- getJenksBreaks(IrrigationDF$IrrigationValues, 10, subset = nrow(IrrigationDF)*0.01)

# See value in vector
Irrigation_Jenks

# shows first element
Irrigation_Jenks[1] 

# shows second element
Irrigation_Jenks[2] 
min_val <- min(values(Irrigation), na.rm = TRUE)
min_val
max_val <- max(values(Irrigation), na.rm = TRUE)# so on and so further...
max_val
# Create categorisation by using the Jenks values in the vector
Irrigation_Jenks_cl <- c(0, Irrigation_Jenks[1], 1,
                         Irrigation_Jenks[1], Irrigation_Jenks[2], 2,
                         Irrigation_Jenks[2], Irrigation_Jenks[3], 3,
                         Irrigation_Jenks[3], Irrigation_Jenks[4], 4,
                         Irrigation_Jenks[4], Irrigation_Jenks[5], 5,
                         Irrigation_Jenks[5], Irrigation_Jenks[6], 6,
                         Irrigation_Jenks[6], Irrigation_Jenks[7], 7,
                         Irrigation_Jenks[7], Irrigation_Jenks[8], 8,
                         Irrigation_Jenks[8], Irrigation_Jenks[9], 9,
                         Irrigation_Jenks[9], max_val, 10) 

Irrigation_Jenks_cl
# create matrix
Irrigation_Jenks_cl_mat <- matrix(Irrigation_Jenks_cl, ncol = 3, byrow = TRUE)
# view categorisation in matrix
Irrigation_Jenks_cl_mat
# reclassify original raster using the jenks classifications
Irrigation_Jenks_cl <- classify(Irrigation, Irrigation_Jenks_cl_mat)
Irrigation_Jenks_cl
# 🔍 DIAGNOSTIC: Check for NA in Irrigation_Jenks_cl
message("🔍 Checking for NA values in Irrigation_Jenks_cl:")
print(global(Irrigation_Jenks_cl, c("min", "max", "mean", "notNA"), na.rm = FALSE))
tm_shape(Irrigation_Jenks_cl) + tm_raster(
  col.scale = tm_scale_continuous(values = cols4all::c4a("brewer.spectral")),
  col.legend = tm_legend(title = "Irrigation (on Jenks scale)"),
  midpoint = NA
) +
  tm_shape(swiss_boundary) + tm_polygons(fill_alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# 🌡️ Temperature: Kein Optimum berücksichtigt! Höher ≠ immer besser.
# Temperature Jenks reclassification
# Extract values from Raster
temperatureValues <- values(temperature)
# Change the values from vector object to data.frame object
temperatureDF <- as.data.frame(temperatureValues)
# Remove missing values and reapply column name
temperatureDF <- as.data.frame(temperatureDF[!is.na(temperatureDF$temperatureValues),])
colnames(temperatureDF) <- "temperatureValues"
# Use the getJenksBreaks() function. Sample 0.010 (1%) of the pixels 
temperature_Jenks <- getJenksBreaks(temperatureDF$temperatureValues, 10, subset = nrow(temperatureDF)*0.01)
# See value in vector
temperature_Jenks
# shows first element
temperature_Jenks[1]
# shows second element
temperature_Jenks[2]
min_val <- min(values(temperature), na.rm = TRUE)
min_val
max_val <- max(values(temperature), na.rm = TRUE)
max_val
# Create categorisation by using the Jenks values in the vector
temperature_Jenks_cl <- c(0, temperature_Jenks[1], 1,
                         temperature_Jenks[1], temperature_Jenks[2], 2,
                         temperature_Jenks[2], temperature_Jenks[3], 3,
                         temperature_Jenks[3], temperature_Jenks[4], 4,
                         temperature_Jenks[4], temperature_Jenks[5], 5,
                         temperature_Jenks[5], temperature_Jenks[6], 6,
                         temperature_Jenks[6], temperature_Jenks[7], 7,
                         temperature_Jenks[7], temperature_Jenks[8], 8,
                         temperature_Jenks[8], temperature_Jenks[9], 9,
                         temperature_Jenks[9], max_val, 10) 
temperature_Jenks_cl
# create matrix
temperature_Jenks_cl_mat <- matrix(temperature_Jenks_cl, ncol = 3, byrow = TRUE)
# view categorisation in matrix
temperature_Jenks_cl_mat
# Remove zero-width intervals from temperature classification matrix
temperature_Jenks_cl_mat <- temperature_Jenks_cl_mat[temperature_Jenks_cl_mat[,1] < temperature_Jenks_cl_mat[,2], ]
# reclassify original raster using the jenks classifications
temperature_jenks_recl <- classify(temperature, temperature_Jenks_cl_mat)
temperature_jenks_recl
# 🔍 DIAGNOSTIC: Check for NA in temperature_jenks_recl
message("🔍 Checking for NA values in temperature_jenks_recl:")
print(global(temperature_jenks_recl, c("min", "max", "mean", "notNA"), na.rm = FALSE))
tm_shape(temperature_jenks_recl) + tm_raster(
  col.scale = tm_scale_continuous(values = cols4all::c4a("brewer.spectral")),
  col.legend = tm_legend(title = "Temperature (on Jenks scale)"),
  midpoint = NA
) +
  tm_shape(swiss_boundary) + tm_polygons(fill_alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)


# 💧 bio17 (precipitation of driest quarter): Kein Optimum berücksichtigt!
# Jenks classification for bio17
bio17Values <- values(bio17)
bio17DF <- as.data.frame(bio17Values)
bio17DF <- as.data.frame(bio17DF[!is.na(bio17DF$bio17Values),])
colnames(bio17DF) <- "bio17Values"
bio17_Jenks <- getJenksBreaks(bio17DF$bio17Values, 10, subset = nrow(bio17DF)*0.01)
min_val <- min(values(bio17), na.rm = TRUE)
max_val <- max(values(bio17), na.rm = TRUE)
bio17_Jenks_cl <- c(0, bio17_Jenks[1], 1,
                   bio17_Jenks[1], bio17_Jenks[2], 2,
                   bio17_Jenks[2], bio17_Jenks[3], 3,
                   bio17_Jenks[3], bio17_Jenks[4], 4,
                   bio17_Jenks[4], bio17_Jenks[5], 5,
                   bio17_Jenks[5], bio17_Jenks[6], 6,
                   bio17_Jenks[6], bio17_Jenks[7], 7,
                   bio17_Jenks[7], bio17_Jenks[8], 8,
                   bio17_Jenks[8], bio17_Jenks[9], 9,
                   bio17_Jenks[9], max_val, 10)
# create matrix
bio17_Jenks_cl_mat <- matrix(bio17_Jenks_cl, ncol = 3, byrow = TRUE)
# Remove zero-width intervals from bio17 classification matrix
bio17_Jenks_cl_mat <- bio17_Jenks_cl_mat[bio17_Jenks_cl_mat[,1] < bio17_Jenks_cl_mat[,2], ]
bio17_jenks_recl <- classify(bio17, bio17_Jenks_cl_mat)
# 🔍 DIAGNOSTIC: Check for NA in bio17_jenks_recl
message("🔍 Checking for NA values in bio17_jenks_recl:")
print(global(bio17_jenks_recl, c("min", "max", "mean", "notNA"), na.rm = FALSE))
tm_shape(bio17_jenks_recl) + tm_raster(
  col.scale = tm_scale_continuous(values = cols4all::c4a("brewer.spectral")),
  col.legend = tm_legend(title = "BIO17 (on Jenks scale)"),
  midpoint = NA
) +
  tm_shape(swiss_boundary) + tm_polygons(fill_alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# ⛰️ Elevation: Kein Optimum berücksichtigt! Höher ≠ besser.
# Jenks classification for elevation
elevationValues <- values(elevation)
elevationDF <- as.data.frame(elevationValues)
elevationDF <- as.data.frame(elevationDF[!is.na(elevationDF$elevationValues),])
colnames(elevationDF) <- "elevationValues"
elevation_Jenks <- getJenksBreaks(elevationDF$elevationValues, 10, subset = nrow(elevationDF)*0.01)
min_val <- min(values(elevation), na.rm = TRUE)
max_val <- max(values(elevation), na.rm = TRUE)
elevation_Jenks_cl <- c(0, elevation_Jenks[1], 1,
                       elevation_Jenks[1], elevation_Jenks[2], 2,
                       elevation_Jenks[2], elevation_Jenks[3], 3,
                       elevation_Jenks[3], elevation_Jenks[4], 4,
                       elevation_Jenks[4], elevation_Jenks[5], 5,
                       elevation_Jenks[5], elevation_Jenks[6], 6,
                       elevation_Jenks[6], elevation_Jenks[7], 7,
                       elevation_Jenks[7], elevation_Jenks[8], 8,
                       elevation_Jenks[8], elevation_Jenks[9], 9,
                       elevation_Jenks[9], max_val, 10)
# create matrix
elevation_Jenks_cl_mat <- matrix(elevation_Jenks_cl, ncol = 3, byrow = TRUE)
# Remove zero-width intervals from elevation classification matrix
elevation_Jenks_cl_mat <- elevation_Jenks_cl_mat[elevation_Jenks_cl_mat[,1] < elevation_Jenks_cl_mat[,2], ]
elevation_jenks_recl <- classify(elevation, elevation_Jenks_cl_mat)
# 🔍 DIAGNOSTIC: Check for NA in elevation_jenks_recl
message("🔍 Checking for NA values in elevation_jenks_recl:")
print(global(elevation_jenks_recl, c("min", "max", "mean", "notNA"), na.rm = FALSE))
tm_shape(elevation_jenks_recl) + tm_raster(
  col.scale = tm_scale_continuous(values = cols4all::c4a("brewer.spectral")),
  col.legend = tm_legend(title = "Elevation (on Jenks scale)"),
  midpoint = NA
) +
  tm_shape(swiss_boundary) + tm_polygons(fill_alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)


# 🌡️🌧️⛰️ Gaussian suitability transformations (optimal range assumed) replacing Jenks-based suitability for: temperature, precipitation, and elevation
# 🌡️
optimal_temp <- 16
sd_temp <- 3
temperature_suitability <- exp(-((temperature - optimal_temp)^2) / (2 * sd_temp^2))

# 🌧️
optimal_prec <- 700
sd_prec <- 150
precipitation_suitability <- exp(-((precipitation - optimal_prec)^2) / (2 * sd_prec^2))

# ⛰️
optimal_elev <- 400
sd_elev <- 200
elevation_suitability <- exp(-((elevation - optimal_elev)^2) / (2 * sd_elev^2))

# Resample suitability rasters to solar_radiation template
temperature_suitability_resampled <- resample(temperature_suitability, solar_radiation_jenks_recl, method = "bilinear")
precipitation_suitability_resampled <- resample(precipitation_suitability, solar_radiation_jenks_recl, method = "bilinear")
elevation_suitability_resampled <- resample(elevation_suitability, solar_radiation_jenks_recl, method = "bilinear")

# Second resampling- The final resampling ensures that all classified rasters used in the weighted overlay (suitablemap_WLC):
temperature_jenks_recl_resampled <- resample(temperature_jenks_recl, solar_radiation_jenks_recl, method = "bilinear")
# 🔍 DIAGNOSTIC: Check for NA in temperature_jenks_recl_resampled
message("🔍 Checking for NA values in temperature_jenks_recl_resampled:")
print(global(temperature_jenks_recl_resampled, c("min", "max", "mean", "notNA"), na.rm = FALSE))

precipitation_Jenks_recl_resampled <- resample(precipitation_Jenks_recl, solar_radiation_jenks_recl, method = "bilinear")  # or "ngb" for categorical
# 🔍 DIAGNOSTIC: Check for NA in precipitation_Jenks_recl_resampled
message("🔍 Checking for NA values in precipitation_Jenks_recl_resampled:")
print(global(precipitation_Jenks_recl_resampled, c("min", "max", "mean", "notNA"), na.rm = FALSE))

Irrigation_Jenks_cl_resampled <- resample(Irrigation_Jenks_cl, solar_radiation_jenks_recl, method = "bilinear")  # or "ngb" for categorical
# 🔍 DIAGNOSTIC: Check for NA in Irrigation_Jenks_cl_resampled
message("🔍 Checking for NA values in Irrigation_Jenks_cl_resampled:")
print(global(Irrigation_Jenks_cl_resampled, c("min", "max", "mean", "notNA"), na.rm = FALSE))

rev_slope_jenks_cl_resampled <- resample(rev_slope_jenks_cl, solar_radiation_jenks_recl, method = "bilinear")  # or "ngb" for categorical
# 🔍 DIAGNOSTIC: Check for NA in rev_slope_jenks_cl_resampled
message("🔍 Checking for NA values in rev_slope_jenks_cl_resampled:")
print(global(rev_slope_jenks_cl_resampled, c("min", "max", "mean", "notNA"), na.rm = FALSE))

bio17_jenks_recl <- resample(bio17_jenks_recl, solar_radiation_jenks_recl, method = "bilinear")
# 🔍 DIAGNOSTIC: Check for NA in bio17_jenks_recl (resampled)
message("🔍 Checking for NA values in bio17_jenks_recl (resampled):")
print(global(bio17_jenks_recl, c("min", "max", "mean", "notNA"), na.rm = FALSE))

elevation_jenks_recl <- resample(elevation_jenks_recl, solar_radiation_jenks_recl, method = "bilinear")
# 🔍 DIAGNOSTIC: Check for NA in elevation_jenks_recl (resampled)
message("🔍 Checking for NA values in elevation_jenks_recl (resampled):")
print(global(elevation_jenks_recl, c("min", "max", "mean", "notNA"), na.rm = FALSE))


# Multi-criteria decision analysis (MCDA) 
# Replace the original temperature, precipitation, and elevation layers with suitability rasters
suitablemap_WLC <- (
  0.197 * solar_radiation_jenks_recl +
  0.202 * precipitation_suitability_resampled +
  0.067 * Irrigation_Jenks_cl_resampled +
  0.058 * rev_slope_jenks_cl_resampled +
  0.283 * temperature_suitability_resampled +
  0.104 * bio17_jenks_recl +
  0.098 * elevation_suitability_resampled
)
# 🔍 DIAGNOSTIC: Check for NA in suitablemap_WLC
message("🔍 Checking for NA values in suitablemap_WLC:")
print(global(suitablemap_WLC, c("min", "max", "mean", "notNA"), na.rm = FALSE))


minmax <- global(suitablemap_WLC, c("min", "max"), na.rm = TRUE)[,1]

if (any(is.na(minmax))) {
  warning("⚠️ Normalization failed: min or max is NA. Check input rasters and overlay logic.")
  suitablemap_WLC_norm <- suitablemap_WLC  # fallback: no normalization
} else {
  suitablemap_WLC_norm <- (suitablemap_WLC - minmax[1]) / (minmax[2] - minmax[1])
  # Mask to valid input cells
  suitablemap_WLC_norm <- mask(suitablemap_WLC_norm, suitablemap_WLC)
}
# 🔍 DIAGNOSTIC: Check for NA in suitablemap_WLC_norm
message("🔍 Checking for NA values in suitablemap_WLC_norm:")
print(global(suitablemap_WLC_norm, c("min", "max", "mean", "notNA"), na.rm = FALSE))

# Visual check of normalization
plot(suitablemap_WLC_norm, main = "Normalized Suitability Map (0-1)")

# Check dimensions and CRS
# compareGeom(suitablemap_WLC_norm) # (optional: can be commented out)

# Check value range
global(suitablemap_WLC_norm, c("min", "max"), na.rm = TRUE)


# Set the raster layer name for plotting
names(suitablemap_WLC_norm) <- "suitability"

# Use tmap v4 palette
tm_shape(suitablemap_WLC_norm) +
  tm_raster(
    col = "suitability",
    col.scale = tm_scale_continuous(
      values = c4a("brewer.spectral"),
      limits = c(0, 1)
    ),
    col.legend = tm_legend(title = "Suitability")
  ) +
  tm_shape(swiss_boundary) +
  tm_polygons(fill_alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)


# Stack all rasters
stack_all <- c(solar_radiation, precipitation, Irrigation, slope, temperature, elevation, bio17)
# Count cells with no NA in any layer
sum(complete.cases(as.data.frame(values(stack_all))))
summary(values(suitablemap_WLC))
summary(values(suitablemap_WLC_norm))

# Plot normalized suitability map
plot(suitablemap_WLC_norm, main = "Normalized Suitability Map (WLC)")

# Write output rasters
terra::writeRaster(solar_radiation_jenks_recl, "output/rasters/suitability/solar_radiation_jenks_recl.tif", filetype = "GTiff", overwrite = T)
terra::writeRaster(Irrigation_Jenks_cl_resampled, "output/rasters/suitability/Irrigation_Jenks_cl_resampled.tif", filetype = "GTiff", overwrite = T)
terra::writeRaster(rev_slope_jenks_cl_resampled, "output/rasters/suitability/rev_slope_jenks_cl_resampled.tif", filetype = "GTiff", overwrite = T)
terra::writeRaster(precipitation_Jenks_recl_resampled, "output/rasters/suitability/precipitation_Jenks_recl_resampled.tif", filetype = "GTiff", overwrite = T)
terra::writeRaster(temperature_jenks_recl_resampled, "output/rasters/suitability/temperature_jenks_recl_resampled.tif", filetype = "GTiff", overwrite = T)
terra::writeRaster(bio17_jenks_recl, "output/rasters/suitability/bio17_jenks_recl.tif", filetype = "GTiff", overwrite = T)
terra::writeRaster(suitablemap_WLC_norm, "output/rasters/suitability/suitablemap_WLC_norm.tif", filetype = "GTiff", overwrite = T)

# Write suitability rasters for temperature, precipitation, elevation (optional)
terra::writeRaster(temperature_suitability_resampled, "output/rasters/suitability/temperature_suitability_resampled.tif", filetype = "GTiff", overwrite = T)
terra::writeRaster(precipitation_suitability_resampled, "output/rasters/suitability/precipitation_suitability_resampled.tif", filetype = "GTiff", overwrite = T)
terra::writeRaster(elevation_suitability_resampled, "output/rasters/suitability/elevation_suitability_resampled.tif", filetype = "GTiff", overwrite = T)

