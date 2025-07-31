#' Rule-based GIS suitability analysis combining terrain, soil, and climate

# Load required libraries
library(terra)
library(sf)
library(tmap)
library(spatialEco)
library(BAMMtools)
library(cols4all)

# Use official EPSG parameters for GeoTIFF CRS
Sys.setenv(g_tiff_srs_source = "EPSG")

# Helper function: check file existence
check_file <- function(path, label) {
  if (!file.exists(path)) stop(paste0("\u274c ", label, " raster missing!"))
}

# Check raster file existence
check_file("output/rasters/bio12_current_swiss.tif", "Precipitation")
check_file("data/climate/current/bio/CHELSA_bio17_1981-2010_V.2.1.tif", "bio17")
check_file("data/climate/current/bio/CHELSA_rsds_1981-2010_mean_V.2.1.tif", "Solar radiation") # nolint: line_length_linter.
check_file("output/rasters/slope_swiss.tif", "Slope")
check_file("output/rasters/elevation_swiss.tif", "Elevation")
check_file("data/landuse/irrigation_LGRIP30_2015_N40E00_001_2023014175240.tif", "Irrigation") # nolint: line_length_linter.
check_file("output/rasters/bio1_current_swiss.tif", "Temperature")

# Load rasters
load_raster <- function(path, name) {
  r <- rast(path)
  names(r) <- name
  r
}

precipitation <- load_raster("output/rasters/bio12_current_swiss.tif", "precipitation") # nolint: line_length_linter.
bio17 <- load_raster("data/climate/current/bio/CHELSA_bio17_1981-2010_V.2.1.tif", "bio17") # nolint: line_length_linter.
solar_radiation <- load_raster("data/climate/current/bio/CHELSA_rsds_1981-2010_mean_V.2.1.tif", "solar_radiation") # nolint: line_length_linter.
slope <- load_raster("output/rasters/slope_swiss.tif", "slope")
elevation <- load_raster("output/rasters/elevation_swiss.tif", "elevation")
irrigation <- load_raster("data/landuse/irrigation_LGRIP30_2015_N40E00_001_2023014175240.tif", "irrigation") # nolint: line_length_linter. # nolint: line_length_linter.
temperature <- load_raster("output/rasters/bio1_current_swiss.tif", "temperature") # nolint: line_length_linter.

# Ensure CRS matches
project_to <- function(r, target) {
  if (!compareGeom(r, target, stopOnError = FALSE)) {
    if (!is.na(crs(r))) project(r, crs(target)) else stop("\u274c Missing CRS!")
  } else {
    r
  }
}

solar_radiation <- project_to(solar_radiation, precipitation)
slope <- project_to(slope, precipitation)
irrigation <- project_to(irrigation, precipitation)
bio17 <- project_to(bio17, precipitation)
elevation <- project_to(elevation, precipitation)
temperature <- project_to(temperature, precipitation)

# Resample all rasters to common grid
template <- precipitation
solar_radiation <- resample(solar_radiation, template)
slope <- resample(slope, template)
irrigation <- resample(irrigation, template)
bio17 <- resample(bio17, template)
elevation <- resample(elevation, template)
temperature <- resample(temperature, template)

# Boundary mask
swiss_boundary <- st_read("data/boundaries/country_boundary.geojson") |>
  st_transform(crs = crs(precipitation))

mask_crop <- function(r) mask(crop(r, vect(swiss_boundary)), vect(swiss_boundary))

precipitation <- mask_crop(precipitation)
solar_radiation <- mask_crop(solar_radiation)
slope <- mask_crop(slope)
irrigation <- mask_crop(irrigation)
bio17 <- mask_crop(bio17)
elevation <- mask_crop(elevation)
temperature <- mask_crop(temperature)

# Apply Jenks classification to all relevant rasters
jenks_rasters <- list(
  solar_radiation = solar_radiation,
  precipitation = precipitation,
  slope = slope,
  irrigation = irrigation,
  temperature = temperature,
  bio17 = bio17,
  elevation = elevation
)

jenks_classified <- list()

apply_jenks <- function(r, name) {
  df <- as.data.frame(values(r)) |>
    na.omit()
  colnames(df) <- name
  brks <- getJenksBreaks(df[[1]], 10, subset = nrow(df) * 0.01)
  cl <- c(
    0, brks[1], 1,
    brks[1], brks[2], 2,
    brks[2], brks[3], 3,
    brks[3], brks[4], 4,
    brks[4], brks[5], 5,
    brks[5], brks[6], 6,
    brks[6], brks[7], 7,
    brks[7], brks[8], 8,
    brks[8], brks[9], 9,
    brks[9], max(values(r), na.rm = TRUE), 10
  )
  cl_mat <- matrix(cl, ncol = 3, byrow = TRUE)
  classify(r, cl_mat)
}

for (layer_name in names(jenks_rasters)) {
  jenks_classified[[layer_name]] <- apply_jenks(jenks_rasters[[layer_name]], layer_name)
}

# Flip slope for suitability (lower is better)
max_slope <- global(jenks_classified$slope, "max", na.rm = TRUE)[1, 1]
rev_slope <- jenks_classified$slope
values(rev_slope) <- max_slope - values(jenks_classified$slope)

# Gaussian suitability transformations
gaussian_suitability <- function(r, optimal, sd) {
  exp(-((r - optimal)^2) / (2 * sd^2))
}

temperature_suit <- gaussian_suitability(temperature, 16, 3)
precipitation_suit <- gaussian_suitability(precipitation, 700, 150)
elevation_suit <- gaussian_suitability(elevation, 400, 200)

# Resample all suitability layers to a common template
resample_to_template <- function(r) resample(r, jenks_classified$solar_radiation, method = "bilinear")

temperature_suit <- resample_to_template(temperature_suit)
precipitation_suit <- resample_to_template(precipitation_suit)
elevation_suit <- resample_to_template(elevation_suit)
rev_slope <- resample_to_template(rev_slope)
jenks_classified <- lapply(jenks_classified, resample_to_template)

# Weighted Linear Combination (WLC)
suitability_wlc <- (
  0.197 * jenks_classified$solar_radiation +
    0.202 * precipitation_suit +
    0.067 * jenks_classified$irrigation +
    0.058 * rev_slope +
    0.283 * temperature_suit +
    0.104 * jenks_classified$bio17 +
    0.098 * elevation_suit
)

# Normalize
minmax <- global(suitability_wlc, c("min", "max"), na.rm = TRUE)[, 1]
suitability_wlc_norm <- (suitability_wlc - minmax[1]) / (minmax[2] - minmax[1])
suitability_wlc_norm <- mask(suitability_wlc_norm, suitability_wlc)

# Plot
tm_shape(suitability_wlc_norm) +
  tm_raster(
    col = "suitability",
    col.scale = tm_scale_continuous(values = c4a("brewer.spectral"), limits = c(0, 1)),
    col.legend = tm_legend(title = "Suitability")
  ) +
  tm_shape(swiss_boundary) +
  tm_polygons(fill_alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# Save outputs
writeRaster(suitability_wlc_norm, "output/rasters/suitability/suitability_wlc_norm.tif", overwrite = TRUE)
writeRaster(temperature_suit, "output/rasters/suitability/temperature_suit.tif", overwrite = TRUE)
writeRaster(precipitation_suit, "output/rasters/suitability/precipitation_suit.tif", overwrite = TRUE)
writeRaster(elevation_suit, "output/rasters/suitability/elevation_suit.tif", overwrite = TRUE)
writeRaster(rev_slope, "output/rasters/suitability/rev_slope.tif", overwrite = TRUE)
writeRaster(jenks_classified$irrigation, "output/rasters/suitability/irrigation_jenks.tif", overwrite = TRUE)
writeRaster(jenks_classified$bio17, "output/rasters/suitability/bio17_jenks.tif", overwrite = TRUE)
writeRaster(jenks_classified$solar_radiation, "output/rasters/suitability/solar_radiation_jenks.tif", overwrite = TRUE)
