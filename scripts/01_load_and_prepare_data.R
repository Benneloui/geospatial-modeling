# 01_load_and_prepare_data.R
# --------------------------
# 1) read config and libs
library(yaml)
library(terra)
library(sf)
library(glue)
library(here)


# read project config
cfg <- read_yaml("config.yml")

# build full paths
data_dir  <- cfg$data_dir
proc_dir  <- cfg$proc_dir
bfile     <- file.path(data_dir, cfg$boundary_file)
elev_file <- file.path(data_dir, cfg$elevation_file)
bio_pattern <- cfg$bioclim_current_pattern  # e.g. "data/climate/current/bio/CHELSA_{var}_1981-2010_…"
bio_pattern <- as.character(cfg$bioclim_current_pattern)

# 2) load Switzerland boundary
swiss <- st_read(bfile) %>%
  st_transform(crs = 2056)   # EPSG:2056 for LV95

# 3) load vineyard occurrence points
#    expecting columns: lon, lat, presence
pts <- read.csv("data/vineyard_inspection.csv") 
pts_sf <- st_as_sf(pts,
                   coords = c("lon","lat"),
                   crs = 2056) %>%         # matches your sample coords
  st_crop(swiss)

# 4) load environmental rasters
# 4a) elevation
elev <- rast(elev_file)

# 4b) one bioclim variable (e.g. bio1)
#     here we pull var = "bio1" — later loop over all of c("bio1","bio12",…)
var <- "bio1"
bio1_file <- glue(bio_pattern, var = var)
bio1_file <- glue(bio_pattern, var = var, .open = "{", .close = "}")
bio1_file <- glue("data/climate/current/bio/CHELSA_{var}_1981-2010_V.2.1.tif")
print(bio1_file)
bio1 <- rast(bio1_file)

# 4c) Optimize processing by first cropping in the rasters’ native CRS,
#     then reprojecting the smaller subset to Switzerland’s CRS (EPSG:2056).
#
# Step 1: Identify the original CRS of the elevation raster.
#         This CRS tells us how raster cells map to real-world coordinates.
orig_crs <- terra::crs(elev)

# Step 2: Transform the Swiss boundary polygon into this original raster CRS.
#         We need a spatial object in the same CRS to perform accurate cropping.
swiss_orig <- st_transform(swiss, crs = orig_crs)
swiss_orig_vec <- terra::vect(swiss_orig)

# Step 3: Crop and mask the elevation and bio1 rasters using the transformed Swiss boundary.
#         Cropping reduces the raster extent to Switzerland only,
#         and masking sets all pixels outside the boundary to NA.
elev_cropped <- crop(elev, swiss_orig_vec) |> mask(swiss_orig_vec)
bio1_cropped <- crop(bio1, swiss_orig_vec) |> mask(swiss_orig_vec)

# Step 4: Convert the original Swiss boundary back to a terra SpatVector in EPSG:2056.
#         This defines the target CRS for our final output rasters.
swiss_vec <- terra::vect(swiss)
target_crs <- terra::crs(swiss_vec)

# Step 5: Reproject the cropped rasters from their native CRS to EPSG:2056.
#         Because we cropped first, reprojection runs much faster on smaller data.
elev_ch <- terra::project(elev_cropped, target_crs)
bio1_ch <- terra::project(bio1_cropped, target_crs)

# 5.5) Align raster extents and resolution
# We use the elevation raster as the template,
# and resample the bio1 raster to match its grid.
bio1_ch <- terra::resample(bio1_ch, elev_ch, method = "bilinear")

# 6) stack into a single SpatRaster
env_stack <- c(elev_ch, bio1_ch)
names(env_stack) <- c("elevation", var)

# save for downstream scripts
dir.create(file.path(proc_dir, "rasters"), recursive = TRUE, showWarnings = FALSE)
writeRaster(env_stack,
            filename = file.path(proc_dir, "rasters/env_current.tif"),
            overwrite = TRUE)

# quick check
print(env_stack)
plot(env_stack)



# Convert vineyards_simplified.gpkg to GeoJSON
vineyards <- st_read("data/crops/vineyards/vineyards_simplified.gpkg", quiet = TRUE)
st_write(vineyards, "data/crops/vineyards/vineyards_simplified.geojson", delete_dsn = TRUE, quiet = TRUE)

# Convert to Shapefile
st_write(vineyards, "data/crops/vineyards/vineyards_simplified.shp", delete_dsn = TRUE, quiet = TRUE)
