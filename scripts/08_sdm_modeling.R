# species_distribution_modeling.R
# -------------------------------
# Species distribution modeling (MaxEnt / Random Forest)
# Binary reclassification of environmental layers

# Load required libraries
library(terra)
library(sf)
library(tmap)

# Helper: reclassify a raster and plot it
reclassify_and_plot <- function(
  raster, 
  layer_name, 
  breaks, 
  palette, 
  labels,
  boundary
) {
  # Create reclassification matrix
  mat <- matrix(breaks, ncol = 3, byrow = TRUE)
  # Apply reclassification
  recl <- reclassify(raster, mat)
  # Plot
  tm_shape(recl) +
    tm_raster(
      style = "cat",
      title = layer_name,
      palette = palette,
      labels = labels
    ) +
    tm_shape(boundary) +
    tm_polygons(alpha = 0, border.col = "black") +
    tm_layout(frame = FALSE, legend.outside = TRUE)
  invisible(recl)
}

# Read boundary shapefile
kenya_states <- st_read(
  "data/boundaries/kenya_states.geojson",
  quiet = TRUE
)

# Ensure boundary is in native CRS
boundary_vect <- vect(
  st_transform(kenya_states, crs = crs(temp))
)

# 1. Temperature (°C)
# threshold: < 15 = unsuitable (0), ≥ 15 = suitable (1)
temp_recl <- reclassify_and_plot(
  raster = temp,
  layer_name = "Temperature",
  breaks = c(
    -Inf, 15, 0,
    15, Inf, 1
  ),
  palette = c("grey", "#F1948A"),
  labels = c("Unsuitable (<15.0)", "Suitable (≥15.0)"),
  boundary = kenya_states
)

# 2. Elevation (m)
# threshold: ≤ 1200 = suitable (1), > 1200 = unsuitable (0)
elev_recl <- reclassify_and_plot(
  raster = elev,
  layer_name = "Elevation",
  breaks = c(
    -Inf, 1200, 1,
    1200, Inf, 0
  ),
  palette = c("orange", "grey"),
  labels = c("Suitable (≤1200m)", "Unsuitable (>1200m)"),
  boundary = kenya_states
)

# 3. NDVI (vegetation)
# threshold: ≤ 0.5 = unsuitable (0), > 0.5 = suitable (1)
ndvi_recl <- reclassify_and_plot(
  raster = nvdi,
  layer_name = "NDVI",
  breaks = c(
    -Inf, 0.5, 0,
    0.5, Inf, 1
  ),
  palette = c("grey", "green"),
  labels = c("Unsuitable (≤0.5)", "Suitable (>0.5)"),
  boundary = kenya_states
)

# 4. Precipitation (mm)
# threshold: ≤ 350 = unsuitable (0), > 350 = suitable (1)
prec_recl <- reclassify_and_plot(
  raster = prec,
  layer_name = "Precipitation (mm)",
  breaks = c(
    -Inf, 350, 0,
    350, Inf, 1
  ),
  palette = c("grey", "skyblue"),
  labels = c("Unsuitable (≤350mm)", "Suitable (>350mm)"),
  boundary = kenya_states
)

# 5. Population density (persons)
# threshold: 0 = unsuitable, ≥1 = suitable\pop_recl <- reclassify_and_plot(
  raster = population,
  layer_name = "Population",
  breaks = c(
    -Inf, 0, 0,
    0, Inf, 1
  ),
  palette = c("grey", "orange"),
  labels = c("Unsuitable (0 persons)", "Suitable (≥1 person)"),
  boundary = kenya_states
)

# 6. Aridity index
# threshold: ≤ 0.2 = unsuitable (0), > 0.2 = suitable (1)
arid_recl <- reclassify_and_plot(
  raster = aridity,
  layer_name = "Aridity",
  breaks = c(
    -Inf, 0.2, 0,
    0.2, Inf, 1
  ),
  palette = c("grey", "orange"),
  labels = c("Unsuitable (≤0.2)", "Suitable (>0.2)"),
  boundary = kenya_states
)

# 7. Binary combined suitability
suitable_lf_binary <-
  temp_recl * ndvi_recl * prec_recl * elev_recl * pop_recl * arid_recl

# Plot combined binary suitability
tm_shape(suitable_lf_binary) +
  tm_raster(
    style = "cat",
    palette = c("#f0f0f0", "red"),
    labels = c("Not Suitable", "Highly Suitable")
  ) +
  tm_shape(kenya_states) +
  tm_polygons(alpha = 0, border.col = "black") +
  tm_text("NAME_1", size = "AREA") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# 8. Summed suitability score
layers_stack <- c(
  temp_recl,
  ndvi_recl,
  prec_recl,
  elev_recl,
  pop_recl,
  arid_recl
)
summed_score <- app(layers_stack, sum)

# Plot summed score
tm_shape(summed_score) +
  tm_raster(
    style = "cat",
    palette = c(
      "#FDFEFE", "#FADBD8",
      "#F5B7B1", "#F1948A",
      "#E74C3C"
    ),
    labels = c(
      "Low (2)", "Modest (3)",
      "Medium (4)", "High (5)",
      "Highest (6)"
    )
  ) +
  tm_shape(kenya_states) +
  tm_polygons(alpha = 0, border.col = "black") +
  tm_text("NAME_1", size = "AREA") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# --- Burkina Faso millet modeling ---
# Load libraries
library(geodata)

# Load vector data
bf_shp <- st_read("H:/Datasets/burkina_faso.shp", quiet = TRUE)
bf_vect <- vect(bf_shp)

# Read presence/absence data and convert to SpatVector
data_df <- read.csv("H:/Datasets/Burkina_data.csv")
coords_sf <- st_as_sf(
  data_df,
  coords = c("long", "lat"),
  crs = 4326
)
df_vect <- vect(coords_sf)

# Download climate and soil
clim_curr <- worldclim_global(var = "bio", res = 10, path = "./data")
soil_ph   <- soil_world(var = "phh2o", depth = 15, res = 10, path = "./data")

# Crop and resample to Burkina Faso extents
clim_crop <- crop(clim_curr, bf_vect)
soil_crop <- crop(soil_ph, bf_vect)
clim_res  <- resample(clim_curr, soil_crop)

# Extract values at presence points
clim_vals <- extract(clim_res, df_vect)
soil_vals <- extract(soil_crop, df_vect)

# Combine data
df_combined <- cbind(data_df, clim_vals, soil_vals)

# Rename predictors
names(df_combined)[names(df_combined) == "wc2.1_10m_bio_1"]  <- "temp"
names(df_combined)[names(df_combined) == "wc2.1_10m_bio_12"] <- "precip"
names(df_combined)[names(df_combined) == "phh2o_5-15cm"]        <- "ph"

# Fit logistic regression model
glm_model <- glm(
  presence ~ temp + precip + ph,
  data = df_combined,
  family = binomial
)

# Predict probability raster
predictors <- c(
  clim_res[[c("bio01", "bio12")]],
  soil_crop[["phh2o_5-15cm"]]
)
names(predictors) <- c("temp", "precip", "ph")
prob_raster <- predict(predictors, glm_model, type = "response")
plot(prob_raster, main = "Millet Presence Probability")

# Future climate scenario
clim_future <- cmip6_world(
  var   = "bio",
  res   = 10,
  ssp   = "585",
  model = "BCC-CSM2-MR",
  time  = "2041-2060",
  path  = "./data"
)
clim_f_crop <- crop(clim_future, bf_vect)
clim_f_res  <- resample(clim_f_crop, soil_crop)

future_preds <- c(
  clim_f_res[[c("bio01", "bio12")]],
  soil_crop[["phh2o_5-15cm"]]
)
names(future_preds) <- c("temp", "precip", "ph")
future_prob <- predict(future_preds, glm_model, type = "response")
plot(future_prob, main = "Future Millet Probability")

