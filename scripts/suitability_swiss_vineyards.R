# ecocrop_suitability_switzerland.R
# Advanced Ecocrop-based climate suitability modelling for Swiss vineyards
#
# This script loads current and future climate data for Switzerland,
# computes suitability for Vitis vinifera based on Ecocrop parameters,
# and exports geospatial outputs and a summary PDF report.
#
# Required packages: terra, sf, dplyr, ggplot2, tmap, rmarkdown
#
# Author: Your Name
# Date: 2024

# ============================
# 1. Package Setup
# ============================
required_packages <- c("terra", "sf", "dplyr", "ggplot2", "tmap", "rmarkdown")
missing_pkgs <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(paste("Missing packages:", paste(missing_pkgs, collapse = ", ")),
       call. = FALSE)
}

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tmap)

# ============================
# 2. Parameter Definitions
# ============================
# Ecocrop thresholds for European wine grapes
params <- list(
  ktmp = c(min_abs = -1, min_opt = 1, max_opt = 1, max_abs = 1), # killing temp used only as min threshold
  tavg = c(min_abs = 10, min_opt = 18, max_opt = 30, max_abs = 38),
  prec = c(min_abs = 49, min_opt = 91, max_opt = 128, max_abs = 195),
  ph   = c(min_abs = 4.5, min_opt = 5.5, max_opt = 7.5, max_abs = 8.5)
)

# Growing season months for Switzerland (April to October)
grow_months <- 4:10

# Weights for each parameter in final suitability (must sum to 1)
weights <- c(tavg = 0.4, prec = 0.4, ph = 0.2)

# ============================
# 3. Utility Functions
# ============================
#' Linear suitability scaling between min_opt and max_opt
#' @param r SpatRaster layer
#' @param p numeric vector of length 4 (min_abs, min_opt, max_opt, max_abs)
linear_suitability <- function(r, p) {
  s <- classify(r, rbind(
    c(-Inf, p["min_abs"], 0),
    c(p["min_abs"], p["min_opt"], "increment"),
    c(p["min_opt"], p["max_opt"], 1),
    c(p["max_opt"], p["max_abs"], "decrement"),
    c(p["max_abs"], Inf, 0)
  ), include.lowest = TRUE, others = NA)

  # Handle increment and decrement using linear scaling
  inc_mask <- r >= p["min_abs"] & r < p["min_opt"]
  dec_mask <- r > p["max_opt"] & r <= p["max_abs"]
  s[inc_mask]  <- (r[inc_mask] - p["min_abs"]) / (p["min_opt"] - p["min_abs"])
  s[dec_mask]  <- (p["max_abs"] - r[dec_mask]) / (p["max_abs"] - p["max_opt"])
  return(s)
}

#' Compute suitability for a list of raster layers
#' @param layers named list of SpatRaster
#' @param params list of parameter vectors
#' @param weights named numeric vector
combine_suitability <- function(layers, params, weights) {
  stopifnot(all(names(weights) %in% names(layers)))
  s_list <- mapply(function(r, p) linear_suitability(r, p), layers, params[names(layers)], SIMPLIFY = FALSE)
  stack_s <- rast(s_list)
  weighted <- stack_s * weights[names(s_list)]
  sum_suit <- app(weighted, fun = sum, na.rm = TRUE)
  sum_suit[sum_suit > 1] <- 1
  return(sum_suit)
}

# ============================
# 4. Data Loading Helpers
# ============================
load_raster_safe <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  rast(path)
}

# ============================
# 5. Data Paths (edit as needed)
# ============================
# Current climate rasters
climate_current <- list(
  tavg = "data/climate/current/bio/CHELSA_bio1_1981-2010_V.2.1.tif",
  prec = "data/climate/current/bio/CHELSA_bio12_1981-2010_V.2.1.tif"
)

# Soil pH raster (static)
soil_ph_path <- "data/soil/soil_ph.tif"

# Future scenarios (SSP3-RCP7 climate as simulated by the GCMs 2071-2100)
climate_future <- list(
  rcp45 = list(
    tavg = "data/climate/future/bio/CHELSA_bio1_2041-2070_rcp45.tif",
    prec = "data/climate/future/bio/CHELSA_bio12_2041-2070_rcp45.tif"
  ),
  rcp85 = list(
    tavg = "data/climate/future/bio/CHELSA_bio1_2041-2070_rcp85.tif",
    prec = "data/climate/future/bio/CHELSA_bio12_2041-2070_rcp85.tif"
  )
)

# Switzerland boundary
boundary_file <- "data/boundaries/swiss_boundary.shp"

# Output folders
output_dir <- "output/rasters"
report_file <- "output/ecocrop_report.pdf"

# Create output directory
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ============================
# 6. Load Data
# ============================
message("Loading boundary…")
ch_boundary <- st_read(boundary_file, quiet = TRUE) |> st_transform(2056)
ch_vect <- vect(ch_boundary)

message("Loading soil pH raster…")
soil_ph <- load_raster_safe(soil_ph_path)
soil_ph <- project(crop(soil_ph, ch_vect) |> mask(ch_vect), "EPSG:2056")

load_climate <- function(paths) {
  lapply(paths, function(p) {
    r <- load_raster_safe(p)
    r <- crop(r, ch_vect) |> mask(ch_vect)
    project(r, "EPSG:2056")
  })
}

message("Loading current climate rasters…")
clim_cur <- load_climate(climate_current)
message("Loading future climate rasters…")
clim_future <- lapply(climate_future, load_climate)

# ============================
# 7. Growing-season Aggregation
# ============================
# Example uses monthly mean rasters 'tavg_{month}.tif' etc. Here we simply use the annual layers.
# Extend here if monthly data available.

# ============================
# 8. Suitability Computation
# ============================
message("Computing suitability for current climate…")
current_layers <- list(
  tavg = clim_cur$tavg,
  prec = clim_cur$prec,
  ph   = soil_ph
)
current_suit <- combine_suitability(current_layers, params, weights)
writeRaster(current_suit, file.path(output_dir, "suitability_current.tif"), overwrite = TRUE)

future_suit <- list()
for (scen in names(clim_future)) {
  message("Computing suitability for ", scen, "…")
  layers <- list(
    tavg = clim_future[[scen]]$tavg,
    prec = clim_future[[scen]]$prec,
    ph   = soil_ph
  )
  future_suit[[scen]] <- combine_suitability(layers, params, weights)
  writeRaster(future_suit[[scen]],
              file.path(output_dir, paste0("suitability_", scen, ".tif")),
              overwrite = TRUE)
}

# ============================
# 9. Mapping
# ============================
message("Generating maps…")

map_fun <- function(r, title) {
  tm_shape(r) + tm_raster(title = title, palette = "YlGn", style = "cont") +
    tm_layout(frame = FALSE)
}

pdf("output/suitability_maps.pdf")
print(map_fun(current_suit, "Current"))
for (scen in names(future_suit)) {
  print(map_fun(future_suit[[scen]], scen))
}
dev.off()

# ============================
# 10. PDF Report
# ============================
message("Rendering PDF report…")
report_rmd <- tempfile(fileext = ".Rmd")
cat(
  "---\ntitle: 'Ecocrop Suitability Switzerland'\noutput: pdf_document\n---\n",
  "\n## Overview\nThis report summarises suitability modelling for Swiss vineyards.",
  file = report_rmd
)

rmarkdown::render(report_rmd, output_file = report_file, quiet = TRUE)
message("Report written to ", report_file)

message("All tasks completed.")