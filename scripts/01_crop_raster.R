# 01_load_and_prepare_data.R
# --------------------------
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tmap)
library(fs)
library(glue)
library(here)

# Paths
raster_dir <- here("data", "landuse") # Change to other folder if needed
boundary_path <- here("data", "boundaries", "country_boundary.geojson") # this is for che, change for other countries

# Load boundary
glue("Reading boundary from: {boundary_path}") |> message()
country_boundary <- st_read(boundary_path, quiet = TRUE)
dplyr::glimpse(country_boundary)
country_vect <- vect(country_boundary)
country_crs <- crs(country_vect)

# List all raster files
raster_files <- fs::dir_ls(raster_dir, glob = "*.tif")

skipped <- c()
for (f in raster_files) {
  glue("Processing raster: {f}") |> message()
  r <- rast(f)
  raster_crs <- crs(r)
  glue("Raster CRS: {raster_crs}") |> message()
  glue("Boundary CRS: {country_crs}") |> message()
  if (terra::same.crs(r, country_vect)) {
    result <- tryCatch({
      r_crop <- crop(r, country_vect) |> mask(country_vect)
      # Neuer Dateiname mit in dem Fall _CHE vor .tif / sonst ändern
      f_che <- sub("\\.tif$", "_CHE.tif", f)
      writeRaster(r_crop, f_che, overwrite = TRUE)
      # Datei in den macOS Trash verschieben statt löschen
      system(glue('mv "{f}" ~/.Trash/'))
      glue("Cropped, saved as: {f_che}, original moved to Trash.") |> message()
      NULL
    }, error = function(e) {
      glue("Fehler beim Cropping/Masking: {f}\n{e$message}") |> message()
      skipped <<- c(skipped, f)
    })
  } else {
    glue("CRS mismatch! Skipping raster: {f}") |> message()
    skipped <- c(skipped, f)
  }
}

glue("All rasters checked for CRS, cropped, and saved in {raster_dir}.") |> message()
if (length(skipped) > 0) {
  glue("Folgende Dateien wurden übersprungen (CRS mismatch):\n{paste(skipped, collapse = '\n')}") |> message()
} else {
  glue("Alle Dateien wurden erfolgreich gecropped.") |> message()
}

