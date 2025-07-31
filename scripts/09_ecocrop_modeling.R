# Ecocrop model evaluation for Vitis vinifera under current and future climate
library(geodata)
library(Recocrop)
library(terra)

# =============================================================================
# STEP 1: GET ECOCROP PARAMETERS FOR EUROPEAN WINE GRAPE
# =============================================================================

# Get the parameters for European wine grape
crop <- Recocrop::ecocropPars("European wine grape")
print("Crop object structure:")
print(crop)

# Create the ecocrop model
m <- Recocrop::ecocrop(crop)
print("Ecocrop model:")
print(m)

# =============================================================================
# STEP 2: EXAMINE PARAMETERS IN DETAIL
# =============================================================================

# Parameter names available in the model
cat("\nParameter names:\n")
print(m$parameter_names)

# Extract the parameter values from the model
params <- m$parameters
names(params) <- m$parameter_names

# Display parameters in a readable format
cat("\n=== ECOCROP PARAMETERS FOR EUROPEAN WINE GRAPE ===\n")
for (i in 1:length(params)) {
  cat(
    "\n", names(params)[i], " (",
    switch(names(params)[i],
      "ktmp" = "Killing Temperature °C",
      "tavg" = "Average Temperature °C",
      "prec" = "Precipitation mm",
      "ph" = "Soil pH",
      names(params)[i]
    ), "):\n"
  )
  cat("  Min absolute (killing/unsuitable):", params[[i]][1], "\n")
  cat("  Min optimal (growth starts):     ", params[[i]][2], "\n")
  cat("  Max optimal (growth ends):       ", params[[i]][3], "\n")
  cat("  Max absolute (killing/unsuitable):", params[[i]][4], "\n")
}

# Create a summary data frame
param_df <- data.frame(
  parameter = m$parameter_names,
  description = c(
    "Killing Temperature (°C)",
    "Average Temperature (°C)",
    "Precipitation (mm)",
    "Soil pH"
  ),
  min_abs = sapply(params, function(x) x[1]),
  min_opt = sapply(params, function(x) x[2]),
  max_opt = sapply(params, function(x) x[3]),
  max_abs = sapply(params, function(x) x[4])
)

cat("\n=== PARAMETER SUMMARY TABLE ===\n")
print(param_df)

# =============================================================================
# STEP 3: VISUALIZE PARAMETER RESPONSE CURVES
# =============================================================================

# Plot the response curves
cat("\nGenerating parameter response curves...\n")
plot(m, main = "European Wine Grape - EcoCrop Parameters")

# =============================================================================
# STEP 4: EXAMINE RAW CROP PARAMETERS (ALTERNATIVE ACCESS)
# =============================================================================

cat("\n=== RAW CROP OBJECT EXAMINATION ===\n")
cat("Crop name:", crop$name, "\n")
cat("Number of parameters:", length(crop$parameters), "\n")

# The crop$parameters contains all 20 values in a vector
# These are typically organized as 4 thresholds for 5 environmental variables
if (length(crop$parameters) == 20) {
  # Organize into matrix (4 thresholds x 5 variables)
  param_matrix <- matrix(crop$parameters, nrow = 4, ncol = 5)
  rownames(param_matrix) <- c("min_abs", "min_opt", "max_opt", "max_abs")
  colnames(param_matrix) <- c("var1", "var2", "var3", "var4", "var5")

  cat("\nAll 20 parameters organized as matrix:\n")
  print(param_matrix)

  cat("\nNote: The model object (m) only uses 4 of these 5 variables.\n")
  cat("The parameters used by the model correspond to columns in this matrix.\n")
}

# Display all raw parameters
cat("\nRaw parameter vector (all 20 values):\n")
print(crop$parameters)

# =============================================================================
# STEP 5: MODEL INFORMATION
# =============================================================================

cat("\n=== MODEL CONFIGURATION ===\n")
cat("Growing season duration:", m$duration, "days\n")
cat("Number of years:", m$nyears, "\n")
cat("Parameters used by model:", paste(m$parameter_names, collapse = ", "), "\n")

# =============================================================================
# STEP 6: SAVE PARAMETERS FOR FUTURE USE
# =============================================================================

# Save parameters as CSV for documentation
write.csv(param_df, "grape_ecocrop_parameters.csv", row.names = FALSE)
cat("\nParameters saved to 'grape_ecocrop_parameters.csv'\n")

# Save the complete model object for later use
saveRDS(m, "grape_ecocrop_model.rds")
cat("Model object saved to 'grape_ecocrop_model.rds'\n")

# =============================================================================
# STEP 7: READY FOR CLIMATE DATA APPLICATION
# =============================================================================

cat("\n=== NEXT STEPS ===\n")
cat("1. Load climate rasters (temperature, precipitation)\n")
cat("2. Use predict(m, climate_data) to calculate suitability\n")
cat("3. The model is now ready for spatial analysis\n")

# Example of how you would use this with climate data:
cat("\n=== EXAMPLE USAGE WITH CLIMATE DATA ===\n")
cat("# Load climate rasters\n")
cat("# temp_raster <- rast('temperature.tif')  # Average temperature\n")
cat("# prec_raster <- rast('precipitation.tif')  # Precipitation\n")
cat("# ph_raster <- rast('soil_ph.tif')  # Soil pH (optional)\n")
cat("#\n")
cat("# Stack the predictors\n")
cat("# climate_stack <- c(temp_raster, prec_raster, ph_raster)\n")
cat("# names(climate_stack) <- c('tavg', 'prec', 'ph')\n")
cat("#\n")
cat("# Calculate suitability\n")
cat("# suitability <- predict(m, climate_stack)\n")
cat("# plot(suitability, main='Grape Suitability')\n")

cat("\n=== SCRIPT COMPLETE ===\n")
