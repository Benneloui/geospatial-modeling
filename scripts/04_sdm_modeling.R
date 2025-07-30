# Species Distribution Modeling using MaxEnt or Random Forest
We should use the aforementioned thresholds to produce binary or Boolean maps using the above criteria. This means that the pixel values of each raster layer will be equal to 0 indicating that its an unsuitable condition for LF transmission, and 1 for suitable conditions for LF transmission.

Let us reclassify each layer according to the above criteria, starting with temperature:

# reclassify temperature as a binary or Boolean layer
temp
# lowest value = 1.2
# highest value = 29.6
# reclassify anything below 15 as 0, and the rest above 15 as 1

# reclassify the values into two groups 
# all values > 0 and <= 15 change to 0
# all values > 15 and <= 30 change to 1
temp_cl <- c(0, 15, 0, 
                        15, 30, 1)

# convert into a matrix format
temp_cl_mat <- matrix(temp_cl, ncol = 3, byrow = TRUE)
# see matrix
temp_cl_mat
# apply matrix to reclassify() function to categorize the raster accordingly
temp_recl <- reclassify(temp, temp_cl_mat)
When you reclassify the raster for temp to temp_recl. This is what the output should look like:

tm_shape(temp_recl) + tm_raster(style = "cat", title = "Temperature", palette= c("grey", "#F1948A"), labels = c("Unsuitable (<15.0)", "Suitable (15 & above)")) +
    tm_shape(kenya_states) + tm_polygons(alpha = 0, border.col = "black") +
    tm_layout(frame = FALSE, legend.outside = TRUE)



    Lets repeat the same steps for elevation to reclassify it’s layer according to the given criteria:

# reclassify elevation as a binary or Boolean layer
elev
# lowest value = -11m (below sea level)
# highest value = 4865m (above sea level)
# reclassify anything below 1200m as 1, and the rest above 1200m as 0

# all values > -12 and <= 1199.999 change to 0
# all values > 1199.999 and <= 4900 change to 1
elev_cl <- c(-12, 1199.999, 1, 
                            1199.999, 4900, 0)

# convert into a matrix format
elev_cl_mat <- matrix(elev_cl, ncol = 3, byrow = TRUE) 
# see matrix
elev_cl_mat
# apply matrix to reclassify() function to categorize the raster accordingly
elev_recl <- reclassify(elev, elev_cl_mat)
The elevation output should look something like:

tm_shape(elev_recl) + tm_raster(style = "cat", title = "Elevation", palette= c("grey", "orange"), labels = c("Unsuitable (>1200m)", "Suitable (1200m & below)")) +
    tm_shape(kenya_states) + tm_polygons(alpha = 0, border.col = "black") +
    tm_layout(frame = FALSE, legend.outside = TRUE)


    # nvdi
nvdi

nvdi_cl <- c(-1, 0.5, 0, 
                        0.5, 0.9, 1)

nvdi_cl_mat <- matrix(nvdi_cl, ncol = 3, byrow = TRUE)
nvdi_cl_mat
nvdi_recl <- reclassify(nvdi, nvdi_cl_mat)

tm_shape(nvdi_recl) + tm_raster(style = "cat", title = "NDVI (Vegetation)", palette= c("grey", "green"), labels = c("Unsuitable (0.5 & Below)", "Suitable (> 0.5)")) +
    tm_shape(kenya_states) + tm_polygons(alpha = 0, border.col = "black") +
    tm_layout(frame = FALSE, legend.outside = TRUE)

# prec
prec

prec_cl <- c(0, 350, 0, 
                        350, 2700, 1)

prec_cl_mat <- matrix(prec_cl, ncol = 3, byrow = TRUE) 
prec_cl_mat

prec_recl <- reclassify(prec, prec_cl_mat)

tm_shape(prec_recl) + tm_raster(style = "cat", title = "Precipitation (mm)", palette= c("grey", "skyblue"), labels = c("Unsuitable (350mm & Below)", "Suitable (> 350mm)")) +
    tm_shape(kenya_states) + tm_polygons(alpha = 0, border.col = "black") +
    tm_layout(frame = FALSE, legend.outside = TRUE)

# popl
popl

popl_cl <- c(-1, 0, 0, 
                        0, 126300, 1)

popl_cl_mat <- matrix(popl_cl, ncol = 3, byrow = TRUE)
popl_cl_mat
popl_recl <- reclassify(popl , popl_cl_mat)

tm_shape(popl_recl) + tm_raster(style = "cat", title = "Population density", palette= c("grey", "orange"), labels = c("Unsuitable (0 people)", "Suitable (at least 1 person)")) +
    tm_shape(kenya_states) + tm_polygons(alpha = 0, border.col = "black") +
    tm_layout(frame = FALSE, legend.outside = TRUE)

# arid
arid

arid_cl <- c(0, 0.20, 0, 
                        0.20, 3, 1)

arid_cl_mat <- matrix(arid_cl, ncol = 3, byrow = TRUE)
arid_cl_mat
arid_recl <- reclassify(arid, arid_cl_mat)

tm_shape(arid_recl) + tm_raster(style = "cat", title = "Aridity (Dryness)", palette= c("grey", "orange"), labels = c("Unsuitable (0.2 & below)", "Suitable (> 0.2)")) +
    tm_shape(kenya_states) + tm_polygons(alpha = 0, border.col = "black") +
    tm_layout(frame = FALSE, legend.outside = TRUE)



    Suitable_LF_Binary <- temp_recl*nvdi_recl*prec_recl*elev_recl*popl_recl*arid_recl
Visualizing the output:

tm_shape(Suitable_LF_Binary) + tm_raster(style = "cat", title = "", palette=c("#f0f0f0", "red"), labels=c("Zone: Not Suitable", "Zone: Highly Suitable")) +
    tm_shape(kenya_states) + tm_polygons(alpha = 0, border.col = "black") + tm_text("NAME_1", size = "AREA") +
    tm_layout(frame = FALSE, legend.outside = TRUE)


    RasterStack <- stack(temp_recl, nvdi_recl, prec_recl, elev_recl, popl_recl, arid_recl)
Suitable_LF_Summed <- calc(RasterStack, sum)
# check for minimum and maximum
Suitable_LF_Summed@data@min
Suitable_LF_Summed@data@max
# minimum = 2
# maximum = 6


tm_shape(Suitable_LF_Summed) + tm_raster(style = "cat", title = "Suitability score", palette=c("#FDFEFE", "#FADBD8", "#F5B7B1", "#F1948A", "#E74C3C"), labels=c("Low (2)", "Modest (3)", "Medium (4)", "High (5)", "Highest (6)")) +
    tm_shape(kenya_states) + tm_polygons(alpha = 0, border.col = "black") + tm_text("NAME_1", size = "AREA") +
    tm_layout(frame = FALSE, legend.outside = TRUE)




    # --- Script ---

#install.packages("maps")
library(terra)
library(geodata)
library(sf)
library(sp)
burkina_faso = read_sf("H:/Datasets/burkina_faso.shp")
plot(st_geometry(burkina_faso))
#convert spatial object to terra SpatVector
burkina_faso_vect <- vect(burkina_faso)
# Read the millet presence/absence dataset and convert it into Dataframe
data <- read.csv("H:/Datasets/Burkina_data.csv")
df <- data.frame(data)
#Check th first rows of the dataset
head(df)
# Plot the coordintes of the millet/cultivated and non-cultivated areas on the map
points(x = df$long, y = df$lat, col = "Red",pch = 19, cex = 0.5)
#convert dataframe to spatial object
coordinates(df) <- ~long+lat
#Assign a coodinate reference system to the data

crs(df) <- "+proj=longlat +datum=WGS84"
df_vect <- vect(df)  # convert sp object to terra SpatVector

#Download cimate and soil variables
# Current climate variables at 10 arc-minutes (~18 km)
clim_current <- worldclim_global(var = "bio", res = 10,path=".")
clim_current
# Soil pH at 0-5cm depth
soil_ph <- soil_world(var = "phh2o", depth = 15, res = 10, path=".")
soil_ph
#First mask the climatedata to the Burkina Faso extents
plot(soil_ph)
clim_current_selected<-mask(clim_current,burkina_faso)
soil_ph_selected<-mask(soil_ph, burkina_faso)
clim_current_selected
soil_ph_selected
#resample them to the Burkina Faso extents
clim_current_selected_resampled<-crop(clim_current_selected,burkina_faso_vect)
soil_ph_selected_resampled<-crop(soil_ph_selected,burkina_faso_vect)

#Check the resampled data
clim_current_selected_resampled
soil_ph_selected_resampled

#Finally resample the climate and the soil_ph data to the simlar extents and pixel sizes

clim_current_resampled <- resample(clim_current, soil_ph_selected_resampled)

#Check now the new resampled layers and ensure thay have the same extents and pixel sizes

clim_current_resampled
soil_ph_selected_resampled

plot(soil_ph_selected_resampled)
# We now need to extract the climate variables and append to our initial data frame

clim_vals <- extract(clim_current_resampled, df_vect)
soil_vals <- extract(soil_ph_selected_resampled, df_vect)


# Combine all data
df_combined <- cbind(df_vect, clim_vals, soil_vals)
head(df_combined)
names(df_combined)

# Rename specific variables
names(df_combined)[names(df_combined) == "wc2.1_10m_bio_1"] <- "temp"
names(df_combined)[names(df_combined) == "wc2.1_10m_bio_12"] <- "precip"
names(df_combined)[names(df_combined) == "phh2o_5-15cm"] <- "ph"
names(df_combined)
#Build your glm model (we shall use only 3 predictors, annual temperature, annual precipitation and ph)

glm_model <- glm(presence ~ temp + precip + ph, data = df_combined, family = binomial)
glm_model
summary(glm_model)

#Let us take the exponent of the coefficient values to get the odd ratios

exp(coefficients(glm_model)[2])

#For every degree increase in temperature the odds ratio of millet presence increases on average by a constant factor of 2.488239    (or 148.8239 %)

#Precipitation

exp(coefficients(glm_model)[3])

#For every mm increase in precipitation the odds ratio of millet presence increases on average by a constant factor of 0.9978393   (or decreases by 0.2167 %)

exp(coefficients(glm_model)[4])

#For every unit increase in ph the odds ratio of millet presence increases on average by a constant factor of 3.080137     (or 2.080137   %)

#Rasterize to show the spatial explicit suitability surface

names(clim_current_resampled)
names(soil_ph_selected_resampled)

predictors <- c(clim_current_resampled[[c("wc2.1_10m_bio_1", "wc2.1_10m_bio_12")]], soil_ph_selected_resampled[["phh2o_5-15cm"]])
names(predictors)
# Rename specific variables
names(predictors)[names(predictors) == "wc2.1_10m_bio_1"] <- "temp"
names(predictors)[names(predictors) == "wc2.1_10m_bio_12"] <- "precip"
names(predictors)[names(predictors) == "phh2o_5-15cm"] <- "ph"

names(predictors)

millet_prob_raster <- predict(predictors, glm_model, type = "response")
plot(millet_prob_raster)
Burkina_Faso_mask <- vect(countries)

millet_prob_raster <- mask(millet_prob_raster, Burkina_Faso_mask)

#Plot the millet probability map for Burkina_Faso Under the current climate
plot(millet_prob_raster, main = "Predicted Probability of Millet Presence")
millet_prob_raster

#Task - Modify the code to plot the millet probability under future climates and compare the results

# Future climate scenario (e.g., 2050, RCP 8.5, model = "BCC-CSM2-MR")
clim_future <- cmip6_world(var = "bio", res = 10, ssp = "585", model = "BCC-CSM2-MR", time='2041-2060',path=".")
names(clim_future)

clim_future_selected<-crop(clim_future,burkina_faso_vect)
clim_future_resampled <- resample(clim_future_selected, soil_ph_selected_resampled)

names(clim_future_resampled)


future_predictors <- c(clim_future_resampled[[c("bio01", "bio12")]], soil_ph_selected_resampled[["phh2o_5-15cm"]])
names(future_predictors)
# Rename specific variables
names(future_predictors)[names(future_predictors) == "bio01"] <- "temp"
names(future_predictors)[names(future_predictors) == "bio12"] <- "precip"
names(future_predictors)[names(future_predictors) == "phh2o_5-15cm"] <- "ph"

names(future_predictors)

future_millet_prob_raster <- predict(future_predictors, glm_model, type = "response")
plot(future_millet_prob_raster)