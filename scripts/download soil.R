library(XML)
library(here)
library(sf)

# Optionally use a country shapefile to define a precise ROI
# Read project config to get country code (requires yaml package)
library(yaml)
config <- yaml::read_yaml(here("config.yml"))
country_code <- config$country  # e.g. "CH"


# Directly reference the local country boundary GeoJSON
shp_path <- here("data", "boundaries", "country_boundary.geojson")
if (!file.exists(shp_path)) {
  stop("Country boundary file not found at: ", shp_path)
}
shp <- st_read(shp_path, quiet = TRUE)

# Transform to match WCS CRS
shp <- st_transform(shp, crs = 4326)
# Derive bounding box from shapefile
b <- st_bbox(shp)
# GDAL expects projwin = c(minX, maxY, maxX, minY)
bb <- c(b["xmin"], b["ymax"], b["xmax"], b["ymin"])

# Set variable of interest: SoilGrids soil property code
voi <- "phh2o" # Soil pH (H2O)

# Standard depth intervals and metrics
depths <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
metrics <- c("mean", "Q0.05", "Q0.5", "Q0.95", "uncertainty")

# Choose first depth and metric for initial download
depth <- depths[1]   # "0-5cm"
metric <- "mean"    # "mean"

# Construct the coverage layer name dynamically
voi_layer <- paste(voi, depth, metric, sep = "_")  # e.g., "phh2o_0-5cm_mean"

# Path to the WCS. See maps.isric.org
wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map")
wcs_service = "SERVICE=WCS"
wcs_version = "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.

# Define region of interest bounding box (minX, maxY, maxX, minY) and SRS
# bb is now defined from shapefile above
projwin_srs <- "EPSG:4326"     # coordinate reference system for projwin

# Example 1: Describe the coverage layer
wcs_request = "DescribeCoverage" 

wcs = paste(wcs_path, wcs_service, wcs_version, wcs_request, sep="&")

# we create a XML that can be used with the gdalinfo utility after being saved to disk:
l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
l1.l <- newXMLNode("CoverageName", voi_layer, parent=l1)

# Save to local disk
xml.out = "./sg.xml"
saveXML(l1, file = xml.out)

# Example 2: Download a Tiff for a region of interest (ROI)
wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3

l1 <- newXMLNode("WCS_GDAL")
l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
l1.l <- newXMLNode("CoverageName", voi_layer, parent=l1)

# Save to local disk
xml.out = "./sg.xml"
saveXML(l1, file = xml.out)

# Define output filename based on layer name
file.out <- here::here("output/rasters", paste0(voi_layer, ".tif"))


# Download raster as GeoTIFF (Warning: it can be large!)
sf::gdal_utils(
  util        = "translate",
  source      = xml.out,
  destination = file.out,
  options     = c(
    "-tr", "250", "250",
    "-projwin", bb[1], bb[2], bb[3], bb[4],
    "-projwin_srs", projwin_srs,
    "-co", "TILED=YES", "-co", "COMPRESS=DEFLATE",
    "-co", "PREDICTOR=2", "-co", "BIGTIFF=YES"
  ),
  config_options = c("GDAL_HTTP_UNSAFESSL=YES")
)

# Clip to country shapefile boundary
cropped <- here::here("output/rasters", paste0(voi_layer, "_", country_code, ".tif"))
sf::gdal_utils(
  util        = "warp",
  source      = file.out,
  destination = cropped,
  options     = c(
    "-cutline", shp_path,
    "-crop_to_cutline",
    "-co", "TILED=YES", "-co", "COMPRESS=DEFLATE",
    "-co", "PREDICTOR=2", "-co", "BIGTIFF=YES"
  )
)
