# Universal Geospatial Modeling Pipeline 🌍

A modular, reusable pipeline for climate suitability and environmental analysis — fully parameterized and easily adaptable for any region or crop.

---

## 🔍 Overview

This R-based pipeline supports automated environmental modeling for climate suitability, crop distribution, and species habitat analysis. Developed as a prototype using Switzerland and viticulture (wine grapes), it is designed to scale to any country and crop type with minimal modification.

---

## 📁 Project Structure
universal-geospatial-modeling/
├── data/                    # Environmental input data (climate, elevation, crops)
│   ├── climate
│   │   ├── future
│   │   ├── current
│   ├── elevation/
│   ├── soil/
│   └── crops/
├── output/                 # Processed rasters and results
│   └── rasters/
├── scripts/                # Modular R scripts
│   ├── 01_load_and_prepare_data.R
│   └── …
├── config.yml              # User-defined parameters
└── README.md               # Project documentation

---

## ⚙️ Configuration

All user input is centralized in `config.yml`. Set the following:

```yaml
country: CH
crop: grape
scenario: ssp245
climate_vars: [1, 12]
boundary_source: geoboundaries
boundary_file: boundaries/swiss_boundary.shp
elevation_file: SRTM_CH.tif
crop_data:
  path: crops/vineyards
  filename: vineyards.shp
```

You can switch countries, crops, and climate scenarios without modifying the R code.

The script will:
- Automatically download missing elevation and CHELSA climate data
- Read or download boundary files
- Load and process crop shapefiles
- Crop and mask rasters to the country outline 
- Export results to output/rasters/
    

---

## **🌐 Data Sources Used**

- **Climate**: [CHELSA v1.2](https://chelsa-climate.org)
- **Elevation**: [SRTM via OpenTopography/elevatr](https://www.opentopography.org) 
- **Boundaries**: [GeoBoundaries](https://www.geoboundaries.org) 
- **Crop Layers**: [FAO GLC-SHARE](https://www.fao.org/geonetwork), [Copernicus Global Land Cover](https://land.copernicus.eu), or [ESA WorldCereal](https://worldcereal.org)
    
---

## **📦 Goals & Features**

✅ Fully modular and parameterized
✅ Compatible with any country and crop
✅ Climate scenario ready (e.g., SSP2-4.5, SSP5-8.5)
✅ Designed for future web integration or AI wrapping
✅ Built with scientific transparency and reproducibility in mind

---

## **📘 Future Steps**

- Add modeling script for suitability prediction (e.g., SDM / thresholding / Ecocrop logic)  
- Visualize outputs with tmap or leaflet
- Wrap as Shiny app or publish via R Markdown report
- Export results for academic publication or stakeholder use
    

---

## **🧑‍🏫 Author & Credits**

Developed by [Your Name]
Institute for Geography
[Your University], 2025

  
Open-source under MIT License