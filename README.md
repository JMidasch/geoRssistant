# WIP R-Package

R-Package that adds various quality-of-life functions for working with geodata to R.

## Installation Guide

The package was developed with R version 4.4.3 on Windows. Mac & Linux haven't been tested.
Use the following code to install and load the package:

```R
library(devtools)
install_github("JMidasch/geoRssistant")
library(geoRssistant)
```

## Currrently included functions:

---

### Extract raw sensor data from DJI thermal images

```R 
djithermal.convert(input_dir, output_dir, height=512, width=640)
```

#### Problem:

The actual thermal data of thermal images taken by DJI hardware such as the Mavic 3T is encrypted in the rJPGs metadata and can't be accessed directly without relying on the blackbox DJI thermal SDK.

#### Solution:

This function extracts the thermal data from all images in a directory and saves it to individual tifs in another directory while preserving the metadata necessary for further processing in photogrammetry software. This allows the user to either work with the raw sensor data directly or to apply their own custom calibration.

---

### Create overview map

```R 
map.aoi(aoi, basemap = NULL, dem_raster = NULL, dem_z = 14, map_crs = NULL, map_ext = 1, watermask = TRUE, aoi_colour = "#ff0000", landscape = NULL, aspectratio = 16/9)
```
  
#### Problem:
  
For scientific papers, presentations etc. you always need a map to show where your research area is located. However, this can be very time consuming, especially if you want a consistent esthetic for different projects.

#### Solution:
  
This function creates a simple, usable overview map for a research area just based on a polygon. If needed there are also a lot of additional customization options.

---


### Merge Raster Tiles

```R 
rastertile.merge(input_dir = ".", pattern, input_ext, epsg ="25832", unzip = TRUE, remove_old = FALSE)
```
  
#### Problem:
  
When you download geodata from a governmental open geodata portal you often have to download several individual tiles, often in the form of individual zip or tar.gz archives. Unzipping every single archive and merging all the raster files so you can actually work with them takes way to much time.

#### Solution:
  
This function unzips all zip and tar.gz archives in a folder and merges all raster files they contain.

---

### Create a Relative Elevation Model (REM)

```R
river.rem(input_raster, centerline = NULL, seed_point = NULL, output_dir, output_pattern = "riverdem", river_width = 10, overwrite = FALSE)
```

#### Problem:

Relative Elevation Models (REM) can not only help in the exploration of the microtopography around a river and reveal it's paleochannels, they can also look really really cool. However, the workflow to create them in a GIS can be very timeconsuming.

#### Solution:

This function takes a Digital Elevation Model and a river centerline to create a REM. If no centerline is provided it will be generated automatically based on a user-provided seed point.
Known issue: The function might get stuck while automatically generating the centerline.

This workflow is heavily based on [the tutorial by Daniel Coe](https://dancoecarto.com/creating-rems-in-qgis-the-idw-method).

---

### Sort QGIS Layer Files

```R
qgis.filesort(input_path, output_dir ="", remove_old = FALSE)
```
#### Problem:
  
When you work on a QGIS project for a extended period of time and don't keep a close eye on your file structure, you might lose track of which files are actually needed for the project and where they are. If you then want to send the project to another person or want to back it up somewhere it's quite a hassle to organize all the data in a compact way.
  
#### Solution:
  
This function takes a QGIS project as input and copies it and all files related to it's layers to a single directory. Files are sorted into "Vector", "Raster" or "Other" subdirectories depending on the layer type. The layer sources in the QGIS project are adjusted accordingly and definition queries, styles etc. are kept intact in the process.

---

## TODO:
- Add new function: Relief visualization
- Add new function: Geozone checker
- Add new function: CRS batch transformer
- Add new function: Directory content comparer
- Add new function: UAS data sorter
