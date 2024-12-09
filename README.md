# WIP R-Package

R-Package that adds a quality-of-life functions for working with geodata to R.
  
## Currrently included:

### rastertile.merge(input_dir = ".", pattern, input_ext, epsg ="25832", unzip = TRUE, remove_old = FALSE)
  
#### Problem:
  
When you download geodata from a governmental open geodata portal you often have to download several individual tiles, often in the form of individual zip archives. Unzipping every single archive and merging all the raster files so you can actually work with them takes way to much time.

#### Solution:
  
This function unzips all zip archives in a folder and merges all raster they contain.

### qgis.filesort(input_path, output_dir ="", remove_old = FALSE)
  
#### Problem:
  
When you work on a QGIS plugin for a extended period of time and don't keep a close eye on your file structure, you might lose track of which files are actually needed for the project and where they are. If you then want to send the project to another person or want to back it up somewhere it's quite a hassle to organize all the data in a compact way.
  
#### Solution:
  
This function takes a QGIS-project as input and copies it and all files related to it's layers to a single folder. The layer sources in the QGIS project are adjusted accordingly and definition queries, styles etc. are kept intact in the process.
