#' Merge Raster Tiles
#'
#' Merges multiple raster files from a specified directory into a single mosaic raster.
#' Designed primarily for raster tiles from OpenGeoData portals, this function also includes
#' an option to automatically unzip raster tile archives before processing.
#'
#' @details
#' This function processes raster files in a given directory that match a specified filename pattern.
#' It supports handling `.xyz` and other raster file formats. The function performs the following steps:
#' 1. Optionally unzips `.zip`  and `.tar.gz` archives if `unzip = TRUE`.
#' 2. Reads raster files based on the specified file extension.
#' 3. Merges the raster tiles into a single mosaic, preserving the coordinate reference system (CRS).
#' 4. Saves the final mosaic as a GeoTIFF file.
#'
#' The output raster retains metadata, styling, and definition queries where applicable.
#'
#' @param input_dir Path to the directory containing the raster files, `.zip` or `.tar.gz` archives. Defaults to the current working directory (`"."`).
#' @param pattern Filename pattern to filter the raster files (e.g., "tile" to match files named `tile01.tif`, `tile02.tif`, etc.).
#' @param input_ext File extension of the raster files to process (e.g., `"xyz"`, `"tif"`, etc.). Do not include the `"."`.
#' @param epsg EPSG code for the crs of the raster. Defaults to `"25832"` (UTM32).
#' @param unzip Logical. If `TRUE`, unzips `.zip` archives in the directory before processing. Defaults to `TRUE`.
#' @param remove_old Logical. If `TRUE`, deletes original files after extraction or processing. Defaults to `FALSE`.
#'
#' @return Final mosaic as a `SpatRaster`
#'
#' @examples
#' \dontrun{
#' # Merge all `.xyz` raster tiles matching "tile" in the current directory
#' rastertile.merge(
#'   input_dir = ".",
#'   pattern = "tile",
#'   input_ext = "xyz"
#' )
#'
#' # Merge `.tif` raster files from a specific folder and unzip archives first
#' rastertile.merge(
#'   input_dir = "/path/to/data",
#'   pattern = "tile",
#'   input_ext = "tif",
#'   epsg = "25832",
#'   unzip = TRUE
#' )
#' }
#'
#' @importFrom terra rast mosaic writeRaster crs
#' @importFrom stats na.omit
#' @importFrom utils read.table untar unzip

#' @export


rastertile.merge <- function(input_dir = ".", pattern ="", input_ext = "tif", epsg = "25832", unzip = TRUE, remove_old = FALSE) {
  # Unzips data if needed
  if (unzip){
    # Get list of zip
    zip_list <- list.files(path = input_dir, pattern = paste0(pattern,"\\.zip$"), recursive = FALSE, full.names = TRUE)
    if (length(zip_list) > 0) {
      counter <- 1
      for (zip in zip_list){
        print(paste("Unzipping: ", counter, "/", length(zip_list)))
        unzip(zip, exdir = input_dir)
        counter <- counter + 1
      }
    }
    tar_list <- list.files(path = input_dir, pattern = paste0(pattern,"\\.tar\\.gz$"), recursive = FALSE, full.names = TRUE)
    if (length(tar_list) > 0) {
      counter <- 1
      for (tar in tar_list){
        print(paste("Untarring: ", counter, "/", length(tar_list)))
        untar(tar, exdir = input_dir)
        counter <- counter + 1
      }
    }
  }

  # List of .xyz files + empty list
  file_list <- list.files(path = input_dir, pattern = paste0(pattern,".*\\.", input_ext, "$"), recursive = TRUE, full.names = TRUE)
  raster_list <- list()

  # Load rasters and append to list
  for (file in file_list) {
    print(paste("Assembling tiles: ", length(raster_list)+1, "/", length(file_list)))
    raster_list[[length(raster_list) + 1]] <- rast(file)
  }

  # Check if raster_list is not empty before mosaicking
  if (length(raster_list) > 0) {
    print("Merging...")
    # Create mosaic
    mosaic_raster <- do.call(mosaic, c(raster_list, fun = mean))
    if (input_ext == "xyz"){
      terra::crs(mosaic_raster) <- paste0("epsg:", epsg)
    }
    else {
      terra::crs(mosaic_raster) <- crs(raster_list[[1]])
    }
    # Save finished mosaic
    print("Saving...")
    output_name <- paste0(input_dir, "/", pattern, "_merged.tif")
    writeRaster(mosaic_raster, filename = output_name, filetype = "GTiff", overwrite = TRUE)
    return(mosaic_raster)
    # Print completion message
    print(paste0("Mosaicking complete and saved as ", output_name))
  } else {
    print("No valid rasters to mosaic.")
  }
}
