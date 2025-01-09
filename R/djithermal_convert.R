#' Convert Thermal Images to GeoTIFF
#'
#' This function processes thermal images captured in RJPEG format, extracts thermal data embedded in EXIF metadata,
#' decodes the data, and converts it into GeoTIFF raster files. The function also copies relevant metadata to the output files.
#'
#' @details
#' The function performs the following steps:
#' 1. Reads thermal data from the EXIF metadata of each image in the input directory.
#' 2. Decodes the thermal data from base64 format.
#' 3. Reshapes the decoded data into a 2D matrix and converts it to a raster object.
#' 4. Writes the raster to a GeoTIFF file in the specified output directory.
#' 5. Copies key EXIF metadata to the GeoTIFF files.
#'
#' Images with encrypted thermal data are skipped, and their filenames are logged for review.
#'
#' @param input_dir Path to the directory containing RJPEG thermal images (`_T.JPG`).
#' @param output_dir Path to the directory where the output GeoTIFF files will be saved.
#' @param height Numeric. Height (in pixels) of the thermal image matrix. Default is 512.
#' @param width Numeric. Width (in pixels) of the thermal image matrix. Default is 640.
#'
#' @return A message summarizing the number of processed and skipped files. Output GeoTIFF files are saved in the specified output directory.
#'
#' @examples
#' \dontrun{
#' # Convert thermal images in the input directory to GeoTIFF format
#' djithermal.convert(input_dir = "path_to_rjpeg_images", output_dir = "path_to_output_tiffs")
#' }
#'
#' @importFrom exifr read_exif exiftool_call
#' @importFrom terra rast writeRaster
#' @importFrom caTools base64decode
#' @importFrom tools file_path_sans_ext
#'
#' @export


djithermal.convert <- function(input_dir, output_dir, height=512, width=640){
  # Get all paths to the thermal images from the input directory
  rjpeg_list <- list.files(path = input_dir, pattern = "_T.JPG$",full.names = TRUE)
  counter <- 0
  encrypted <- list()
  print(paste0(counter, "/", length(rjpeg_list)))
  # Process each thermal image individually
  for (rjpeg in rjpeg_list){

    # Extract the thermal data from the image using exifr
    in_exif <- read_exif(rjpeg)
    thermal_data <- in_exif$ThermalData

    # Some images have to be skipped due to encryption
    if(grepl("@", thermal_data)){
      encrypted <- c(encrypted, basename(rjpeg))
      print(paste0("Image skipped due to encryption: ", basename(rjpeg)))
      next
    }

    # Decode base64
    thermal_data <- sub("^base64:", "", thermal_data)  # Remove 'base64:' prefix
    thermal_data <- caTools::base64decode(thermal_data, what = "int", size=2)           # Decode base64 string

    # Reshape the 1D vector into a 2D matrix (assuming 512x640 resolution)
    thermal_matrix <- matrix(thermal_data, nrow = height, ncol = width, byrow = TRUE)

    # Convert the matrix to a raster object using terra
    raster_obj <- rast(thermal_matrix)

    # Write the raster object to a TIFF file using terra
    output_path <- paste0(output_dir, "/", tools::file_path_sans_ext(basename(rjpeg)), "_cal.tif")
    terra::writeRaster(raster_obj, output_path, overwrite = TRUE)

    # Combine all EXIF metadata updates into a single call
    exiftool_args <- paste0(
      "-Model=", in_exif$Model[1], " ",
      "-Make=", in_exif$Make[1], " ",
      "-Orientation=", in_exif$Orientation[1], " ",
      "-FocalLength=", in_exif$FocalLength[1], " ",
      "-FocalLengthIn35mmFormat=", in_exif$FocalLengthIn35mmFormat[1], " ",
      "-DigitalZoomRatio=", in_exif$DigitalZoomRatio[1], " ",
      "-ApertureValue=", in_exif$ApertureValue[1], " ",
      "-GPSAltitude=", in_exif$GPSAltitude[1], " ",
      "-GPSLatitude=", in_exif$GPSLatitude[1], " ",
      "-GPSLongitude=", in_exif$GPSLongitude[1], " ",
      "-delete_original"
    )

    # Run a single exiftool command with all the arguments
    exiftool_call(exiftool_args, output_path, quiet = TRUE)

    # Update the user on progress
    counter <- counter + 1
    print(paste0(counter, "/", length(rjpeg_list)))
  }
  print(paste0("Number of files skipped due to encryption: ", length(encrypted)))
  for(j in encrypted){
    print(j)
  }
}
