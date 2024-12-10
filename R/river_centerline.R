#' Extract River Centerline
#'
#' This function processes a high-resolution Digital Elevation Model (DEM) to extract river centerlines
#' by simulating water flow and determining areas of significant flow accumulation.
#'
#' @details
#' The function performs the following steps:
#' 1. Fills depressions in the DEM to ensure continuous flow simulation using the D8 algorithm.
#' 2. Computes flow accumulation across the DEM.
#' 3. Identifies river networks by applying a flow accumulation threshold.
#' 4. Converts the identified river raster into a vector of centerlines (line geometries).
#'
#' Intermediate products, such as the filled DEM and flow accumulation raster, are saved with filenames
#' derived from the input DEM. If these files already exist, they are reused to save computation time on reruns.
#'
#' @param input_raster Path to the input DEM file in GeoTIFF format (`.tif`).
#' @param threshold Numeric. Flow accumulation threshold to define river networks. Cells with flow accumulation values above this threshold are considered part of the river network. Defaults to `1000`.
#'
#' @return A `SpatVector` object containing the river centerlines as vector geometries.
#'
#' @examples
#' \dontrun{
#' # Extract river centerlines from a DEM with a flow accumulation threshold of 1000
#' rivers <- river.centerline(input_raster = "path_to_dem.tif", threshold = 1000)
#'
#' # Save the vectorized river network
#' writeVector(rivers, "river_centerlines.shp", filetype = "ESRI Shapefile")
#' }
#'
#' @importFrom terra rast writeVector plot as.lines
#' @importFrom tools file_path_sans_ext
#' @importFrom whitebox wbt_fill_depressions wbt_d8_flow_accumulation
#'
#' @export


river.centerline <- function(input_raster, threshold = 100000) {
  # Checks input validity
  if (!file.exists(input_raster) || !grepl("\\.tif$", input_raster)) {
    stop("Invalid path: The path does not exist or is not a .tif file.")
  }
  # Intermediate product file naming
  filename_filled   <- paste0(tools::file_path_sans_ext(input_raster), "_filled.tif")
  filename_flowacc  <- paste0(tools::file_path_sans_ext(input_raster), "_flowacc.tif")

  # Fill depressions to ensure better flow simulation
  if (!file.exists(filename_filled)){
    print("Creating filled DEM...")
    wbt_fill_depressions(input_raster, filename_filled)
  }
  else{ #On rerun:
    print("Using preexisting filled DEM.")
  }

  # Flow accumulation
  if (!file.exists(filename_flowacc)){
    print("Creating flow accumulation...")
    wbt_d8_flow_accumulation(filename_filled, filename_flowacc)
  }
  else{ #On rerun:
    print("Using preexisting flow accumulation.")
  }

  # Load and threshold the flow accumulation
  flow_acc <- rast(filename_flowacc)
  rivers <- flow_acc > threshold

  # Plot the river network
  plot(rivers, main = "Extracted River Network")

  # Convert raster rivers to vector (lines)
  return(as.lines(rivers))
}
