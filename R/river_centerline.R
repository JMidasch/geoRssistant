#' Extract River Centerlines from Points, Shapefiles, or GeoPackages
#'
#' This function processes a high-resolution Digital Elevation Model (DEM) to determine water flow paths
#' starting from specific points, based on the D8 flow direction algorithm. Points can be provided as a
#' `data.frame`, shapefile, GeoPackage, or `terra` vector.
#'
#' @details
#' The function performs the following steps:
#' 1. Fills depressions in the DEM to ensure continuous flow simulation using the D8 algorithm.
#' 2. Computes flow direction from the DEM.
#' 3. Traces water flow paths starting from user-provided points or vector files.
#'
#' Intermediate products, such as the filled DEM and flow direction raster, are saved with filenames
#' derived from the input DEM. If these files already exist, they are reused to save computation time on reruns.
#'
#' @param input_raster Path to the input DEM file in GeoTIFF format (`.tif`).
#' @param points Either:
#' - A `data.frame` or `matrix` with columns `x` and `y`, specifying the coordinates of the starting points.
#' - A path to a shapefile (`.shp`), GeoPackage (`.gpkg`), or `terra` vector file containing the starting points.
#' @param output_dir Path to save the output flow paths raster or vector file.
#'
#' @return A file path to the output flow paths file (raster or vector format).
#'
#' @examples
#' \dontrun{
#' # Example 1: Points as data.frame
#' points <- data.frame(x = c(500000, 500100),
#'                     y = c(4000000, 4000100))
#' river.centerline(input_raster = "path_to_dem.tif",
#'                  points = points,
#'                  output_dir = "flow_paths.shp")
#'
#' # Example 2: Points as shapefile
#' river.centerline(input_raster = "path_to_dem.tif",
#'                  points = "points.shp",
#'                  output_dir = "flow_paths.shp")
#'
#' # Example 3: Points as GeoPackage
#' river.centerline(input_raster = "path_to_dem.tif",
#'                  points = "points.gpkg",
#'                  output_dir = "flow_paths.shp")
#' }
#'
#' @importFrom terra rast vect crs as.polygons as.lines writeVector
#' @importFrom tools file_path_sans_ext
#' @importFrom whitebox wbt_fill_depressions wbt_d8_pointer wbt_trace_downslope_flowpaths
#'
#' @export


river.centerline <- function(input_raster, points, output_dir) {
  # Check input validity
  if (!file.exists(input_raster) || !grepl("\\.tif$", input_raster)) {
    stop("Invalid path: The path does not exist or is not a .tif file.")
  }

  # Determine points input type
  if (inherits(points, "SpatVector")) {
    # Points are provided as a terra SpatVector
    points_vect <- points
  } else if (is.character(points) && file.exists(points)) {
    # Points are provided as a file path (shapefile, GeoPackage, etc.)
    file_ext <- tools::file_ext(points)
    if (file_ext == "shp" || file_ext == "gpkg") {
      points_vect <- vect(points)
    } else {
      stop("Unsupported file type for points: must be .shp or .gpkg")
    }
  } else if (is.data.frame(points) || is.matrix(points)) {
    # Points are provided as a data.frame or matrix
    dem_raster <- rast(input_raster)
    points_vect <- vect(points, geom = c("x", "y"), crs = crs(dem_raster))
  } else {
    stop("Invalid points: Provide either a terra SpatVector, a shapefile path, a GeoPackage path, or a data.frame/matrix with 'x' and 'y' columns.")
  }

  # File names
  if (!grepl("/$", output_dir)) {
    output_dir = paste0(output_dir, "/")
  }

  filename_filled  <- paste0(output_dir, tools::file_path_sans_ext(basename(input_raster)), "_filled.tif")
  filename_flowdir <- paste0(output_dir, tools::file_path_sans_ext(basename(input_raster)), "_flowdir.tif")
  filename_centerline_raster <- paste0(output_dir, tools::file_path_sans_ext(basename(input_raster)), "_centerline.tif")
  filename_centerline_vector <- paste0(output_dir, tools::file_path_sans_ext(basename(input_raster)), "_centerline.shp")

  # Fill depressions to ensure better flow simulation
  if (!file.exists(filename_filled)) {
    print("Creating filled DEM...")
    wbt_fill_depressions(input_raster, filename_filled)
  } else {
    print("Using preexisting filled DEM.")
  }

  # Flow direction
  if (!file.exists(filename_flowdir)) {
    print("Creating flow direction...")
    wbt_d8_pointer(filename_filled, filename_flowdir)
  } else {
    print("Using preexisting flow direction.")
  }

  # Trace flow paths directly using the vector points
  print("Tracing downslope flow paths...")
  wbt_trace_downslope_flowpaths(
    d8_pntr = filename_flowdir,
    seed_pts = points_vect,
    output = filename_centerline_raster,
    verbose_mode = FALSE
  )

  # Load the raster and convert it to polygons
  flowpath_polygons <- as.polygons(rast(filename_centerline_raster), values = TRUE)
  # Convert to lines and save the lines as a shapefile
  writeVector(as.lines(flowpath_polygons), filename_centerline_vector, filetype = "ESRI Shapefile")


  print(paste("Centerline saved to:", filename_centerline_vector))
  return(filename_centerline_vector)
}
