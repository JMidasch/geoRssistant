#' Creates Relative Elevation Models for Rivers
#'
#' This function automates the workflow described in this QGIS Tutorial:
#' https://dancoecarto.com/creating-rems-in-qgis-the-idw-method
#'
#' As input it takes a Digital Elevation Model and the centerline of the river.
#' If no centerline is provided it will be calculated automatically based on a user-defined seed point.
#' The seed point should be situated upstream inside the riverbed near the edge of the DEM.
#'
#' @details
#' The function performs the following steps:
#' 1. Load the DEM and the centerline. If no centerline is provided, load the seed point.
#' 2. If no centerline is provided, calculate one based on the seedpoint and the DEM.
#' 2.1 Create a smoother version of the DEM using a median filter
#' 2.2 Breach any sinks in the smoothed DEM
#' 2.3 Fill any remaining sinks
#' 2.4 Calculate Flow Direction
#' 2.5 Trace downslope flowpath based on seed point
#' 2.6 Turn raster flowpath into centerline vector geometry
#' 3. Place points along centerline
#' 4. Extract elevation values for these points
#' 5. IDW interpolate the entire DEMs' area based on the extracted elevation values
#' 6. Subtract the interpolated raster from the original DEM to get the REM
#' 7. Save the REM
#'
#' @param input_raster Either a `terra SpatRaster` or anything that can be read as one. DEM used to calculate the REM.
#' @param centerline Either a `terra SpatVector`, a `sf linestring` or anything that can be read as one. Centerline of a river. If not provided it will be calculated based on the seedpoint.
#' @param seed_point Either a `terra SpatVector`, a `sf point` or anything that can be read as one. Point in the riverbed from which the centerline will be calculated if no centerline was provided.
#' @param output_dir Path to save the output flow paths raster or vector file.
#' @param output_pattern Character. Will be used as part of the file names of any intermediate products.
#' @param river_width Numeric. Aproximate width of the river in meter. Used for point spacing, DEM smoothing and sink breaching.
#' @param overwrite Logical. Determines if the function is allowed to overwrite existing files.
#'
#' @return The REM as a `SpatRaster`
#'
#' @examples
#' \dontrun{
#' river.rem(input_raster = "path_to_dem.tif",
#'           centerline = "centerline.shp",
#'           output_dir = ".")
#'
#' river.rem(input_raster = "path_to_dem.tif",
#'           seed_point = "seed_point.shp",
#'           output_dir = ".")
#' }
#'
#' @importFrom terra rast vect crs as.points resample extract res interpolate
#' @importFrom sf st_line_sample st_cast st_as_sf st_coordinates st_read st_transform st_linestring st_length
#' @importFrom whitebox wbt_median_filter wbt_breach_depressions_least_cost wbt_fill_depressions wbt_d8_pointer wbt_trace_downslope_flowpaths
#' @importFrom RANN nn2
#' @importFrom gstat gstat
#' @importFrom units set_units
#'
#' @export


river.rem <- function(input_raster, centerline = NULL, seed_point = NULL, output_dir, output_pattern = "riverdem", river_width = 10, overwrite = FALSE) {
  filename_rem <- paste0(output_dir, output_pattern, "_rem.tif")
  if(isFALSE(overwrite) && file.exists(filename_rem)){
    stop("Process stopped: REM file already exists and overwrite is disabled!")
  }

  # Load the DEM
  if (!inherits(input_raster, "SpatRaster")) {
    input_raster <- rast(input_raster)
  }

  if (!is.null(centerline)){
    # ---- Centerline was provided ----
    # Validate or read the centerline
    if (inherits(centerline, "SpatVector")) {
      centerline <- st_as_sf(centerline)
    } else if (is.character(centerline) && file.exists(centerline)) {
      centerline <- st_read(centerline, quiet = TRUE)
    } else if (!inherits(centerline, "sf")){
      stop("Invalid input: 'centerline' should be a SpatVector object, sf object or a valid file path.")
    }

    # Consolidate CRS
    if(is.na(crs(input_raster, describe=TRUE)$code) && is.na(st_crs(centerline)$epsg)){
      stop("Neither the DEM nor the points have a known coordinate system!")
    } else if(is.na(crs(input_raster, describe=TRUE)$code)){
      input_raster <- project(input_raster, paste0("EPSG:", st_crs(centerline)$epsg))
    } else if(is.na(st_crs(centerline)$epsg) || crs(input_raster, describe=TRUE)$code != st_crs(centerline)$epsg){
      points <- st_transform(centerline, paste0("EPSG:", crs(input_raster, describe=TRUE)$code))
    }

    centerline <- st_cast(centerline, "LINESTRING", warn = FALSE)

  } else if(!is.null(seed_point)){
    # ---- seed_point was provided ----
    # Load the seed seed_point
    if (!inherits(seed_point, "SpatVector")) {
      seed_point <- vect(seed_point)
    }
    # Consolidate CRS
    if(is.na(crs(input_raster, describe=TRUE)$code) && is.na(crs(seed_point, describe=TRUE)$code)){
      stop("Neither the DEM nor the seed_point have a known coordinate system!")
    } else if(is.na(crs(input_raster, describe=TRUE)$code)){
      input_raster <- project(input_raster, paste0("EPSG:", crs(seed_point, describe=TRUE)$code))
    } else if(is.na(crs(seed_point, describe=TRUE)$code || crs(input_raster, describe=TRUE)$code != crs(seed_point, describe=TRUE)$code)){
      seed_point <- project(seed_point, paste0("EPSG:", crs(input_raster, describe=TRUE)$code))
    }

    # File names
    if (!grepl("/$", output_dir)) {
      output_dir = paste0(output_dir, "/")
    }

    filename_smooth <- paste0(output_dir, output_pattern, "_smoothed.tif")
    filename_breached <- paste0(output_dir, output_pattern, "_breached.tif")
    filename_filled  <- paste0(output_dir, output_pattern, "_filled.tif")
    filename_flowdir <- paste0(output_dir, output_pattern, "_flowdir.tif")
    filename_centerline_raster <- paste0(output_dir, output_pattern, "_centerline.tif")

    # Calculate river_width in raster cells
    river_width_px <- ceiling(river_width / res(input_raster)[1])

    if (isTRUE(overwrite) || !file.exists(filename_flowdir)) {
      if (isTRUE(overwrite) || !file.exists(filename_filled)) {
        if (isTRUE(overwrite) || !file.exists(filename_breached)) {
          if (isTRUE(overwrite)|| !file.exists(filename_smooth)) {
            print("Creating smoothed DEM...")
            wbt_median_filter(input = input_raster, filterx = river_width_px, filtery = river_width_px, output = filename_smooth, verbose_mode = FALSE)
          } else {
            print("Using preexisting smoothed DEM.")
          }
          # Breach depressions to ensure better flow simulation
          print("Creating breached DEM...")
          wbt_breach_depressions_least_cost(dem = filename_smooth, output = filename_breached, dist = river_width_px, fill = TRUE, verbose_mode = FALSE)
        } else {
          print("Using preexisting breached DEM.")
        }
        # Fill depressions to ensure better flow simulation
        print("Creating filled DEM...")
        wbt_fill_depressions(filename_breached, filename_filled, verbose_mode = FALSE)
      } else {
        print("Using preexisting filled DEM.")
      }
      # Calculate Flow direction
      print("Creating flow direction...")
      wbt_d8_pointer(filename_filled, filename_flowdir, verbose_mode = FALSE)
    } else {
      print("Using preexisting flow direction.")
    }

    # Trace flow paths directly using the vector seed_point
    print("Tracing downslope flow paths...")
    wbt_trace_downslope_flowpaths(
      d8_pntr = filename_flowdir,
      seed_pts = seed_point,
      output = filename_centerline_raster,
      verbose_mode = FALSE
    )
    # Get the coordinates of each pixel on the center line
    rivermask <- rast(filename_centerline_raster)
    centerpixels <- as.points(rivermask, na.rm = TRUE)
    # Get coordinates from the points
    centerpixels_coords <- terra::crds(centerpixels)

    # Convert to matrix (only x, y coordinates)
    centerpixels_coords_matrix <- as.matrix(centerpixels_coords[, c("x", "y")])

    # Find the 2 nearest neighbours for each
    nn_result <- nn2(centerpixels_coords_matrix, k = 2, searchtype = "radius", radius = 1.1*sqrt(res(input_raster)[1]**2 + res(input_raster)[2]**2))
    # Order Points based on NN result
    ordered_indices <- numeric(nrow(centerpixels_coords))
    ordered_indices[1] <- 1  # Start with the first point
    visited <- rep(FALSE, nrow(centerpixels_coords))  # Track which points have been visited
    visited[1] <- TRUE
    for (i in 2:nrow(centerpixels_coords)) {
      last_index <- ordered_indices[i - 1]
      next_index <- nn_result$nn.idx[last_index, 2]
      while (visited[next_index]) {
        next_index <- nn_result$nn.idx[next_index, 2]
      }
      ordered_indices[i] <- next_index
      visited[next_index] <- TRUE
    }
    ordered_centerpixels <- centerpixels[ordered_indices, ]
    # Turn points into SF Linestring
    centerpixels_sf <- st_as_sf(centerpixels_coords)
    centerline <- st_sfc(st_linestring(st_coordinates(centerpixels_sf)))
    plot(centerline)
  } else {
    # ---- neither was provided ----
    stop("centerline and seed_point can't both be NULL!")
  }
  # ---- Get centerpoints and their values ----

  # Generate points along the centerlines
  # 13756 seems to be the maximum amount of points st_line_sample can place
  # so the river_width will be adjusted automatically to avoid reaching that limit
  if(sum(st_length(centerline)) / 13750 > set_units(river_width, "m")){
    river_width <- sum(st_length(centerline)) / 13750
  }

  centerpoints <- st_line_sample(centerline, density = 1 / river_width, type = "regular")
  centerpoints <- st_cast(centerpoints, "POINT")

  # Convert sampled points to a SpatVector
  centerpoints_coords <- sf::st_coordinates(centerpoints)  # Extract coordinates from points
  centerpoints_vect <- terra::vect(centerpoints_coords, crs = crs(input_raster))  # Create SpatVector
  plot(centerpoints_vect)
  # Extract raster values at point locations
  values <- terra::extract(input_raster, centerpoints_vect)

  # Prepare data frame for IDW interpolation
  centerpoints_df <- data.frame(
    x = centerpoints_coords[, 1],
    y = centerpoints_coords[, 2],
    values = values[, 2]  # Column 2 contains raster values
  )
  centerpoints_df <- na.omit(centerpoints_df)

  # ---- IDW, final REM ----

  # Calculate new resolution (10x larger cell size)
  new_res <- terra::res(input_raster) * 10

  # Create a new raster with coarser resolution
  coarse_raster <- rast(ext(input_raster), res = new_res, crs = crs(input_raster))

  # Create a gstat object for IDW
  idw_model <- gstat::gstat(
    formula = values ~ 1,
    locations = ~ x + y,
    data = centerpoints_df,
    set = list(idp = 2)
  )
  print("ready for idw")
  # Perform interpolation on the coarse raster grid
  idw_raster <- interpolate(coarse_raster, idw_model)
  idw_raster <- idw_raster[["var1.pred"]]
  print("idw done")

  # Resample IDW raster to match the DEM resolution
  upsampled_idw <- resample(idw_raster, input_raster, method = "bilinear", verbose = FALSE)
  print("upsampled")
  # Calculate REM by subtracting upsampled_idw from input_raster
  rem <- input_raster - upsampled_idw

  # Save the REM raster to disk
  writeRaster(rem, filename_rem, overwrite = TRUE, verbose = FALSE)

  return(rem)
}
