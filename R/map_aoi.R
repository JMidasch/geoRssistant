#' Map Research AOI
#'
#' Creates a overview map of a research area for use in papers and presentations.
#' While only the `aoi` has to be defined several other options for customization exist.
#' Since it was originally created for remote arctic environments the default visualization
#' focuses on topography and water bodies for orientation.
#'
#' @details
#' This function takes a polygon and creates a map for the area around it based on several parameters.
#' It supports all common vector data types. The function performs the following steps:
#' 1. Load AOI.
#' 2. Determine the map extent based on AOI or an additional vector geometry and a given aspect ratio.
#' 3. Determine which country or region the AOI is situated in and load it's geometry for a inset map
#' 4. Load custom basemap or calculate one automatically from a DEM. Downloads a DEM automatically if necessary.
#' 5. Saves the final mosaic as a GeoTIFF file.
#' 6. Load custom water mask or receive one from OSM
#' 7. Create map with small inset overview map
#'
#' @param aoi Either sf-geometry, SpatVector or valid path to a file openable with `sf::st_read`. Should contain geometries representing an area of interest. Only mandatory argument.
#' @param basemap Either `NULL`, a SpatRaster or a valid path to a file openable with `terra::rast()`. Custom 3-band basemap used instead of one created automatically from a DEM.
#' @param dem_raster Either `NULL`, a SpatRaster or a valid path to a file openable with `terra::rast()`. Used to calculate a hillshade for the background if basemap is `NULL`. Downloads a DEM using `elevatr` if `NULL`.
#' @param dem_z Numeric, values between 0 and 14. Determines the zoom level of the dem downloaded if both `dem_raster` and `basemap` are `NULL`. More information at https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution . Defaults to `14`.
#' @param map_crs Numeric or `NULL`. EPSG code of crs used in both processing and visualisation. If `NULL` the crs of the AOI is used. Defaults to `NULL`.
#' @param map_ext Either numeric, sf-geometry, SpatVector or valid path to a file openable with `sf::st_read`. Extent of content is used as the extent of the map. If numeric the value is used to scale a buffer around the aoi based on the aoi extent. Defaults to `1`.
#' @param watermask Either logical, sf-geometry, SpatVector or valid path to a file openable with `sf::st_read`. Polygons used to visualise water surfaces. If `TRUE` water polygons and lines are queried from OSM, if `FALSE` no water will be added. Defaults to `TRUE`.
#' @param aoi_colour Character, hex colour value without alpha. Defaults to `"#ff0000"`
#' @param landscape Either logical or `NULL`. Orientation of the map layout. Either forces landscape (`TRUE`), portrait (`FALSE`) or calculates it based on the orientation of the aoi (`NULL`).
#' @param aspectratio Numeric or `NULL`. The map extent is expanded to fit the ratio. If `NULL` the map extent isn't expanded at all. Defaults to `16/9`, the most common screen aspect ratio.
#'
#' @return Returns the final ggplot
#'
#' @examples
#' \dontrun{
#' # Create map with default settings for an existing sf object
#' map.aoi(researcharea_sf)
#'
#' # Create fully customized map
#' map.aoi(
#'   aoi = "/path/to/aoi.shp",
#'   basemap = "/path/to/basemap.tiff",
#'   map_ext = "/path/to/mapextent.shp",
#'   watermask = FALSE,
#'   aoi_colour = "#0000ff"
#'   aspectratio = NULL
#' )
#' }
#'
#' @importFrom terra rast crs ext crop terrain shade project crs<-
#' @importFrom sf st_read st_transform st_crs st_as_sf st_bbox st_sfc st_polygon st_coordinates st_intersection st_union st_centroid st_sf st_intersects st_nearest_feature st_geometry
#' @importFrom ggplot2 ggplot geom_sf theme_void theme_minimal theme element_line element_text element_blank element_rect coord_sf unit
#' @importFrom ggspatial annotation_scale annotation_north_arrow north_arrow_fancy_orienteering
#' @importFrom ggfx with_shadow
#' @importFrom cowplot ggdraw draw_plot
#' @importFrom elevatr get_elev_raster
#' @importFrom rnaturalearth ne_countries
#' @importFrom osmdata opq add_osm_features osmdata_sf
#' @importFrom dplyr select
#' @importFrom RStoolbox ggRGB
#' @importFrom magrittr %>%
#'
#' @export

map.aoi <- function(aoi, basemap = NULL, dem_raster = NULL, dem_z = 14, map_crs = NULL, map_ext = 1, watermask = TRUE, aoi_colour = "#ff0000", landscape = NULL, aspectratio = 16/9) {
  # ------------- Get AOI ------------------
  if (inherits(aoi, "SpatVector")) {
    aoi <- st_as_sf(aoi)
  } else if (is.character(aoi) && file.exists(aoi)) {
    aoi <- st_read(aoi, quiet = TRUE)
  } else if (!inherits(aoi, "sf")){
    stop("Invalid input: 'aoi' should be a SpatVector object, sf object or a valid file path.")
  }
  # ------------- Get CRS ------------------
  if (!is.null(map_crs) && st_crs(aoi)$epsg != map_crs) {
    if (map_crs == 4326){
      map_crs <- 3857
    }
    print(paste0("Projecting AOI from EPSG:", st_crs(aoi)$epsg, " to EPSG:", map_crs, "..."))
    aoi <- st_transform(aoi, map_crs)
  } else if (is.null(map_crs)){
    map_crs <- st_crs(aoi)$epsg
    if (map_crs == 4326){
      print(paste0("AOI CRS detected: WGS84. Automatically changed to Webmercator."))
      map_crs <- 3857
      aoi <- st_transform(aoi, map_crs)
    } else{
      print(paste0("AOI CRS detected: EPSG:", map_crs))
    }
  }
  # ------------- Get Map Extent ------------------
  if(!is.numeric(map_ext)){
    if (inherits(map_ext, "SpatVector")) {
      map_ext <- st_as_sf(map_ext)
    } else if (is.character(map_ext) && file.exists(map_ext)) {
      map_ext <- st_read(map_ext, quiet = TRUE)
    } else if (!inherits(map_ext, "sf")){
      print("Invalid input: 'map_ext' should be a SpatVector object, sf object or a valid file path.")
      print("Defaulting to automatic map extent calculation.")
      map_ext <- 1
    }
    if(!is.null(map_ext)){
      if(st_crs(map_ext)$epsg != map_crs){
        map_ext <- st_transform(map_ext, map_crs)
      }
      bbox <- st_bbox(map_ext)
      x_extent <- (bbox[3] - bbox[1])
      y_extent <- (bbox[4] - bbox[2])
      if(is.null(aspectratio)){
        aspectratio <- x_extent / y_extent
      }
      if(aspectratio < 1){
        aspectratio^(-1)
      }
      if (isTRUE(landscape) || (is.null(landscape) && (y_extent <= x_extent))){
        # landscape format if:
        # landscape is TRUE or
        # landscape is NA and X-extend is larger than Y-extend
        x_ratio <- aspectratio^(-1)
        y_ratio <- 1
        if(y_extent * aspectratio >= x_extent){
          expanded_bbox <- c(
            xmin = bbox[1] - ((y_extent * aspectratio)-x_extent)/2,
            xmax = bbox[3] + ((y_extent * aspectratio)-x_extent)/2,
            ymin = bbox[2],
            ymax = bbox[4]
        )
        } else {
          expanded_bbox <- c(
            xmin = bbox[1],
            xmax = bbox[3],
            ymin = bbox[2] - ((x_extent / aspectratio)-y_extent)/2,
            ymax = bbox[4] + ((x_extent / aspectratio)-y_extent)/2
          )
        }
        aspectratio <- x_ratio
      } else {
        # portrait format if:
        # landscape is FALSE or
        # landscape is NA and Y-extend is larger than X-extend
        x_ratio <- 1
        y_ratio <- aspectratio^(-1)
        if(y_extent / aspectratio >= x_extent){
          expanded_bbox <- c(
            xmin = bbox[1] - ((y_extent / aspectratio)-x_extent)/2,
            xmax = bbox[3] + ((y_extent / aspectratio)-x_extent)/2,
            ymin = bbox[2],
            ymax = bbox[4]
          )
        } else {
          expanded_bbox <- c(
            xmin = bbox[1],
            xmax = bbox[3],
            ymin = bbox[2] - ((x_extent * aspectratio)-y_extent)/2,
            ymax = bbox[4] + ((x_extent * aspectratio)-y_extent)/2
          )
        }
      }
    }
  }
  # --- Calculate Map extent if not provided ---
  if(is.numeric(map_ext)){
    print("Calculating map extent...")

    bbox <- st_bbox(aoi)
    x_extent <- (bbox[3] - bbox[1])
    y_extent <- (bbox[4] - bbox[2])
    avg_extent <- ( x_extent + y_extent ) / 2
    if(is.null(aspectratio)){
      aspectratio <- x_extent / y_extent
    }
    if(aspectratio < 1){
      aspectratio^(-1)
    }
    if (isTRUE(landscape) || (is.null(landscape) && (y_extent <= x_extent))){
      # landscape format if:
      # landscape is TRUE or
      # landscape is NA and X-extend is larger than Y-extend
      x_ratio <- aspectratio^(-1)
      y_ratio <- 1
      if((y_extent + map_ext * 2 * avg_extent) * aspectratio >=
          x_extent + map_ext * (((y_extent + 2 * avg_extent) * (aspectratio)) - x_extent)){
        expanded_bbox <- c(
          xmin = bbox[1] - map_ext * ((((y_extent + 2 * avg_extent) * (aspectratio)) - x_extent) / 2),
          xmax = bbox[3] + map_ext * ((((y_extent + 2 * avg_extent) * (aspectratio)) - x_extent) / 2),
          ymin = bbox[2] - map_ext * avg_extent,
          ymax = bbox[4] + map_ext * avg_extent
        )
      } else {
        expanded_bbox <- c(
          xmin = bbox[1] - map_ext * avg_extent,
          xmax = bbox[3] + map_ext * avg_extent,
          ymin = bbox[2] - map_ext * ((((x_extent + 2 * avg_extent) / (aspectratio)) - y_extent) / 2),
          ymax = bbox[4] + map_ext * ((((x_extent + 2 * avg_extent) / (aspectratio)) - y_extent) / 2)
        )
      }
      aspectratio <- x_ratio
    } else {
      # portrait format if:
      # landscape is FALSE or
      # landscape is NA and Y-extend is larger than X-extend
      x_ratio <- 1
      y_ratio <- aspectratio^(-1)
      if((x_extent + 2 * map_ext * avg_extent) * aspectratio >=
          y_extent + map_ext * (((x_extent + 2 * avg_extent) * (aspectratio)) - y_extent)){
        expanded_bbox <- c(
          xmin = bbox[1] - map_ext * avg_extent,
          xmax = bbox[3] + map_ext * avg_extent,
          ymin = bbox[2] - map_ext * ((((x_extent + 2 * avg_extent) * (aspectratio)) - y_extent) / 2),
          ymax = bbox[4] + map_ext * ((((x_extent + 2 * avg_extent) * (aspectratio)) - y_extent) / 2)
        )
      } else {
        expanded_bbox <- c(
          xmin = bbox[1] - map_ext * ((((y_extent + 2 * avg_extent) / (aspectratio)) - x_extent) / 2),
          xmax = bbox[3] + map_ext * ((((y_extent + 2 * avg_extent) / (aspectratio)) - x_extent) / 2),
          ymin = bbox[2] - map_ext * avg_extent,
          ymax = bbox[4] + map_ext * avg_extent
        )
      }
    }
  }
  expanded_bbox_sf <- st_as_sf(st_sfc(
    st_polygon(list(matrix(
      c(expanded_bbox["xmin.xmin"], expanded_bbox["ymin.ymin"],  # Bottom-left
        expanded_bbox["xmax.xmax"], expanded_bbox["ymin.ymin"],  # Bottom-right
        expanded_bbox["xmax.xmax"], expanded_bbox["ymax.ymax"],  # Top-right
        expanded_bbox["xmin.xmin"], expanded_bbox["ymax.ymax"],  # Top-left
        expanded_bbox["xmin.xmin"], expanded_bbox["ymin.ymin"]), # Closing the polygon
      ncol = 2, byrow = TRUE
    ))),
    crs = st_crs(map_crs)
  ))

  # ------------- Get Country/Region ------------------
  countries <- ne_countries(scale = "medium", type = "map_units", returnclass = "sf")

  if (st_crs(countries) != map_crs) {
    countries <- st_transform(countries, map_crs)
  }

  aoicountry <- countries[st_intersects(countries, aoi, sparse = FALSE), ]
  aoicountry <- aoicountry[!is.na(aoicountry$sovereignt), ]

  if (nrow(aoicountry) == 0) {
    print("AOI is not located in any known landarea. Finding the closest one instead: ")
    closest_index <- st_nearest_feature(aoi, countries)
    aoicountry <- countries[closest_index, ]
  }

  for (i in 1:nrow(aoicountry)){
    if (!is.null(aoicountry$geou_dif[i]) && !is.na(aoicountry$geou_dif[i]) && aoicountry$geou_dif[i] != 0 ||
        !is.null(aoicountry$adm_dif[i]) && !is.na(aoicountry$adm_dif[i]) && aoicountry$adm_dif[i] != 0){
      print(paste0("AOI located in ", aoicountry$geounit[i], ", ", aoicountry$type[i], " of ", aoicountry$sovereignt[i]))
    }
    else
      print(paste0("AOI located in the ", aoicountry$type[i], " of ", aoicountry$sovereignt[i]))
  }
  # ------------- Get Basemap ------------------
  print("Creating basemap...")
  if(!is.null(basemap)){
    if (is.character(basemap) && file.exists(basemap)) {
      basemap <- rast(basemap)
    } else if (!inherits(basemap, "SpatRaster")){
      basemap <- NULL
    }
    if(!is.null(basemap) && crs(basemap, describe=TRUE)$code != map_crs){
      print(paste0("Projecting DEM from EPSG:", crs(basemap, describe=TRUE)$code, " to EPSG:", map_crs, "..."))
      basemap <- project(basemap, paste0("EPSG:", map_crs))
    }
  }

  # --- Calculate Hillshade if no Basemap provided ---
  if(is.null(basemap)){
    if (is.character(dem_raster) && file.exists(dem_raster)) {
      dem_raster <- rast(dem_raster)
    } else if (!inherits(dem_raster, "SpatRaster")){
      print("No valid DEM provided, downloading elevation data...")
      dem_raster <- get_elev_raster(expanded_bbox_sf, z = dem_z)
      dem_raster <- rast(dem_raster)
      crs(dem_raster)  <- paste0("epsg:", map_crs)
    }
    if (crs(dem_raster, describe=TRUE)$code != map_crs) {
      print(paste0("Projecting DEM from EPSG:", crs(dem_raster, describe=TRUE)$code, " to EPSG:", map_crs, "..."))
      dem_raster<- project(dem_raster, paste0("EPSG:", map_crs))
    }

    # Crop DEM to expanded AOI
    dem_cropped <- crop(dem_raster, ext(expanded_bbox))

    # Compute hillshade
    slope <- terrain(dem_cropped, v = "slope", unit = "radians")
    aspect <- terrain(dem_cropped, v = "aspect", unit = "radians")
    hillshade_r <- shade(slope, aspect, angle = 45, direction = 300)
    hillshade_g <- shade(slope, aspect, angle = 45, direction = 0)
    hillshade_b <- shade(slope, aspect, angle = 45, direction = 60)
    basemap <- c(hillshade_r, hillshade_g, hillshade_b)
  }

  # ------------- Get Water ------------------
  if(isTRUE(watermask)){
    print("Looking for water...")
    expanded_bbox_wgs84 <- st_transform(expanded_bbox_sf, 4326)
    coords_wgs84 <- st_coordinates(expanded_bbox_wgs84)
    watermask_all <- opq(c(coords_wgs84[1,"X"], coords_wgs84[1,"Y"], coords_wgs84[3,"X"], coords_wgs84[3,"Y"])) %>%
        add_osm_features(features = list("natural"="water", "natural"="bay", "natural"="cape", "natural"="strait",
                                         "waterway"="river", "waterway"="stream", "waterway"="tidal_channel", "waterway"="canal",
                                         "landuse"="basin", "landuse"="reservoir", "leisure"="marina")) %>%
        osmdata_sf()
    watermask_sf <- watermask_all$osm_polygons
    watermask_sf_multi <- watermask_all$osm_multipolygons
    if (is.null(watermask_sf) && is.null(watermask_sf_multi)) {
      watermask_sf <- st_sf(geometry = st_sfc(), crs = map_crs)
    } else if (is.null(watermask_sf)){
      watermask_sf <- dplyr::select(watermask_sf_multi, geometry = .data$geometry)
    } else if (!is.null(watermask_sf) && !is.null(watermask_sf_multi)){
      watermask_sf <- rbind(dplyr::select(watermask_sf, geometry = .data$geometry), dplyr::select(watermask_sf_multi, geometry = .data$geometry))
    } else {
      watermask_sf <- dplyr::select(watermask_sf, geometry = .data$geometry)
    }
    if(nrow(watermask_sf) != 0){
      print("Water polygons found!")
      if(st_crs(watermask_sf)$epsg != map_crs){
        watermask_sf <- st_transform(watermask_sf, map_crs)
      }
      watermask_sf <- st_intersection(watermask_sf, expanded_bbox_sf)
    }
    watermask_line <- watermask_all$osm_lines
    watermask_line_multi <- watermask_all$osm_multilines
    if (is.null(watermask_line) && is.null(watermask_line_multi)) {
      watermask_line <- st_sf(geometry = st_sfc(), crs = map_crs)
    } else if (is.null(watermask_line)){
      watermask_line <- dplyr::select(watermask_line_multi, geometry = .data$geometry)
    } else if (!is.null(watermask_line) && !is.null(watermask_line_multi)){
      watermask_line <- rbind(dplyr::select(watermask_line, geometry = .data$geometry), dplyr::select(watermask_line_multi, geometry = .data$geometry))
    } else {
      watermask_line <- dplyr::select(watermask_line, geometry = .data$geometry)
    }
    if(nrow(watermask_line) != 0){
      print("Water lines found!")
      if(st_crs(watermask_line)$epsg != map_crs){
        watermask_line <- st_transform(watermask_line, map_crs)
      }
      watermask_line <- st_intersection(watermask_line, expanded_bbox_sf)
    }
  } else if (isFALSE(watermask)) {
    watermask_sf <- st_sf(geometry = st_sfc(), crs = map_crs)
    watermask_line <- st_sf(geometry = st_sfc(), crs = map_crs)
  } else {
    watermask_line <- st_sf(geometry = st_sfc(), crs = map_crs)
    if (inherits(watermask, "SpatVector")) {
      watermask_sf <- st_as_sf(watermask)
    } else if (is.character(watermask) && file.exists(aoi)) {
      watermask_sf <- st_read(watermask, quiet = TRUE)
    } else if (inherits(aoi, "sf")){
      watermask_sf <- watermask
    } else {
      stop("Invalid input: 'watermask' should be TRUE, FALSE, SpatVector object, sf object or a valid file path.")
    }
    # Check CRS of AOI
    if (st_crs(watermask_sf)$epsg != map_crs) {
      print(paste0("Projecting watermask from EPSG:", st_crs(watermask_sf)$epsg, " to EPSG:", map_crs, "..."))
      watermask_sf <- st_transform(watermask_sf, map_crs)
    }
  }

  # ------------- Create Main Map ------------------
  print("Creating map...")
  main_map <-
    ggRGB(basemap, 1, 2, 3, scale = 1, geom_raster = TRUE) +
    geom_sf(data = watermask_sf, fill = "#1C3A62", color = "#00000000", linewidth = 0) +
    geom_sf(data = watermask_line, color = "#1C3A62", lineend = "round", linewidth = 1) +
    geom_sf(data = aoi, fill = paste0(aoi_colour, "22"), color = paste0(aoi_colour, "BB"), linewidth = 0.3) +
    annotation_scale(location = "br", width_hint = 0.2, text_cex = 0.8, line_width = 0.5,
                          pad_x = unit(0.1 * x_ratio, "npc"), pad_y = unit(0.03 * y_ratio, "npc")) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           pad_x = unit(0.1 * x_ratio, "npc"), pad_y = unit(0.1 * y_ratio, "npc"),
                           width = unit(0.12 * x_ratio, "npc"), height= unit(0.12 * y_ratio, "npc"),
                           style = north_arrow_fancy_orienteering) +
    coord_sf(expand = FALSE,
             xlim = c(expanded_bbox["xmin"], expanded_bbox["xmax"]),
             ylim = c(expanded_bbox["ymin"], expanded_bbox["ymax"]),
             clip = "on") +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.major = element_line(color = "grey40", linetype = "dotted", linewidth = 0.3),
      axis.text.x = element_text(size = 8, color = "black"),
      axis.text.y = element_text(size = 8, color = "black", angle = 90, vjust = 0.5, hjust = 0.5),
      axis.ticks = element_line(colour = "gray80", linewidth = 0.5),
      axis.title = element_blank(),
      panel.border = element_rect(colour = "gray80", fill = NA, linewidth=0.3)
    )
  # ------------- Create inset map ------------------
  # Compute country extent
  country_bbox <- st_bbox(aoicountry)
  country_width <- country_bbox["xmax"] - country_bbox["xmin"]
  country_height <- country_bbox["ymax"] - country_bbox["ymin"]

  # Extent ratio based on max dimension
  extent_ratio <- max(x_extent, y_extent) / max(country_width, country_height)

  # Build inset map depending on extent ratio

  # Plot only centroid in inset
  aoi_centroid <- st_centroid(st_union(aoi))

  inset_map <- ggplot() +
    with_shadow(
      geom_sf(data = aoicountry, fill = "gray80", color = "black"),
      x_offset = 2, y_offset = 2, sigma = 3, colour = "black", alpha = 0.5
    ) +
    theme_void() +
    coord_sf(expand = FALSE)

  if (extent_ratio > 0.15) {
    # Plot AOI shape in inset
    inset_map <- inset_map +
      geom_sf(data = aoi, fill = paste0(aoi_colour, "22"), color = paste0(aoi_colour, "BB"), linewidth = 0.3)

  } else {
    # Plot only centroid in inset
    aoi_centroid <- st_centroid(st_union(aoi))
    inset_map <- inset_map +
      geom_sf(data = aoi_centroid, color = aoi_colour, size = 2)
  }
  # ------------- Create final image ------------------
  final_plot <- ggdraw() +
    draw_plot(main_map) +
    draw_plot(inset_map,
              x = 1 - (0.40 * x_ratio),
              y = 1 - (0.40 * y_ratio),
              width = 0.3 * x_ratio,
              height = 0.3 * y_ratio) +
    theme(aspect.ratio = aspectratio)
  print(final_plot)
  return(final_plot)
  print("Done!")
}

utils::globalVariables(".data")
