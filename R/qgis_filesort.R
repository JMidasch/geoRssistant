#' Sort and Consolidate QGIS Project Files
#'
#' This function moves a QGIS project and all the source files for its layers
#' into a single directory. Source paths, styling, definition queries,
#' and metadata are kept intact. Layer files are sorted into appropriate
#' subdirectories ("Vector", "Raster", "Other") based on file extention.
#'
#' @details
#' This function processes a QGIS project file, copying all the associated
#' layer files and adjusting their paths to be relative to the new directory.
#' Layers are sorted based on their type.
#'
#' Metadata, styling, and definition queries are preserved during the operation.
#'
#' @param input_path Path to the .qgz QGIS project file.
#' @param output_dir Directory where all files will be copied. Defaults to a new folder
#'        with the same name as the QGIS project in the QGIS project's directory.
#' @param remove_old Logical, if TRUE, deletes old files after copying.
#'
#' @examples
#'  \dontrun{
#' qgis.filesort(input_path = "/path/to/project.qgz",
#'               output_dir = "",
#'               remove_old = FALSE)
#' }
#' @importFrom xml2 read_xml xml_find_all xml_text xml_set_text write_xml
#' @importFrom utils unzip zip
#' @importFrom tools file_path_sans_ext
#' @export


qgis.filesort <- function(input_path, output_dir ="", remove_old = FALSE) {
  # Check path validity
  if (!file.exists(input_path) || !grepl("\\.qgz$", input_path)) {
    stop("Invalid path: The path does not exist or is not a .qgz file.")
  }

  # Get path components
  project_file <- basename(input_path)
  project_dir <- dirname(input_path)
  project_sans <- tools::file_path_sans_ext(project_file)

  #Get output directory or create it
  if (output_dir == ""){
    output_dir <- tools::file_path_sans_ext(input_path)
    # Create new folder
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
      print(paste("Folder created:", output_dir))
    }
  }

  # Copy QGIS project file and unzip it
  project_copy <- paste0(output_dir, "/", project_sans, ".zip")
  file.copy(input_path, project_copy)
  unzip(project_copy, exdir = output_dir)

  # Read the .qgs as a xml
  project_new <- paste0(output_dir, "/", project_sans, ".qgs")
  project_xml <- read_xml(project_new)
  project_layernodes <- xml_find_all(project_xml, "//datasource")

  # Copy all layers into the directory and adjust xml
  print("Copying files...")
  for (layer in project_layernodes){
    # Get path from xml
    layerpath_old <- xml_text(layer)
    # Catch definition queries
    layerpath_query <- ""
    if (grepl("\\|", layerpath_old)){
      layerpath_query <- paste0( "\\|", sub(".*\\|", "", layerpath_old))
      layerpath_old <- sub("\\|.*", "", layerpath_old)
    }

    # Catch some different path types
    if (!grepl("^([a-zA-Z]:|/)", layerpath_old)) {
      # Turn relative layer path into absolute path
      layerpath_old <- file.path(project_dir, layerpath_old)
      layerpath_old <- normalizePath(layerpath_old, winslash = "/", mustWork = FALSE)
    }
    else if (grepl("^https?://", layerpath_old, ignore.case = TRUE)) {
      # Skip web URLs
      next
    }

    # Check if file exists
    if(file.exists(layerpath_old) && !dir.exists(layerpath_old)){
      # Classify layer
      if (grepl("\\.(tif|tiff|jpg|png|jpeg|bmp|gif|aux\\.xml|ovr|rrd|tfw|jgw|pgw|bpw|gfw|prj|vrt)$", layerpath_old, ignore.case = TRUE)) {
        layerclass <- "Raster"
      } else if (grepl("\\.(shp|geojson|json|gpkg|kml|gml|shx|dbf|prj|cpg|qpj|sbn|sbx|aih|xml|kmz|xsd)$", layerpath_old, ignore.case = TRUE)) {
        layerclass <- "Vector"
      } else {
        layerclass <- "Other"
      }
      # Create class-based subdirectory if it doesn't exist yet
      class_dir <- paste0(output_dir, "/", layerclass)
      if (!dir.exists(class_dir)) {
        dir.create(class_dir, recursive = TRUE)
      }

      # Adjust path in xml
      layerpath_file <- basename(layerpath_old)
      xml_set_text(layer, paste0("./", layerclass, "/", layerpath_file, layerpath_query))

      # Get dir and filename without extension for the current layer
      layerpath_dir <- dirname(layerpath_old)
      layerpath_sans <- tools::file_path_sans_ext(layerpath_file)

      # Find file and all corresponding metadata files, copy them to new location
      layerlist <- list.files(path = layerpath_dir, pattern = paste0("^", layerpath_sans, "\\..+$"), full.names = TRUE, recursive = FALSE)
      for (layerfile in layerlist){
        if (layerclass == "Raster" && grepl("\\.(tif|tiff|jpg|png|jpeg|bmp|gif|aux\\.xml|ovr|rrd|tfw|jgw|pgw|bpw|gfw|prj|vrt)$", layerfile, ignore.case = TRUE)){
          layerpath_new <- paste0(class_dir, "/", basename(layerfile))
          file.copy(layerfile, layerpath_new)
        }
        else if (layerclass == "Vector" && grepl("\\.(shp|geojson|json|gpkg|kml|gml|shx|dbf|prj|cpg|qpj|sbn|sbx|aih|xml|kmz|xsd)$", layerfile, ignore.case = TRUE)){
          layerpath_new <- paste0(class_dir, "/", basename(layerfile))
          file.copy(layerfile, layerpath_new)
        }
        else if (layerclass == "Other"){
          layerpath_new <- paste0(class_dir, "/", basename(layerfile))
          file.copy(layerfile, layerpath_new)
        }
        else {
          print(paste("File skipped due to layertype/fileextension mismatch:", basename(layerfile)))
        }
        # Remove old layer files if requested and if file was successfully copied
        if(remove_old && file.exists(layerpath_new)){
            file.remove(layerfile)
        }
      }
    }
  }
  # Save xml
  print("Reassembling QGIS project file...")
  write_xml(project_xml, project_new, options = "as_xml")

  # Get stylefile and zip everything back up
  style_file <- list.files(path = output_dir, pattern="_styles.db", full.names = TRUE)
  zip(paste(output_dir, "/", project_file, sep=""), files = c(project_new, style_file), extras = "--junk-paths")

  # Delete the friends we made along the way
  file.remove(c(project_copy, project_new, style_file))

  # Remove old QGZ file if requested and if file was successfully copied
  if(remove_old && file.exists(file.path(output_dir, project_file))){
      file.remove(input_path)
  }
}
