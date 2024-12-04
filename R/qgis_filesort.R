#' Sort and Consolidate QGIS Project Files
#'
#' This function moves a QGIS project and all the source files for its layers
#' into a single directory. Source paths, styling, definition queries,
#' and metadata are kept intact.
#'
#' @details
#' This function processes a QGIS project file, copying all the associated
#' layer files and adjusting their paths to be relative to the new directory.
#' Metadata, styling, and definition queries are preserved during the operation.
#'
#' @param input_path Path to the .qgz QGIS project file.
#' @param output_dir Directory where all files will be copied. Defaults to a new folder
#'        with the same name as the QGIS project in the QGIS project's directory.
#' @param remove_old Logical, if TRUE, deletes old files after copying.
#'
#' @examples
#' # Example usage
#' qgis.filesort(input_path = "C:/Users/janni/Documents/00_eagle/UrbanFormAndSociety/20241023_Tauberbischofsheim/QGIS/20241119_TBB.qgz",
#'               output_dir = "",
#'               remove_old = FALSE)
#'
#' @importFrom xml2 read_xml xml_find_all xml_text xml_set_text write_xml
#' @importFrom utils unzip zip
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
    if (file.exists(output_dir) == FALSE) {
      dir.create(output_dir)
      print(paste("Folder created:", project_sans))
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
      print(paste("Definition Query caught in layer:", layerpath_old))
      layerpath_query <- paste0( "\\|", sub(".*\\|", "", layerpath_old))
      layerpath_old <- sub("\\|.*", "", layerpath_old)
    }
    # Adjust path in xml
    layerpath_file <- basename(layerpath_old)
    xml_set_text(layer, paste0("./", layerpath_file, layerpath_query))

    # Get dir and filename without extension for the current layer
    layerpath_dir <- file.path(project_dir, dirname(layerpath_old))
    layerpath_sans <- tools::file_path_sans_ext(layerpath_file)
    print(layerpath_sans)

    # Find file and all corresponding metadata files, copy them to new location
    layerlist <- list.files(path = layerpath_dir, pattern = paste0("^", layerpath_sans, "\\..+$"), full.names = TRUE, recursive = FALSE)
    for (layerfile in layerlist){
      layerpath_new <- paste0(output_dir, "/", basename(layerfile))
      file.copy(layerfile, layerpath_new)
      # Remove of file if requested and if file was successfully copied
      if(remove_old){
        if(file.exists(layerpath_new)){
          file.remove(layerfile)
        }
      }
    }
  }
  # Save xml, zip everything back up
  print("Reassembling QGIS project file...")
  write_xml(project_xml, project_new, options = "as_xml")
  zip(paste(output_dir, "/", project_file, sep=""), files = c(project_new, list.files(path = output_dir, pattern="_styles.db", full.names = TRUE)))
  # Delete the friends we made along the way
  file.remove(c(project_copy, project_new, list.files(path = output_dir, pattern="_styles.db", full.names = TRUE)))
  # Remove of file if requested and if file was successfully copied
  if(remove_old){
    if(file.exists(paste(output_dir, "/", project_file, sep=""))){
      file.remove(input_path)
    }
  }
}
