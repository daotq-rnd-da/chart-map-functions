#' @title Save a leaflet html map as PNG
#'
#' @desciption This function saves a leaflet map object as a PNG image.
#' The output image can be customized in terms of resolution and control visibility.
#' @author daotq  
#' 
#' @param map The map object (Leaflet/Mapview).
#' @param folder_name Character. The subfolder in your project to save the file. Defaults to "outputs".
#' @param file_name Character. The name of the output PNG file. Defaults to "my_map.png".
#' @param width Numeric. The width of the captured image. Defaults to 1200.
#' @param height Numeric. The height of the captured image. Defaults to 900.
#'
#' @export
SaveHTMLMapToPNG <- function(map, 
                             folder_name = "visuals",
                             file_name = "my_map.png", 
                             width = 1200, 
                             height = 900) {
  
  # Setup
  if (!require("pacman")) install.packages("pacman")
  # mapview and webshot2 are required for mapshot
  pacman::p_load(mapview, webshot2, here)
  
  # Define path 
  ## Ensure the target directory exists
  if (!dir.exists(here::here(folder_name))) {
    dir.create(here::here(folder_name), recursive = TRUE)
  }
  
  ## Construct the full path
  full_path <- here::here(folder_name, file_name)
  
  # Export the map
  mapview::mapshot(
    map, 
    file = full_path, 
    vwidth = width, 
    vheight = height,
    remove_controls = c("zoomControl", "layersControl", "homeButton", "drawControl")
  )
  
  message(paste("Map successfully saved at:", full_path))
}
