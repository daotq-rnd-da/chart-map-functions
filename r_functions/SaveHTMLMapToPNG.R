#' Save a leaflet map as PNG
#'
#' @desciption This function saves a leaflet map object as a PNG image.
#' The output image can be customized in terms of resolution and control visibility.
#' 
#' @author daotq  
#' @param map A leaflet map object to be saved as PNG.
#' @param file_name Character string specifying the output file name. Default is "my_map.png".
#' @param width Numeric value for the capture width (in pixels). Default is 1200.
#' @param height Numeric value for the capture height (in pixels). Default is 900.
#'
#' @return A PNG file saved to the specified location.
#' @examples
#' SaveHTMLMapToPNG(map)
#' SaveHTMLMapToPNG(map, file_name = "custom_map.png", width = 1600, height = 1200)

SaveHTMLMapToPNG <- function(map, 
                             file_name = "my_map.png", 
                             width = 1200, 
                             height = 900) {
  
  library(mapview)
  library(webshot2)
  
  # Save the map as a PNG file
  # width and height define the capture size (resolution)
  mapview::mapshot(map, 
                   file = file_name, 
                   vwidth = width, 
                   vheight = height,
                   remove_controls = c("zoomControl", "layersControl")) # Hide controls for a cleaner image
  
  message(paste("Map successfully saved at:", file_name))
}
