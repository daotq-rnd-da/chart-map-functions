#' Add a scale bar to a leaflet map
#'
#' @description This function adds a scale bar to a leaflet map object,
#' making maps more informative and user-friendly.
#' 
#' @author daotq
#' @param map A leaflet map object to which the scale bar will be added.
#' @param position Character string specifying the position of the scale bar
#'   on the map. Options include "bottomleft", "bottomright", "topleft",
#'   and "topright". Default is "bottomleft".
#'
#' @return A leaflet map object with the scale bar added.
#' @examples
#' AddScaleBarForMap(map)
#' AddScaleBarForMap(map, position = "topright")

#' Required library
library(leaflet)

#' Function
AddScaleBarForMap <- function(map, 
                              position = "bottomleft") {
  map <- map %>%
    addScaleBar(
      position = position ,
      options = scaleBarOptions(
        maxWidth = 100,
        # in pixels
        metric = TRUE,
        imperial = FALSE,
        updateWhenIdle = TRUE
      )
    )
  return(map)
}
