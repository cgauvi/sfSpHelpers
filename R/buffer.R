

#' Create a buffer around an sf object + merge the resulting polygons using st_union
#'
#' @param shp, sf object
#' @param dist, distance (supposed to be in some valid unit)
#'
#' @return shpUnionedBuff, sf object with potentially less features than shp
#'
#' @export
#'
unionShpBuffer <- function(shp, dist){

  shpBuff <- sf::st_buffer( shp , dist = dist)

  shpUnionedBuff <- st_union(shpBuff) %>%
    st_cast("POLYGON") %>%   #need this lin, otherwise we will get a multipolygon object
    st_sf(crs=st_crs(shp))

  print(paste0("There are now ", nrow(shpUnionedBuff), " simple features after applying buffer+union with dist ", dist))

  return(shpUnionedBuff)

}


