

convertCoordsToSf <- function(df, listCoordColNames,crs=4326){

  shp <- st_as_sf(df,
                          coords = listCoordColNames,
                          crs = crs,
                          remove=F)  #keep the long,lat columns
  #For leaflet
  names(shp$geometry) <- NULL

  #Remove the Z coord
  shp %<>% st_zm()

  print(glue::glue("The crs is {st_crs(shp)}"))

  return(shp)
}






