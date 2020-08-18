
#' Make sure polygons that intersect are treated  MULTIPOLYGONS
#'
#' Useful to deal with stats can shp files (which has many self-overlapping polygons)
#'
#' @param shp, sf object to clean
#'
#' @return shp, valid and cleaned sf object
#' @export
makeValid <- function(shp){

  listValid <- st_is_valid(shp)
  listIdxInvalid <- which( listValid == F)

  if(all(listValid)) print("Ok, all polygons are valid")
  else{
    writeLines (
      paste0("There are ", sum(!listValid), " invalid polygons: \n" ,
             paste(
               paste0(listIdxInvalid, " : " , sf::st_is_valid(shp[listIdxInvalid,], reason=T) ),
               collapse="\n")
      )
    )

    print("here is a sample problematic polygon")
    plot(shp$geometry[listIdxInvalid[[1]]])

    shp[listIdxInvalid, ] <- lwgeom::st_make_valid( shp[listIdxInvalid, ] )

    if( sum(!st_is_valid(shp)) == 0) print("Ok polygon should now be valid")
  }

  return(shp)

}

#' Wrapper for makeValid, st_zm,
#' @param shp valid and cleaned sf object
#'
#' @return shp, cleaned shp
#' @export
makeSfValidRmZM <- function(shp){

  if( any(grepl("sf", class(shp) ) )){
    print("Making SF object valid + using CRS +init=epsg:4326 + droping z coord which fucks up leaflet")
    shp %<>% makeValid()
    shp %<>% st_zm()                              #remove the z values
    names(st_geometry(shp)) <- NULL               #Leaflet bug

    print(paste0("In makeSfValidProjRmZ, crs is ", paste(st_crs(shp),sep=",",collapse = ",")))

  } else { print("Spatial object is " , paste(class(shp), collapse = ",", sep=","), " -- does not contain sf")}

  return(shp)

}

