



#### Valid polygons #####



#' Make sure polygons that intersect are treated  MULTIPOLYGONS
#'
#' Useful to deal with stats can shp files (which has many self-overlapping polygons)
#'
#' @param shp, sf object to clean
#'
#' @return shp, valid sf object
#' @export
#'
#' @examples
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

#' Wrapper for makeValid, st_zm, st_transform ...
#'
#'
#'
#' @param shp
#'
#' @return
#' @export
#'
#' @examples
makeSfValidProjRmZ <- function(shp){

  if( any(grepl("sf", class(shp) ) )){
    print("Making SF object valid + using CRS +init=epsg:4326 + droping z coord which fucks up leaflet")
    shp %<>% makeValid()
    shp %<>% st_transform(crs="+init=epsg:4326")  #projection
    shp %<>% st_zm()                              #remove the z values
    names(st_geometry(shp)) <- NULL               #Leaflet bug

    print(paste0("In makeSfValidProjRmZ, setting the coords to ", sf::st_crs(shp)))

  } else { print("Spatial object is " , paste(class(shp), collapse = ",", sep=","), " -- does not contain sf")}

  return(shp)

}



### Bbox ####

#' Create a st_polygon from a bounding box
#'
#' Really useful function to define bounding box enveloppe
#'
#' @param x sf or sfc object that we want to bound within some box
#' @param buffer dist to add around the bounding box to avoid the weird boundary effects
#'
#' @export
#'
#' @return st_polygon forming a box
#' @details See https://stackoverflow.com/questions/45719790/create-voronoi-polygon-with-simple-feature-in-r
#'
#' @examples   shpUnionedSchoolds %>% st_centroid() %>% bbox_polygon()
bbox_polygon <- function(x, buffer=0) {
  bb <- sf::st_bbox(x)

  #Alternatively, using sp syntax
  #p <- x %>% as("Spatial") %>% sp::bbox() %>% splancs::bboxx() #Bounding box with all 4 extreme points
  #p %<>% rbind(mat[1, ] ) #Close the polygon

  p <- matrix(
    c(bb["xmin"], bb["ymin"], #Create the bounding box => rectangle with 4 extreme points
      bb["xmin"], bb["ymax"],
      bb["xmax"], bb["ymax"],
      bb["xmax"], bb["ymin"],
      bb["xmin"], bb["ymin"]), #Close the polygon
    ncol = 2, byrow = T
  )

  if(buffer == 0 ) polyReturn <- sf::st_polygon(list(p)) #Convert list of points to polygon (need list here)
  else polyReturn <- sf::st_polygon(list(p)) %>% st_buffer(dist=buffer)

  return(polyReturn)
}




#' Get the long/lat of centroids of sf object
#'
#' @param shp, sf object
#'
#' @export
#'
#' @return shp with 2 new lng and lat columns
#'
getCentroids <- function(shp){



  #For reference: st_centroid(shpSchools) %>% pull(geometry) is equiv to st_geometry(st_centroid(shpSchools))
  centroidsCols <-  do.call( rbind, st_geometry(st_centroid(shp))) %>%
    as_tibble() %>%
    setNames(c("lng","lat"))

  if(!any(c("lng","lat") %in% colnames(shp))) shp %<>% bind_cols(centroidsCols)


  return(shp)


}

#' Create a buffer around an sf object
#'
#' @param shp, sf object
#' @param dist, distance (supposed to be in some valid unit)
#'
#' @export
#'
#' @return shpBuffSchools
#'
createBuffer <- function(shp, dist){

  shpBuff <- sf::st_buffer( shp , dist = dist) #seems dist is in km
  print(paste0("There are now ", nrow(shpBuff), " simple features after applying buffer with dist ", dist))

  return(shpBuff)
}


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

  shpBuff <- createBuffer(shp, dist)

  shpUnionedBuff <- st_union(shpBuff) %>%
    st_cast("POLYGON") %>%   #need this lin, otherwise we will get a multipolygon object
    st_sf

  print(paste0("There are now ", nrow(shpUnionedBuff), " simple features after applying buffer+union with dist ", dist))

  return(shpUnionedBuff)

}




convertLongLatToSf <- function(dfListings, listCoordColNames){

  shpListings <- st_as_sf(dfListings,
                          coords = listCoordColNames,
                          crs = 4326,
                          remove=F)  #keep the long,lat columns
  #For leaflet
  names(shpListings$geometry) <- NULL

  #Remove the Z coord
  shpListings %<>% st_zm()

  return(shpListings)
}




buldNeighList <- function(shp){

  shp %<>% as("Spatial")
  poly2nb(shp)

}


convertNeighToListWeights <- function(nb){

  nb2listw(nb)

}




getSfDf <- function(shp){
  stopifnot(any(grepl("sf", class(shp))))

  st_geometry(shp) <- NULL

  return(shp)
}



