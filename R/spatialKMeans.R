

#' Smooth/thin out a large number of points or polygons by taking a spatially weighted k means
#'
#' In other words:
#' 1) get k spatially representative points by performing k-means on a 2D matrix of lng lat
#' 2) for each of these centroids, aggregate some function (e.g. median) of the covariates var over the numClosestPoints closest points spatially to get an idea of the value of var in a neighbourhood
#'
#' @param shp
#' @param numCentroids number of representative points to take (use EITHER numCentroids or numClosestPoints)
#' @param numClosestPoints - analogous to bandwidth - larger number reduces the variance (use EITHER numCentroids or numClosestPoints)
#' @param var - string - variable to summarise
#' @param aggFct - function to use to summarise - e.g. median or mean
#' @param propToKeep
#' @param iter.max max number of iterations for kmeans
#'
#' @return shpCentroids with column var
#' @export
#'
spatialKMeans <- function(shp,
                          var,
                         numCentroids =NULL,
                         propToKeep=NULL,
                         numClosestPoints = 10,
                         aggFct=function(x) {median(x,na.rm = T) } ,
                         iter.max = 10**3){

  #Make sure we only use 1 of the 2 arguments - either a raw numer of centroids OR a proportion
  stopifnot( sum( purrr::map_lgl(list(propToKeep, numCentroids), is.null)) %% 2 ==1 )

  #Make sure the variable is present
  stopifnot(var %in% colnames(shp))

  if (is.null(numCentroids)){
    stopifnot(dplyr::between(propToKeep, 0, 1) )
    numCentroids <- ceiling(propToKeep*nrow(shp))
  }

  #Can only consider as many points as there are rows/observations
  stopifnot(numCentroids <= nrow(shp))

  #Add points/centroids
  shp %<>% SfSpHelpers::getCentroids()


  #Try to smooth out the points by taking representative cluster centroids + nn
  matSingles <- shp[ , c('lng','lat')] %>%
    sf::st_set_geometry(NULL) %>%
    as.matrix()

  #Apply k-means
  kmeansResults <- kmeans(x = matSingles ,
                          centers = numCentroids,
                          iter.max = iter.max)

  dfCentroids <-  kmeansResults$centers %>% dplyr::as_tibble()

  #Get the numClosestPoints closest points in the real dataset
  #matClosest is an nXk matrix where n is the number of rows in dfCentroids
  matClosest <- FNN::get.knnx( data= matSingles ,
                               query=dfCentroids ,
                               k = numClosestPoints)$nn.index

  #Compute the aggregating function (e.g. median or mean) over the desired covariates + only consider the numClosestPoints closest points spatially
  dfCentroids[[var]] <- purrr::map_dbl( 1:nrow(matClosest),
                                          ~aggFct( shp[[var]][ matClosest[.x,] ]) )

  #Convert back to shp
  shpCentroids <- dfCentroids %>%
    sf::st_as_sf(coords=c( "lng","lat") , crs=sf::st_crs(shp))


  return(shpCentroids)
}
