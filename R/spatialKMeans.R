

#' Smooth out a large number of points or polygons by taking a spatially weighted k means
#'
#' @param shp
#' @param numCentroids
#' @param numClosestPoints - analoguous to bandwith - larger number reduces the variance
#' @param var - string - variable to summarise
#' @param aggFct - function to use to summarise - e.g. median or mean
#'
#' @return shpCentroids with column var
#' @export
#'
spatialKMeans <- function(shp,
                         numCentroids =NULL,
                         propToKeep=NULL,
                         numClosestPoints = 10,
                         var="ass_diff",
                         aggFct=median){

  #Make sure we only use 1 of the 2 arguments - either a raw numer of centroids OR a proportion
  stopifnot( sum( purrr::map_lgl(list(propToKeep, numCentroids), is.null)) %% 2 ==1 )

  #Mke sure the variable is present
  stopifnot(var %in% colnames(shp))

  if (is.null(numCentroids)){
    stopifnot(dplyr::between(propToKeep, 0, 1) )
    numCentroids <- ceiling(propToKeep*nrow(shp))
  }

  #Can only consider as many points as
  stopifnot(numCentroids <= nrow(shp))

  #Add points/centroids
  shp %<>% SfSpHelpers::getCentroids()


  #Try to smooth out the points by taking representative cluster centroids + nn
  matSingles <- shp[ , c('lng','lat')] %>%
    sf::st_set_geometry(NULL) %>%
    as.matrix()

  #Apply k-means
  kmeansResults <- kmeans(x = matSingles ,
                          centers = numCentroids)

  dfCentroids <-  kmeansResults$centers %>% dplyr::as_tibble()

  #Get the numClosestPoints closest points in the real dataset (with a ass_diff and other covariates)
  #matClosest is an nXk matrix where n is the number of rows in dfCentroids
  matClosest <- FNN::get.knnx( data= matSingles ,
                               query=dfCentroids ,
                               k = numClosestPoints)$nn.index

  #Compute the median covar
  dfCentroids[[var]] <- purrr::map_dbl( 1:nrow(matClosest),
                                          ~aggFct( shp[[var]][ matClosest[.x,] ] , na.rm=T) )

  #Convert back to shp
  shpCentroids <- dfCentroids %>%
    sf::st_as_sf(coords=c( "lng","lat") , crs=st_crs(shp))


  return(shpCentroids)
}
