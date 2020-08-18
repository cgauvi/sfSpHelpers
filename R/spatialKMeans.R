

#' Smooth out a large number of points or polygons by taking a spatially weighted k means
#'
#' @param shp
#' @param numCentroids
#' @param numClosestPoints - analoguous to bandwith - larger number reduces the variance
#' @param covar - string - variable to summarise
#' @param aggFct - function to use to summarise - e.g. median or mean
#'
#' @return shpCentroids with column covar
#' @export
#'
spatialKMeans <- function(shp,
                         numCentroids = 10**3,
                         propToKeep=NULL,
                         numClosestPoints = 10,
                         covar="ass_diff",
                         aggFct=median){

  stopifnot( sum(map_lgl(c(propToKeep, numCentroids), is.null)) %% 2 ==1 )

  #Can only consider as many points as
  stopifnot(numCentroids <= nrow(shp))

  #Add points/centroids
  shp %<>% SfSpHelpers::getCentroids()


  #Try to smooth out the points by taking representative cluster centroids + nn
  matSingles <- shp[ , c('lng','lat')] %>%
    st_set_geometry(NULL) %>%
    as.matrix()

  #Apply k-means
  kmeansResults <- kmeans(x = matSingles ,
                          centers = numCentroids)

  dfCentroids <-  kmeansResults$centers %>% as.tibble()

  #Get the numClosestPoints closest points in the real dataset (with a ass_diff and other covariates)
  matClosest <- FNN::get.knnx( data= matSingles ,
                               query=dfCentroids ,
                               k = numClosestPoints)$nn.index

  #Compute the median covar
  dfCentroids[[covar]] <- map_dbl( 1:nrow(matClosest),
                                   ~aggFct( shpQcCitySingles[[covar]][ matClosest[.x,] ] , na.rm=T) )

  #Convert back to shp
  shpCentroids <- dfCentroids %>%
    st_as_sf(coords=c( "lng","lat") , crs=proj102002)


  return(shpCentroids)
}
