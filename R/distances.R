getDfLatLongCols <- function(df, id, hardCodedLng=NULL, hardCodedLat=NULL){

  if(any(grepl("sf", class(df)))) df <- getSfDf(df)

  lngColName <- ifelse(is.null(hardCodedLng), grep( "lon.*|lng.*", colnames(df), ignore.case = T, value = T), hardCodedLng)
  latColName <- ifelse(is.null(hardCodedLat), grep( "lat.*", colnames(df), ignore.case = T, value = T), hardCodedLat)

  stopifnot( length(lngColName) == 1 && length(latColName) ==  1)

  print("returning df[, c(id, latitude, longitude)] IN THAT ORDER!")

  maxLat <- max(df[[latColName]])
  minLat <- min(df[[latColName]])
  maxLng <- max(df[[lngColName]])
  minLng <- min(df[[lngColName]])

  #Need to have -90 <= lat <= 90 and -180 <= lng <= 180
  if( !((-90 <= minLat) & (maxLat <= 90) ) |
      !((-180 <= minLng) & (maxLng <= 180) ) ){
    stop(paste0("Fatal error, lng or lat not in valid range - here are the cols used - ",
                " lng: ",lngColName, " - lat: ",  latColName) )
  }

  return(df[, c(id, latColName, lngColName) ])

}


getCrossProd <- function(df1,df2){

  writeLines(
    paste0("Taking cross product of 2 df =>\ndf1 has ", nrow(df1), " rows and\n",
           "df2 has ", nrow(df2))
  )

  df1$dummy <- rep(1,nrow(df1))
  df2$dummy <- rep(1,nrow(df2))

  dfCartProd <- left_join( df1,
                           df2,
                           by="dummy")

  stopifnot(nrow(dfCartProd) == nrow(df1)*nrow(df2))

  return(dfCartProd)

}


getCrossProdWithDist <- function(shp1, shp2,
                                 id1, id2,
                                 lat1=NULL, lng1=NULL,
                                 lat2=NULL, lng2=NULL){

  df1 <- getDfLatLongCols(shp1,id=id1,lat1,lng1)
  df2 <- getDfLatLongCols(shp2,id=id2,lat2,lng2)


  #Check that the colnames are not the same! otherwise taking the cross prod with getCrossProd will create lng.x lng.y for e.g. which are not in the original df1 df2
  latLongnames1 <- colnames(df1)[colnames(df1)!=id1]
  latLongnames2 <- colnames(df2)[colnames(df2)!=id2]

  #Quick check
  if(any(colnames(df1) %in% colnames(df2))){
    print("Watch out, in getCrossProdWithDist => the long lat names are the same in both df! => changing one set of names")
    latLongnames1 <- paste0(latLongnames1,".x")
    latLongnames2 <- paste0(latLongnames2,".y")
  }


  #Take the cross product
  dfCrossProd  <- getCrossProd(df1, df2)

  #Quick check
  stopifnot(all(latLongnames1 %in% colnames(dfCrossProd)))
  stopifnot(all(latLongnames2 %in% colnames(dfCrossProd)))

  #Get the distance for each pair
  system.time(
    dfCrossProd$dist <- purrr::map_dbl(1:nrow(dfCrossProd),
                                       ~ distm ( dfCrossProd[.x, latLongnames1],
                                                 dfCrossProd[.x, latLongnames2])
    )
  )

  return(dfCrossProd)

}




getDistanceMatrix <- function(spPoints, extraStr){

  stopifnot(all(c("longitude","latitude") %in% colnames(spPoints)))

  #make sure we only have the longitude and latitude in a df
  if(any(grepl("sf", class(spPoints)))) st_geometry(spPoints) <- NULL

  spPoints %<>% dplyr::select(longitude, latitude)

  GeneralHelpers:::wrapperRdataWrite(distm,
                                     rdataPath=file.path("data","rdata") ,
                                     rdataName=paste0("distmatrix_", extraStr),
                                     spPoints[, c("longitude","latitude")])


}



getFurtestPoints <- function(distMatrix){

  listMaxDist <- map_dbl(1:nrow(distMatrix), ~max( distMatrix[.x , ]) )
  furtPoint1 <- which.max(listMaxDist)
  furtPoint2 <- which.max(distMatrix[furtPoint1, ])

  return(c(furtPoint1,furtPoint2))
}

