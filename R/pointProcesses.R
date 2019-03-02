convertSpPoints <- function(sp){
  sp <- as( as(sp , "Spatial"), "SpatialPoints")
}

convertPPP <- function(sp){

  if(any(grepl("ppp", class(sp)))) sp <- sp
  else sp <- as( convertSpPoints(sp), "ppp")

  return (sp)
}




simulateEnvPPP <- function(spPPP, fun, funName){


  envSimResults <- spatstat::envelope(as(spPPP, "ppp"),
                                      fun=fun,
                                      nsim=100)

  dfMelted <- as.data.frame(envSimResults) %>% dplyr::select(-theo) %>% melt( id = "r" )

  gSimPlot <- ggplot(dfMelted) +
    geom_point(aes(x=r, y=value,col=variable)) +
    ggtitle(paste0("Simulation of ", funName, "\nComparison with complete spatial randomness (CSR)"))

  (gSimPlot)


  return(list(plot=gSimPlot, dfResults= as.data.frame(envSimResults)))
}



computeOptimalBandwithIPP <- function(shpListings){

  #as.points(convertSpPoints(shpListings)@coords )
  points1 <- as.points(coordinates(convertSpPoints(shpListings)))
  points2 <- as.points(bbox(convertSpPoints(shpListings)))

  mseResults <- mse2d(points1, points2,
                      nsmse=1000,
                      range=0.005)

  dfResults <- as.data.frame(mseResults)



  print(paste0("The min h given the parameter space used is ", dfResults$h[[which.min(dfResults$mse)]] ,
               " with mse: ", dfResults$mse[[which.min(dfResults$mse)]]  ))

  plotHVsMSE <- ggplot( dfResults %>% top_n(n = 900) ) +
    geom_line(aes(x=h,y=mse)) +
    ggtitle("MSE less some constant filtered to keep only 90% largest h\n(Good since smallest 10% leads to terribly large mse)")

  (plotHVsMSE)

  listResults <- list( plot=plotHVsMSE,
                       dfResults=dfResults,
                       minH=dfResults$h[[which.min(dfResults$mse)]],
                       minMse=dfResults$mse[[which.min(dfResults$mse)]] )

  return(listResults)
}


fitKernelIPP <- function(shpListings){

  #Get the sp object
  spListings <- as(shpListings,"Spatial")

  #Get the bb
  pointsBB <- bbox(convertSpPoints(shpListings))

  #Construct some grid
  grid <- Sobj_SpatialGrid(spListings)

  bwResults <- computeOptimalBandwithIPP(shpListings)
  kernResults <- spkernel2d(spListings, bboxx(pointsBB) , h0 =  bwResults$minH, grid$SG@grid )  #important to use bboxx!

  kernResultsNorm <- kernResults/max(kernResults)

  gridKern <- SpatialGridDataFrame(grid$SG@grid, data = data.frame(kernResultsNorm))

  print(summary(gridKern))

  return(gridKern)

}


getContourLinesIPP <- function(shpListings){

  gridKern <- fitKernelIPP(shpListings)


  kernResultsNorm <- gridKern$kernResultsNorm

  grid <- Sobj_SpatialGrid(as(shpListings,"Spatial"))
  spPointsGrid <- SpatialPoints(grid)
  spObj <- SpatialPointsDataFrame ( coords= spPointsGrid@coords[, c("SG.x", "SG.y")] ,
                                    data=as.data.frame(kernResultsNorm),
                                    proj4string=CRS(st_crs(shpNeigh)$proj4string))

  spObj$lng <- spObj@coords[,"SG.x"]
  spObj$lat <- spObj@coords[,"SG.y"]


  #Check this out: https://gis.stackexchange.com/questions/168886/r-how-to-build-heatmap-with-the-leaflet-package
  #Build the contour lines
  #This is a real pain in the neck since it requires passing all x + y vals sorted + the corresponding nXm matrix where n and m are the number of distinct x and y
  dfCoordPlusZ <- cbind(spPointsGrid@coords[, c("SG.x", "SG.y")] , as.data.frame(kernResultsNorm))
  dfMat <- dfCoordPlusZ  %>%  spread( SG.y , kernResultsNorm) %>% dplyr::select(-SG.x)  #Watch out!! the order maters, use SG.y in spread otherwise we will transpose the matrix

  #This returns a list of "contours" where each contour has attributes 1-level (the z value) 2-x 3-y
  conLines <- contourLines(
    x=unique(spPointsGrid@coords[, "SG.x"]) %>% sort,
    y=unique(spPointsGrid@coords[,"SG.y"]) %>% sort,
    as.matrix(dfMat)
  )

  lvls <- as.factor(sapply(conLines, `[[`, "level"))  #sapply equiv to: map_dbl( conLines, "level")
  nlvls <- length(levels(lvls))

  #build polygons from the contourlines
  #get a list of polygons
  pgons <- lapply(1:length(conLines), function(i) Polygons(list(Polygon(cbind(conLines[[i]]$x, conLines[[i]]$y))), ID=i))
  spPolygonsContours <-  SpatialPolygons(pgons)

  return(list(spPolygonsContours=spPolygonsContours,
              nlvls=nlvls,
              lvls=lvls,
              kernResultsNorm=kernResultsNorm,
              spObj=spObj)
  )


}


