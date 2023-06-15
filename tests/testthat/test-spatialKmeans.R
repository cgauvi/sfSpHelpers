test_that("",{


  shp_nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))

  #No need to get the centroids, spatialKMeans does it automatically
  num_clusters <- 10
  var_to_agg <- "AREA"
  shp_nc_agg <- spatialKMeans (shp_nc,
                                    numCentroids = num_clusters,
                                    numClosestPoints = 10,
                                    var=var_to_agg,
                                    aggFct=function(x) median(x,na.rm=T))

  expect_equal(sum(is.na(shp_nc_agg[[var_to_agg]])) ,0)
  expect_equal(nrow(shp_nc_agg) , num_clusters)


})

