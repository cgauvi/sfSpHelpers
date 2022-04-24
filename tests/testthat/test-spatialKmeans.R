test_that("",{


  shpBuildings <- sf::st_read('https://data.montreal.ca/dataset/d26fad0f-2eae-44d5-88a0-2bc699fd2592/resource/1c02ead8-f680-495f-9675-6dd18bd3cad0/download/logsoc_donneesouvertes_20191231.geojson')


  #No need to get the centroids, spatialKMeans does it automatically
  num_clusters <- 50
  var_to_agg <- "an_orig"
  shpBuildingsAgg <- spatialKMeans (shpBuildings,
                                    numCentroids = num_clusters,
                                    numClosestPoints = 10,
                                    var=var_to_agg,
                                    aggFct=function(x) median(x,na.rm=T))

  expect_equal(shpBuildingsAgg$an_orig %>% is.na %>% sum() ,0)
  expect_equal(nrow(shpBuildingsAgg) , num_clusters)


})

