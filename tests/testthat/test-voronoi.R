
test_that("voronoi interpolation on points", {

  require(dplyr)

  shpBuildings <- sf::st_read('https://data.montreal.ca/dataset/d26fad0f-2eae-44d5-88a0-2bc699fd2592/resource/1c02ead8-f680-495f-9675-6dd18bd3cad0/download/logsoc_donneesouvertes_20191231.geojson')
  shpBuildings_sample <-shpBuildings %>%  sample_n(200)

  shpVoronoi <- getVoronoiInterpolation( shpBuildings_sample,
                                          var="an_orig",
                                          fctToAggregate =  median) #nas are removed by default in getVoronoiInterpolation

  expect_true( sum(is.na(shpVoronoi$an_orig)) ==0 )
}
)


test_that("voronoi interpolation on polygons", {

  file_name <- system.file("shape/nc.shp", package="sf")
  nc <- sf::st_read(file_name)

  shpVoronoi <- getVoronoiInterpolation( nc,
                                         var="BIR79",
                                         fctToAggregate = median ) #nas are removed by default in getVoronoiInterpolation

  expect_true( sum(is.na(shpVoronoi$BIR79)) ==0 )

}
)


