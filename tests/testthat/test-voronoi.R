
test_that("voronoi interpolation on points", {

  require(dplyr)

  file_name <- system.file("shape/nc.shp", package="sf")
  nc_centroid <- sf::st_read(file_name) %>% sf::st_centroid()

  shpVoronoi <- getVoronoiInterpolation( nc_centroid,
                                         var="BIR79",
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


