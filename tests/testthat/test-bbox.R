test_that("bbox works", {

  library(sf)

  file_name <- system.file("shape/nc.shp", package="sf")
  nc <- sf::st_read(file_name)

  shpBbox <- bbox_polygon(nc)

  #plot(shpBbox)
  #plot(shpBuildings$geometry, add=T)

  assertthat::are_equal(sf::st_crs(shpBbox), sf::st_crs(nc))
  assertthat::assert_that( !is.null(  sf::st_contains(shpBbox,  sf::st_union(nc) ) [[1]] ) )
})
