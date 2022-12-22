test_that("bbox works", {

  file_name <- system.file("shape/nc.shp", package="sf")
  nc <- sf::st_read(file_name)

  shpBbox <- bbox_polygon(nc)


  expect_equal(sf::st_crs(shpBbox), sf::st_crs(nc))
  expect_true( !is.null(  sf::st_contains(shpBbox,  sf::st_union(nc) ) [[1]] ) )
})



test_that("bbox from points works", {

  library(sf)
  library(dplyr)
  library(magrittr)

  file_name <- system.file("shape/nc.shp", package="sf")
  nc <- sf::st_read(file_name)

  coordinates <- nc %>% sf::st_coordinates() %>% as.data.frame()

  shpBbox <- bbox_from_points(coordinates$X, coordinates$Y,crs=st_crs(nc))
  shpBbox %<>% sf::st_transform(crs=3857) %>% sf::st_buffer(10**4 ) %>% sf::st_transform(crs=st_crs(nc)) #... the bounding box is pretty tight

  # plot(shpBbox)
  # plot(sf::st_union(nc),add=T)

  expect_equal(sf::st_crs(shpBbox), sf::st_crs(nc))
  expect_true( all( purrr::map_lgl( sf::st_within(nc, shpBbox  ) , ~!purrr::is_empty(.x))) )
})
