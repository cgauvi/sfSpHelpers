test_that("no weird bugs and projection and number of features stays the same", {

  file_name <- system.file("shape/nc.shp", package="sf")
  nc <- sf::st_read(file_name)


  nc_scaled <- scale_geom_df(nc,scale_factor = 0.75)

  assertthat::are_equal( nrow(nc_scaled), nrow(nc))
  assertthat::are_equal( sf::st_crs(nc_scaled), sf::st_crs(nc))

  # Nice little map
  #plot(nc$geometry )
  #plot(nc_scaled$geometry, add=T, col='red')



})


test_that("multipolygon works", {

  file_name <- system.file("shape/nc.shp", package="sf")
  nc <- sf::st_read(file_name)

  nc_unioned <- nc$geometry %>% sf::st_union() %>% sf::st_sf()

  nc_scaled <- scale_geom_df(nc_unioned,scale_factor = 0.75)

  assertthat::are_equal( nrow(nc_scaled), nrow(nc))
  assertthat::are_equal( sf::st_crs(nc_scaled), sf::st_crs(nc))

  # Nice little map
  # plot(nc_unioned$geometry )
  # plot(nc_scaled$geometry, add=T, col='red')



})
