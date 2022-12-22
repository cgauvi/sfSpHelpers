



test_that("scaling polygon", {

  file_name <- system.file("shape/nc.shp", package="sf")
  nc <- sf::st_read(file_name)

  nc_scaled <- scale_geom_df(nc, scale_factor = 0.5)

  expect_equal(sf::st_crs(nc_scaled), sf::st_crs(nc))
  expect_true(all(dim(sf::st_crs(nc_scaled)) == dim(sf::st_crs(nc))))
})

