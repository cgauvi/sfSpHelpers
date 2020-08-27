test_that("st_intersects", {

  data(meuse, package = "sp")
  meuse_sf = sf::st_as_sf(meuse, coords = c("x", "y"), crs = 28992, agr = "constant")
  meuse_sf_subset <- meuse_sf[ runif(5, 1, nrow(meuse_sf)), ]


  shpFiltered <- st_generic_filter(meuse_sf,
                                   meuse_sf_subset %>%   sf::st_buffer(dist=10),  #use a buffer to make sure we'll touch
                                   sf::st_intersects)

  expect_equal(nrow(shpFiltered), 5)
})






test_that("st_within", {

  data(meuse, package = "sp")
  meuse_sf = sf::st_as_sf(meuse, coords = c("x", "y"), crs = 28992, agr = "constant")
  meuse_sf_subset <- meuse_sf[ runif(5, 1, nrow(meuse_sf)), ]


  shpFiltered <- st_generic_filter(meuse_sf,
                    meuse_sf %>%  bbox_polygon () %>% sf::st_buffer(50) ,
                    sf::st_within)

  expect_equal(nrow(shpFiltered), nrow(meuse_sf))
})


# Wrappers



test_that("st_intersects wrapper", {

  data(meuse, package = "sp")
  meuse_sf = sf::st_as_sf(meuse, coords = c("x", "y"), crs = 28992, agr = "constant")
  meuse_sf_subset <- meuse_sf[ runif(5, 1, nrow(meuse_sf)), ]


  shpFiltered <- st_intersects_filter(meuse_sf,
                                   meuse_sf_subset %>%   sf::st_buffer(dist=10) )

  expect_equal(nrow(shpFiltered), 5)
})




test_that("st_within - wrapper", {

  data(meuse, package = "sp")
  meuse_sf = sf::st_as_sf(meuse, coords = c("x", "y"), crs = 28992, agr = "constant")
  meuse_sf_subset <- meuse_sf[ runif(5, 1, nrow(meuse_sf)), ]


  shpFiltered <- st_within_filter(meuse_sf,
                                   meuse_sf %>%  bbox_polygon () %>% sf::st_buffer(50) )

  expect_equal(nrow(shpFiltered), nrow(meuse_sf))
})
