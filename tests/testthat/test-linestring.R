test_that("linestring end points works", {

  require(magrittr)

  # see https://github.com/r-spatial/sf/issues/321

  pts_sf <- data.frame(
    x = seq(47, 48, by=0.1),
    y = seq(147, 148, by=0.1),
    attr_data = rnorm(11,42,42),
    id = c(rep("fred",6), rep("wilma",5))
  ) %>%
    sf::st_as_sf(coords = c("x","y")) %>%
    sf::st_set_crs(4326)

  shp_segments <- pts_sf %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(m = mean(attr_data)) %>%
    sf::st_cast("LINESTRING")


  shp_segments_with_endpoints <- st_linestring_add_endpoints(shp_segments)

  expect_equal(shp_segments_with_endpoints$start_x[[1]] ,  47.0 )
  expect_equal(shp_segments_with_endpoints$start_y[[2]] ,   147.6  )
  expect_equal(shp_segments_with_endpoints$end_x[[1]] ,    47.5 )
  expect_equal(shp_segments_with_endpoints$end_y[[2]] ,    148.0)
})
