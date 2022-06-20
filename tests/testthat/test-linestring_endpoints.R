test_that("linestring end points works", {

  require(magrittr)

  # see https://github.com/r-spatial/sf/issues/321

  pts_sf <- data.frame(
    x = seq(-71.5, -71.0, by=0.1),
    y = seq(45.5, 46.0, by=0.1),
    attr_data = rnorm(6,42,42),
    id = c(rep("fred",2), rep("wilma",4))
  ) %>%
    sf::st_as_sf(coords = c("x","y")) %>%
    sf::st_set_crs(4326)

  shp_segments <- pts_sf %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(m = mean(attr_data)) %>%
    sf::st_cast("LINESTRING")


  shp_segments_with_endpoints <- st_linestring_add_endpoints(shp_segments)

  expect_equal(shp_segments_with_endpoints$start_x[[1]] , -71.5  )
  expect_equal(shp_segments_with_endpoints$end_x[[1]] ,   -71.4 )

  expect_equal(shp_segments_with_endpoints$start_y[[2]] , 45.7 )
  expect_equal(shp_segments_with_endpoints$end_y[[2]] ,   45.9)
})



test_that("st_sf_linestring_from_points works ",{


  pts_sf <- data.frame(
    x = seq(-71.5, -71.0, by=0.1),
    y = seq(45.5, 46.0, by=0.1),
    attr_data = rnorm(6,42,42),
    id = c(rep("fred",2), rep("wilma",4))
  ) %>%
    sf::st_as_sf(coords = c("x","y")) %>%
    sf::st_set_crs(4326)




  shp_segments <- pts_sf %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(m = mean(attr_data)) %>%
    sf::st_cast("LINESTRING")


  shp_segments_with_endpoints <- st_linestring_add_endpoints(shp_segments)

  expect_equal( nrow(st_sf_linestring_from_points(shp_segments_with_endpoints %>% st_drop_geometry())) ,2)
  expect_error( st_sf_linestring_from_points(shp_segments_with_endpoints)) #takes in a df - NOT an sf object
})



test_that("linestring creates sf", {

  require(magrittr)
  require(dplyr)
  require(sf)

  df_segment <- data.frame ( wkt= c('LINESTRING (737897.2 3774568, 737915.6 3774566, 737954.5 3774569, 738090.2 3774566, 738098.5 3774565, 738121.3 3774562, 738143.6 3774562, 738168.1 3774565)',
                                    'LINESTRING (737862.9 3775720, 737887.7 3775741, 737924.5 3775775, 737946.6 3775804, 737963 3775821, 737983.9 3775837, 738033.2 3775858, 738053 3775865, 738076.8 3775867, 738115.8 3775872, 738141.6 3775872, 738144.5 3775872)')
  )
  shp_sements <- st_as_sf(df_segment, wkt='wkt'  ,crs="+proj=utm +zone=16 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") %>%
    rename(geometry=wkt)

  shp_sements_endpoints <- st_linestring_to_sf_linestring_endpoints( shp_sements)

  expect_equal(nrow(shp_sements_endpoints),  2 )
  expect_true(all( lapply(shp_sements_endpoints$geometry, length) ==4 )  ) # 2 endpoints with lat and lng

})

