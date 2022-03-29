

test_that("osm query steps - qc city", {

  require(magrittr)

  km_buffer <- 1

  shp_point <- data.frame(X=-71.22776107610193, Y=46.805060757836486) %>% sf::st_as_sf(coords=c('X','Y'),crs=4326)
  df_centroid <- shp_point %>% sf::st_coordinates() %>% as.data.frame

  opq_steps <- osm_buffer_query_helper(shp_point, buffer_km = km_buffer,   list_osm_params= list( list(key='highway', value='steps' ) ))


  #valid
  shp_buffer <- shp_point  %>%
    sf::st_transform(crs=3857) %>%
    sf::st_buffer(km_buffer*1000) %>%
    sf::st_transform(crs=4326)

  opq_steps_valid <- osmdata::opq(bbox = sf::st_bbox(shp_buffer) ) %>%
    osmdata::add_osm_feature(key='highway',value='steps')%>%
    osmdata::osmdata_sf ()

  assertthat::are_equal(opq_steps$overpass_call, opq_steps_valid$overpass_call)
  assertthat::are_equal(nrow(opq_steps$osm_lines), nrow(opq_steps_valid$osm_lines))
})



test_that("osm query cycle path - qc city", {

  require(magrittr)

  km_buffer <- 1

  shp_point <- data.frame(X=-71.22776107610193, Y=46.805060757836486) %>% sf::st_as_sf(coords=c('X','Y'),crs=4326)
  df_centroid <- shp_point %>% sf::st_coordinates() %>% as.data.frame

  #Since adding osm features corresponds to a logical AND and that cycleways are highways the number of rows should be the same
  opq_cycling <- osm_buffer_query_helper(shp_point, buffer_km = km_buffer,   list_osm_params= list(  list (key='cycleway') ) )
  opq_all <- osm_buffer_query_helper(shp_point, buffer_km = km_buffer,   list_osm_params= list(  list(key='highway'),  list (key='cycleway') ) )


  assertthat::are_equal(nrow(opq_cycling$osm_lines), nrow(opq_all$osm_lines))

})




test_that("isochrone all modes atlanta", {

  require(magrittr)

  shp_point <- data.frame(X=-84.33604, Y=33.76117) %>% sf::st_as_sf(coords=c('X','Y'),crs=4326)


  shp_isochrone <- st_make_isochrone_main (shp_point,
                       buffer_km = 1,
                       limit_minutes = c(5,10,15),
                       list_modes= c("motorcar","foot","bicycle"),
                       list_osm_params= list( list(key='highway' ) ))

  expect_equal( nrow(shp_isochrone) == 3*3) #3 modes and 3 time limits


})



test_that("isochrone bicycle montreal", {

  require(magrittr)


  shp_point <- data.frame(X=-73.57595721339615, Y=45.510545744510516) %>% sf::st_as_sf(coords=c('X','Y'),crs=4326)


  shp_isochrone <- st_make_isochrone_main (shp_point,
                           buffer_km = 1,
                           limit_minutes = c(5,10,15),
                           list_modes= c("bicycle"),
                           list_osm_params= list( list(key='highway')  ) ) #adding cycleway is ok only if we want ONLY the cycling paths -- se query test above -- this is a logical AND

  expect_equal( nrow(shp_isochrone), 3*1) #1 mode and 3 time limits
})




test_that("isochrone walking qc city", {

  require(magrittr)


  shp_point <- data.frame(X=-71.22776107610193, Y=46.805060757836486) %>% sf::st_as_sf(coords=c('X','Y'),crs=4326)
  df_centroid <- shp_point %>% sf::st_coordinates() %>% as.data.frame

  shp_isochrone <- st_make_isochrone_main (shp_point,
                           buffer_km = 5,
                           limit_minutes = c(5,10,15),
                           list_modes= c("foot"),
                           list_osm_params= list(  list(key='highway', value='path'))
                           )

  expect_equal( nrow(shp_isochrone) , 3*1) #1 mode and 3 time limits
})
