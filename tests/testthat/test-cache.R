test_that("caching works - default path ", {


  shp_iso_1km <- cache_csv_sf_wrapper(name_str='qc_city_isochrone_1km',
                                   fun=get_isochrone_qc,
                                   buffer_km=1,
                                   id_name='osm_id')


  expect_true(nrow(shp_iso_1km) >1)
})



test_that("caching works - custom path ", {


  get_isochrone_qc <- function(buffer_km){
    shp_point <- data.frame(X=-71.22776107610193, Y=46.805060757836486) %>% sf::st_as_sf(coords=c('X','Y'),crs=4326)
    osm <- SfSpHelpers::osm_buffer_query_helper(shp_point,buffer_km = buffer_km)

    return(osm$osm_lines)
  }

  remove_dir <- F
  dir_path_custom <- here::here('cache')
  if(!dir.exists(dir_path_custom)){
    remove_dir <- T
    dir.create(dir_path_custom)
  }
  shp_iso_5km <- cache_csv_sf_wrapper(name_str='qc_city_isochrone_5km',
                                      fun=get_isochrone_qc,
                                      buffer_km=0.5,
                                      id_name='osm_id',
                                      path_dir=dir_path_custom)

  if(remove_dir) unlink(dir_path_custom,recursive = T)



  expect_true(nrow(shp_iso_5km) >1)
}
)
