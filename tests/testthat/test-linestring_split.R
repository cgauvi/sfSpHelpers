test_that("remove_line_endpoints with meters works", {

  geojson <- '{
  "type": "FeatureCollection",
  "name": "sample",
  "features": [
  { "type": "Feature", "properties": { "COMID": 5329303 },
    "geometry": { "type": "LineString", "coordinates": [ [ -122.916097119649635, 38.229575873993497 ],
      [ -122.916154986316201, 38.229346873993904 ],
      [ -122.916676986315338, 38.228614207328235 ],
      [ -122.917430786314128, 38.227148873997294 ],
      [ -122.917488319647418, 38.226210273998674 ],
      [ -122.917371986314322, 38.22579827399926 ],
      [ -122.917400319647584, 38.224516407334704 ],
      [ -122.918995719645125, 38.223348274003115 ],
      [ -122.920127119643382, 38.223004607336975 ],
      [ -122.921171719641791, 38.222546407337802 ],
      [ -122.922186919640126, 38.221950807338601 ],
      [ -122.922795786305926, 38.221286674006421 ] ]
    }
  }
  ]
}'

  lines <- sf::st_transform(sf::st_read(geojson), 5070)
  length_line_m <- sf::st_length(lines) %>% units::drop_units()


  shp_success <- remove_line_endpoints(shp_lines = lines, id_col = "COMID" , min_distance_m_to_remove = 50)
  length_line_m_new <- sf::st_length(shp_success) %>% units::drop_units()

  dist_removed <- length_line_m-length_line_m_new

  expect_true( nrow(shp_success) > 0 )
  expect_true( dist_removed > 0 )
  expect_true( abs(dist_removed-50*2) < 10**-2 ) #remove 50 m segments at both endpoints

}
)



test_that("remove_line_endpoints with proportion works", {

  geojson <- '{
  "type": "FeatureCollection",
  "name": "sample",
  "features": [
  { "type": "Feature", "properties": { "COMID": 5329303 },
    "geometry": { "type": "LineString", "coordinates": [ [ -122.916097119649635, 38.229575873993497 ],
      [ -122.916154986316201, 38.229346873993904 ],
      [ -122.916676986315338, 38.228614207328235 ],
      [ -122.917430786314128, 38.227148873997294 ],
      [ -122.917488319647418, 38.226210273998674 ],
      [ -122.917371986314322, 38.22579827399926 ],
      [ -122.917400319647584, 38.224516407334704 ],
      [ -122.918995719645125, 38.223348274003115 ],
      [ -122.920127119643382, 38.223004607336975 ],
      [ -122.921171719641791, 38.222546407337802 ],
      [ -122.922186919640126, 38.221950807338601 ],
      [ -122.922795786305926, 38.221286674006421 ] ]
    }
  }
  ]
}'

  lines <- sf::st_transform(sf::st_read(geojson), 5070)
  length_line_m <- sf::st_length(lines) %>% units::drop_units()


  shp_success <- remove_line_endpoints(shp_lines = lines, id_col = "COMID" , min_proportion_remove = 0.1)
  length_line_m_new <- sf::st_length(shp_success) %>% units::drop_units()

  dist_removed <- length_line_m-length_line_m_new

  expect_true( nrow(shp_success) > 0 )
  expect_true( dist_removed > 0 )
  expect_true( abs(dist_removed- 0.1*2*length_line_m) < 10**-2 ) #remove 10% * 2 of total linestring at both endpoints

}
)




test_that("remove_line_endpoints fails when both meters and proportion are set", {

  geojson <- '{
  "type": "FeatureCollection",
  "name": "sample",
  "features": [
  { "type": "Feature", "properties": { "COMID": 5329303 },
    "geometry": { "type": "LineString", "coordinates": [ [ -122.916097119649635, 38.229575873993497 ],
      [ -122.916154986316201, 38.229346873993904 ],
      [ -122.916676986315338, 38.228614207328235 ],
      [ -122.917430786314128, 38.227148873997294 ],
      [ -122.917488319647418, 38.226210273998674 ],
      [ -122.917371986314322, 38.22579827399926 ],
      [ -122.917400319647584, 38.224516407334704 ],
      [ -122.918995719645125, 38.223348274003115 ],
      [ -122.920127119643382, 38.223004607336975 ],
      [ -122.921171719641791, 38.222546407337802 ],
      [ -122.922186919640126, 38.221950807338601 ],
      [ -122.922795786305926, 38.221286674006421 ] ]
    }
  }
  ]
}'

  lines <- sf::st_transform(sf::st_read(geojson), 5070)
  length_line_m <- sf::st_length(lines) %>% units::drop_units()


  testthat::expect_equal(
    r <- tryCatch({
      remove_line_endpoints(shp_lines = lines, id_col = "COMID" , min_proportion_remove = 0.1 , min_distance_m_to_remove = 5)
    },error=function(x){
      print(x)
      return(NA)
    })
    ,
    NA
  )

}
)
