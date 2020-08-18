test_that("multiplication works", {

  library(sf)

  shpBuildings <- st_read(dsn=here::here('Data', 'GeoData', 'QuebecCityOpenData', 'Trees','vdq-arbrepotentielremarquable.shp'))

  shpBbox <- bbox_polygon(shpBuildings)

  plot(shpBbox)
  plot(shpBuildings$geometry, add=T)

  assertthat::are_equal(st_crs(shpBbox), st_crs(shpBuildings))
})
