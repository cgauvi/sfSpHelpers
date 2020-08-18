test_that("valid + rm zm ", {


    shpTreesRaw <- st_read(dsn=here::here('Data', 'GeoData', 'QuebecCityOpenData', 'Trees','vdq-arbrepotentielremarquable.shp'))

    shpTrees <- makeSfValidRmZM(shpTreesRaw)

    assertthat::assert_that(  all( st_is_valid(shpTrees) ) )

    #No zm
    all( colnames( st_coordinates(shpTrees) %in% c("X","Y") ) )

    #geometry has no name
    assertthat::assert_that( is.null( names(st_geometry(shpTrees)) ) )

    #crs is the same as original object
    assertthat::are_equal( st_crs(shpTrees) , st_crs(shpTreesRaw) )

})
