test_that("valid + rm zm ", {


    shpTreesRaw <- sf::st_read('https://www.donneesquebec.ca/recherche/dataset/bc5afddf-9439-4e96-84fb-f91847b722be/resource/bbdca0dd-82df-42f9-845b-32348debf8ab/download/vdq-arbrepotentielremarquable.geojson')

    shpTrees <- makeSfValidRmZM(shpTreesRaw)

    expect_true(  all( st_is_valid(shpTrees) ) )

    #No zm
    all( colnames( st_coordinates(shpTrees) %in% c("X","Y") ) )

    #geometry has no name
    expect_true( is.null( names(st_geometry(shpTrees)) ) )

    #crs is the same as original object
    expect_equal( st_crs(shpTrees) , st_crs(shpTreesRaw) )

})
