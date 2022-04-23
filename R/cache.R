

#' Cache function that works with csvs of sf (SHP) files
#'
#' Send in a unique name to save the object and a function that will create it
#' along with its parameters.
#'
#' Also possible to change the path directory otherwise will use the default one
#'
#' Will zip the files (including all shp shx prj dbf in the case of shp files)
#'
#' Writes the rectangular dataframe to a separate csv to avoid the name mangling
#' nonsense associated with ESRI shp and then merges everything together using
#' the id_name column. This means you need to know the dataset in advance, but
#' should not be an issue if using census data - GEOID for us and GeoUID for can.
#'
#' @param name_str
#' @param fun
#' @param path_dir
#' @param ...
#' @param id_name
#'
#' @return fun(...) - and results cached to a zip file
#' @export
#'
#' @examples
#' \dontrun{
#'
#' get_isochrone <- function(buffer_km){
#'  shp_point <- data.frame(X=-71.22776107610193, Y=46.805060757836486) %>% sf::st_as_sf(coords=c('X','Y'),crs=4326)
#'  osm <- SfSpHelpers::osm_buffer_query_helper(shp_point,buffer_km = buffer_km)
#'
#'  return(osm$osm_lines)
#' }
#'
#'
#' shp_iso_5km <- cache_csv_sf_wrapper(name_str='qc_city_isochrone_5km',
#'                                   fun=get_isochrone,
#'                                   buffer_km=5,
#'                                   id_name='osm_id',
#'                                   path_dir=here('cache'))
#'
#' shp_iso_1km <- cache_csv_sf_wrapper(name_str='qc_city_isochrone_1km',
#'                                  fun=get_isochrone,
#'                                  buffer_km=1,
#'                                  id_name='osm_id')
#' }
cache_csv_sf_wrapper <- function(name_str,
                                 fun,
                                 id_name='GEOID',
                                 path_dir=NA,
                                 ...){

  require(magrittr)
  require(glue)
  require(readr)
  require(dplyr)
  require(sf)
  require(purrr)

  assertthat::assert_that(!is.null(name_str) & !is.na(name_str) & nchar(name_str) >0, msg='Fatal error! no file name provided for cache!')

  #Nested function to get all the files created by ESRI shp
  get_sf_files <- function(name_str){
    paste0(name_str, c('.shx','.dbf','.prj','.shp'))
  }

  #Remove extension if present
  name_str %<>% tools::file_path_sans_ext()

  #Get the path to the cache
  if(is.na(path_dir)){
    path_dir <- file.path (Sys.getenv('HOME'), '.cache' )
    if(!dir.exists(path_dir))dir.create(path_dir)
    print( paste0('Using default cache: ' , path_dir, ' ...') )
  }


  path_csv <- file.path(path_dir, glue('{name_str}.csv'))
  path_shp <- file.path(path_dir, glue('{name_str}.shp'))
  path_all_shp <- map_chr(get_sf_files(name_str), ~file.path(path_dir,.x))

  path_zip <- file.path(path_dir, glue('{name_str}.zip'))

  # Get the county data if not already cached
  if (!file.exists(path_zip)){
    print(glue('{path_zip} does not exist yet - running {as.character(substitute(fun))} ...'))

    shp_geo <-  fun(...)

    if(any(class(shp_geo) == 'sf')){
      write_csv(shp_geo %>% st_drop_geometry(), path_csv)
      st_write(obj = shp_geo %>% select(all_of(c(id_name, "geometry"))),
               dsn=path_shp,
               append=F)
      list_to_zip <- c(path_all_shp , path_csv)
      print(glue('Saving results: \n{path_csv}\n{path_shp}...'))
    }else{
      write_csv(shp_geo , path_csv)
      list_to_zip <- c(path_csv)
      print(glue('Saving results: (no geometry column) \n{path_csv}...'))
    }

    #zip
    zip(zipfile = path_zip,
        files = list_to_zip,
        flags = '-r9Xj')    # only zip the file, not the entire parent directory list

    #remove
    for(f in list_to_zip) unlink(f)

  }else{

    #unzip to the cache dir
    unzip(path_zip,exdir = path_dir )

    #read
    is_sf <- F
    if(file.exists(path_shp)){
      shp_geo <- st_read(path_shp)

      print(glue('Reading existing csv: \n{path_csv}\n{path_shp}...'))
      is_sf <- T
    }
    df_geo <- read_csv(path_csv)

    #Merge back with the original data
    if(is_sf){
      df_geo[[id_name]] <- as.character(df_geo[[id_name]])
      shp_geo[[id_name]] <- as.character(shp_geo[[id_name]])
      shp_geo %<>% left_join(df_geo , by=id_name)
    }else{
      shp_geo <- df_geo
    }

    # remove unzipped
    if(is_sf) {
      for (f in path_all_shp) unlink(f)
    }
    unlink(path_csv)

  }

  return(shp_geo)

}

