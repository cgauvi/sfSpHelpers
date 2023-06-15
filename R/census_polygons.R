
#' Get the stats can url for 13 provinces & territories (2016)
#'
#' Uses the 2016 census dataset, but this is immaterial since no changes over time
#'
#' @param use_cartographic
#'
#' @return url (character)
#' @export
#'
get_prov_url <- function(use_cartographic) {

  if(use_cartographic){
    print('using cartographic boundary')
    url <- 'https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lpr_000b16a_e.zip'
  }else{
    print('using digital boundary')
    url <- 'https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lpr_000a16a_e.zip'
  }

  return(url)

}


#' Get FSA (first 3 digits of postal code) - 2021 data
#'
#' @param use_cartographic
#'
#' @return
#' @export
#'
#' @examples
get_fsa_url <- function(use_cartographic){

  if(use_cartographic){
    print('using cartographic boundary')
    url <- "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lfsa000b21a_e.zip"
  }
  else {
    url <- "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lfsa000a21a_e.zip"
  }

  return(url)


}

#' Get the stats can url for economic regions  (2021)
#'
#' @param use_cartographic
#'
#' @return url (character)
#' @export
#'
get_econ_reg_url <- function(use_cartographic) {


  if(use_cartographic){
    print('using cartographic boundary')
    url <- 'https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/ler_000b21a_e.zip'
  }else{
    print('using digital boundary')
    url <-  'https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/ler_000a21a_e.zip'
  }

  return(url)

}


#' Get the stats can url for census division (2021)
#'
#' @param use_cartographic
#'
#' @return url (character)
#' @export
#'
get_cd_url <- function(use_cartographic) {


  if(use_cartographic){
    print('using cartographic boundary')
    url <- 'https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lcd_000b21a_e.zipp'
  }else{
    print('using digital boundary')
    url <-  'https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lcd_000a21a_e.zip'
  }

  return(url)

}




#' Download the province of quebec cartographic shp file + cache results
#'
#' @param use_cartographic
#' @param data_dir
#'
#' @return url (character)
#' @export
#'
download_prov <- function(use_cartographic = T,
                          PR = "35",
                          dir_to_dl = here::here('data', 'pr'),
                          dir_cache = here::here('data'),
                          new_crs=NULL){



  # Set the cache
  if(is.null(new_crs)) new_crs_str <- 'NULL'
  else new_crs_str <- as.character(new_crs)

  if(is.null(PR)) PR_str <- 'NULL'
  else PR_str <- as.character(PR)

  cache_name <- glue::glue("prov_{PR_str}_carto_{use_cartographic}_crs_{new_crs_str}")
  if(!dir.exists(dir_cache)) dir.create(dir_cache, recursive = T)


  # Get ALL provinces boundary files from stats can
  shp_prov <- SfSpHelpers::cache_csv_sf_wrapper(
    name_str = cache_name,
    id_name = 'PRUID',
    fun = download_generic_wrappee,
    path_dir= dir_cache,
    url_getter = get_prov_url,
    PR=PR,
    dir_to_dl = dir_to_dl,
    use_cartographic = use_cartographic
  )


  return(shp_prov)
}




#' Download economic regions from the stats can website & filter on province
#'
#'
#'
#' Caches tmp files
#'
#' @param PR str. province ID - e.g. 35 (On) or 24 (Qc)
#' @param dir_to_dl
#' @param dir_cache
#' @param new_crs
#' @param use_cartographic bool
#'
#' @return shp_econ_regions sf object with economic regions for that province
#' @export
#'
download_econ_regions <- function(PR = "35",
                                  use_cartographic = F,
                                  dir_to_dl = here::here('data', 'er'),
                                  dir_cache = here::here('data', 'cache'),
                                  new_crs=NULL){


  # Set the cache
  if(is.null(new_crs)) new_crs_str <- 'NULL'
  else new_crs_str <- as.character(new_crs)

  if(is.null(PR)) PR_str <- 'NULL'
  else PR_str <- as.character(PR)

  cache_name <- glue::glue("er_pr_{PR_str}_carto_{use_cartographic}_crs_{new_crs_str}")
  if(!dir.exists(dir_cache)) dir.create(dir_cache, recursive = T)

  # Wrapper over
  shp_econ_regions <- SfSpHelpers::cache_csv_sf_wrapper(
    name_str = cache_name,
    id_name = 'ERUID',
    fun = download_generic_wrappee,
    path_dir  = dir_cache,
    url_getter = get_econ_reg_url,
    PR = PR,
    use_cartographic = use_cartographic,
    dir_to_dl = dir_to_dl,
    new_crs =new_crs
  )

  return(shp_econ_regions)

}





#' Download census division from the stats can website & filter on province
#'
#'
#'
#' Caches tmp files
#'
#' @param PR str. province ID - e.g. 35 (On) or 24 (Qc)
#' @param dir_to_dl
#' @param dir_cache
#' @param new_crs
#' @param use_cartographic bool
#'
#' @return shp_cd sf object with cd for that province - idname CDUID
#' @export
#'
download_cd <- function(PR = "35",
                        use_cartographic = F,
                        dir_to_dl = here::here('data', 'cd'),
                        dir_cache = here::here('data', 'cache'),
                        new_crs=NULL){


  # Set the cache
  if(is.null(new_crs)) new_crs_str <- 'NULL'
  else new_crs_str <- as.character(new_crs)

  if(is.null(PR)) PR_str <- 'NULL'
  else PR_str <- as.character(PR)

  cache_name <- glue::glue("cd_pr_{PR_str}_carto_{use_cartographic}_crs_{new_crs_str}")
  if(!dir.exists(dir_cache)) dir.create(dir_cache, recursive = T)

  # Wrapper over
  shp_cd <- SfSpHelpers::cache_csv_sf_wrapper(
    name_str = cache_name,
    id_name = 'CDUID',
    fun = download_generic_wrappee,
    path_dir  = dir_cache,
    url_getter = get_cd_url,
    PR = PR,
    use_cartographic = use_cartographic,
    dir_to_dl = dir_to_dl,
    new_crs =new_crs
  )

  return(shp_cd)

}





#' Download census division from the stats can website & filter on province
#'
#'
#'
#' Caches tmp files
#'
#' @param PR str. province ID - e.g. 35 (On) or 24 (Qc)
#' @param dir_to_dl
#' @param dir_cache
#' @param new_crs
#' @param use_cartographic bool
#'
#' @return shp_cd sf object with cd for that province - idname CDUID
#' @export
#'
download_fsa <- function(PR = NULL,
                         use_cartographic = F,
                         dir_to_dl = here::here('data', 'fsa'),
                         dir_cache = here::here('data', 'cache'),
                         new_crs=NULL){

  # Set the cache
  if(is.null(new_crs)) new_crs_str <- 'NULL'
  else new_crs_str <- as.character(new_crs)

  if(is.null(PR)) PR_str <- 'NULL'
  else PR_str <- as.character(PR)

  cache_name <- glue::glue("fsa_{PR_str}_carto_{use_cartographic}_crs_{new_crs_str}")
  if(!dir.exists(dir_cache)) dir.create(dir_cache, recursive = T)

  # Wrapper over
  shp_fsa <- SfSpHelpers::cache_csv_sf_wrapper(
    name_str = cache_name,
    id_name = 'CFSAUID',
    fun = download_generic_wrappee,
    path_dir  = dir_cache,
    url_getter = get_fsa_url,
    PR = PR,
    use_cartographic = use_cartographic,
    dir_to_dl = dir_to_dl,
    new_crs = new_crs
  )

  return(shp_fsa)

}




#' Code logic for `download_econ_regions``
#'
download_generic_wrappee <- function(url_getter,
                                     PR = "35",
                                     use_cartographic = F,
                                     dir_to_dl = here::here('data', 'cd'),
                                     new_crs=NULL){

  # Get the URL + unzip locally
  url_dl <- url_getter(use_cartographic)
  if(!dir.exists(dir_to_dl))dir.create(dir_to_dl)
  shp_generic  <- SfSpHelpers::get_zipped_remote_shapefile (url_dl, dirToDownload = dir_to_dl)


  # Filter on province if desired
  if(!is.null(PR)){
    shp_generic <- shp_generic %>% dplyr::filter(PRUID==PR)
  }

  # Spatial manips - reproject if desired
  ## CRS
  if(!is.null(new_crs)){
    shp_generic %<>% sf::st_transform(crs=new_crs)
  }

  ## Make valid
  shp_generic %<>% sf::st_make_valid()


  return(shp_generic)
}

