
#' Fct that wraps st_read with a curl call
#'
#' Can read geojsons for instance
#'
#' Tries to download the file to dirToDownload/nameOfFile for persistance.
#' If this fails, it just downloads it to a tmp file.
#' If dirToDownload/nameOfFile already exists, then it just reads back the shp file.
#' nameOfFile is created from the url path
#'
#' @param url url adress of shp to download
#' @param dirToDownload directory where the file will be unzipped , can be "", in which case a temp dir is used
#'
#' @return shp
#' @export
st_read_remote <- function(url, dirToDownload= here::here('Data')){

  #Use a tmp dir if we do not want persistance
  if(dirToDownload=="")dirToDownload <- tempdir()

  destFile <- tryCatch({
    urlPath <- httr::parse_url(url)$path

    splitUrl <- strsplit(urlPath, split='/')
    destFile <- file.path(dirToDownload, splitUrl[[1]][[length(splitUrl[[1]])]])
  },error=function(e){
    print(glue::glue("Fatal error with url {url}\n{e}\nsaving to root: {here::here()}"))
    return( tempfile()  )
  })

  #Check if file already exists
  if(file.exists(destFile)){
    print('Shp file already exists...')
    return( sf::st_read(destFile) )
  }

  print('Downloading shp file...')

  #If not download it
  curl::curl_download(url = url,
                destfile = destFile )



  return( st_read(destFile) )


}

#' Download a (zipped) shp file from a url
#'
#' @param url
#' @param dirToDownload
#'
#' @return shp downloaded shp file
#' @export
get_zipped_remote_shapefile <- function(url, dirToDownload=here::here("Data")){


  h <- curl::new_handle()
  curl::handle_setopt(h, ssl_verifyhost = 0, ssl_verifypeer=0)


  pathRootNoExt <- tools::file_path_sans_ext( basename(url)  )

  if(grepl('http',url)){

    #Create the dirToDownload/subdir if required
    GeneralHelpers::createDirIfNotExistentRecursive(file.path(dirToDownload,pathRootNoExt))

    #Download the file
    dest <- file.path(dirToDownload, pathRootNoExt, glue::glue("{pathRootNoExt}.zip") )

    #Check if the shp exists
    shpPath <- file.path(dirToDownload, pathRootNoExt, glue::glue("{pathRootNoExt}.shp|geojson") )
    if ( file.exists( shpPath  )){
      return (  sf::st_read(shpPath,quiet=TRUE) )
    }

    curl::curl_download(url= url,
                  destfile = dest ,
                  handle = h)
    path <- dest
  }

  if(grepl('.tgz$|.tar.gz$',path)){
    utils::untar(path, exdir = file.path(dirToDownload, pathRootNoExt))
  } else if(grepl('.zip$',path)){
    utils::unzip(path, exdir = file.path(dirToDownload, pathRootNoExt))
  } else{
    stop('Unsupported filetype')
  }

  file.remove(path)

  shape_name <- grep('.shp|geojson',list.files(file.path(dirToDownload, pathRootNoExt)),value=T)
  setwd(file.path(dirToDownload, pathRootNoExt))
  sf::st_read(shape_name,quiet=TRUE)
}



