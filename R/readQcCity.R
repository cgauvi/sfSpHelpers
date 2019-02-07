

#' Read all the shp files in file.path("Data", "Rdata", "GeoData","QuebecCityOpenData")
#'
#' @param readSP, F by default (so we read in shp files as sf with st_read rather than rgdal::readOGR)
#' @param qcDirPath
#'
#' @return a named list of sf objects
#'
#'
#'@details names include:
#'"vdq-quartier",
#'"vdq-batiments",
#'"vdq-arbrepotentielremarquable",
#'"vdq-zonesaccessansfil"
#'
#'@example readQcCityData()[["vdq-quartier" ]] returns an sf object with the neighbourhood data
#'
#'@export
readQcCityData <- function(readSP=F, qcDirPath="/home/charles/Projects/SfSpHelpers", forceAbsPath=T){

  print(paste0("In readQcCityData => will save all files in subfolders of ", qcDirPath, " change param qcDirPath if you want to change this"))
  print(paste0(ifelse(forceAbsPath, "Using absolute path", "Using relative path")))

  pathQc=file.path(qcDirPath, "Data","GeoData","QuebecCityOpenData")

  #Check the directory path
  listSubDirPath <- list.dirs(path=pathQc,recursive = F)
  if(length(listSubDirPath)<1) {
    stop(paste0("Fatal error in readQcCityData - check qcDirPath, make sure no trailing / ", pathQc))
    GeneralHelpers::determineDirExistenceRecursively(pathQc)
  }

  listSubDirLayer <- purrr::map_chr(listSubDirPath, ~getShpLayerNameInDir(.x))
  listSubDirName <-  purrr::map(listSubDirPath, GeneralHelpers::getFurthestDirectory)

  rdataPathBase <- file.path(qcDirPath, "Data", "Rdata", "GeoData","QuebecCityOpenData")

  listLayers <- vector("list", length(listSubDirPath))

  for(l in 1:length(listSubDirName)){

    tryCatch({
      rdataPath <- file.path(rdataPathBase, listSubDirName[[l]])
      rdataName <- listSubDirName[[l]]

      shpPath <- listSubDirPath[[l]]
      shpLayer <- listSubDirLayer[[l]]

      listLayers[[l]] <- readShpFileWrapper(rdataPath,rdataName, forceAbsPath,
                                            shpPath, shpLayer,
                                            readSP)
    }, error=function(e){
      print(paste0("Fatal error in readQcCityData with ", l , " th file - error ", e))
    })

  }


  shpFileObj <- listLayers
  names(shpFileObj) <- listSubDirLayer

  return(shpFileObj)
}

