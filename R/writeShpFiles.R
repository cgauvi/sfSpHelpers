


writeStatCanQcCityDataShp <- function(year=2016){
  
  #Determine which year to use + where to save the resulting shp file
  if(year==2016){
    dataSetYear <- 'CA16'
    layerName <- "ldb_000b16a_e"
  } else if(year==2011){
    dataSetYear <- 'CA11'
    layerName <- "gdb_000b11a_e"
  } else if(year == 2006){
    dataSetYear <- 'CA06'
    layerName <- "gdb_000b06a_e"
  } else{
    stop(paste0("Fatal error in writeStatCanQcCityDataShp => current implementation only valid for year 2006, 2011 and 2016"))
  }
  
  
  #Read the shp file using cancensus api (also filter to get only qc city data)
  shpQcDB <- cancensus::get_census_geometry(dataset=dataSetYear, regions=list(CMA="24421"),
                                 level='DB') %>% 
    rename( StatCanShapeArea= `Shape Area` )
  
  #Get a valid shape (for instance if we have a self-crossing poolygon like the bowtie => convert to multipolygon)
  shpQcDB %<>% makeValid

  #Determine where to save the shp file
  absPathDir <- file.path(here::here() ,
                          "Data","GeoData","CanCensus",
                          "DB",layerName,"QcCity")
  
  #Check dir existence and create it if necessary using utils from AgencyFootprintPIV2Rpackage
  if(!dir.exists(absPathDir)) {
    AgencyFootprintPIV2Rpackage:::determineDirExistenceRecursively(absPathDir) #send in the absolute path
    print(paste0("Warning " ,  absPathDir, " does not exist => creating the directory + sub str" ))
    AgencyFootprintPIV2Rpackage:::createDirIfNotExistentRecursive(absPathDir)
  }
  
  #Actually write the shp file + use ESRI driver
  sf::st_write(shpQcDB,
               dsn=absPathDir,
               layer="QcCity",
               driver="ESRI Shapefile",
               delete_layer = T)        #Delete existing layer
  
}

