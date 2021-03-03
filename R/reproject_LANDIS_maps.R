library(raster)

reproject <- function(path) {
  setwd(path)
  
  #Adjust duration of sim here if needed.
  time_step<-seq(0, 100,by=1) 
  
  SetLandisCRS <- function (from, to) {
    extent(from) <- extent(to)
    projection(from) <- projection(to)
    return(from)
  }

  climate_map <- "ClimateMap_RDM.tif" #need to read in the original climate map to save the projection
  spatial_reference <- raster(climate_map) #rasterize the input file.
  
  # create directory for reprojected maps
  dir.create("output-biomass_rp") # species biomass maps
  dir.create("cohort-outputs_rp") # age eveneness maps
  dir.create("output-biomass-reclass_rp") # biomass reclass maps

  #Then reproject all the species maps in the output-biomass directory and put them in the output-biomass_rp directory.
  sp_biomass_dir <- "output-biomass" #directory of biomass raster
  all_sp_biomass_files <- list.files(sp_biomass_dir) #all the species biomass files +total biomass file.
  
  for (s in 1:length(all_sp_biomass_files)) { #for each species...
    spp_LANDIS<-all_sp_biomass_files[s]
    spp_LANDIS_eachRaster<-raster(file.path("output-biomass", spp_LANDIS))
    spp_LANDIS_newName<-paste("output-biomass_rp/", spp_LANDIS,sep="")
  
    projectedLandisOutputRaster <- SetLandisCRS(spp_LANDIS_eachRaster, spatial_reference)
    writeRaster(projectedLandisOutputRaster, spp_LANDIS_newName,datatype='INT4S', overwrite=TRUE)
  }
  
  #Then reproject all the reclass maps in the output-biomass-reclass directory and put them in the output-biomass-reclass_rp directory.
  FT_dir <- "output-biomass-reclass" # directory of biomass recall rasters
  all_FT_files <- list.files(FT_dir) #all the species biomass files +total biomass file.
 
   for (s in 1:length(all_FT_files)){ #for each species...
    FT_LANDIS<-all_FT_files[s]
    FT_LANDIS_eachRaster<-raster(file.path("output-biomass-reclass", FT_LANDIS))
    FT_LANDIS_newName<-paste("output-biomass-reclass_rp/", FT_LANDIS,sep="")
    
    projectedLandisOutputRaster <- SetLandisCRS(FT_LANDIS_eachRaster, spatial_reference)
    writeRaster(projectedLandisOutputRaster, FT_LANDIS_newName,datatype='INT4S', overwrite=TRUE)
   }
  
  #Then reproject all the EVEN  maps in the cohort-outputs/ directory and put them in the cohort-outputs_rp directory.
  CohortStats_dir <- "cohort-outputs/age-all-spp" #directory of biomass raster
  all_CS_files <- list.files(CohortStats_dir) #all the species biomass files +total biomass file.
  EVEN_Files <- grep("AGE-EVEN", all_CS_files,value=T)
  
  for (s in 1:length(EVEN_Files)) {
    CS_LANDIS <- EVEN_Files[s]
    CS_LANDIS_eachRaster<-raster(file.path("cohort-outputs/age-all-spp/", CS_LANDIS))
    CS_LANDIS_newName<-paste("cohort-outputs_rp/", CS_LANDIS,sep="")
    
    projectedLandisOutputRaster <- SetLandisCRS(CS_LANDIS_eachRaster, spatial_reference)
    writeRaster(projectedLandisOutputRaster, CS_LANDIS_newName,datatype='INT4S', overwrite=TRUE)
  }
}

#library(Rmpi)
library(parallel)
library(doParallel)
library(foreach)

# set path to overall ensemble location
work_path <- '/gpfs/group/kzk10/default/private/vxs914/landis-RDM' # raw model output

# set up MPI cluster
#print(parallel::detectCores())
ncpu <- parallel::detectCores()-1
#mp_type <- 'MPI'
#cl <- parallel::makeCluster(ncpu, type=mp_type)
cl <- parallel::makeCluster(ncpu)
doParallel::registerDoParallel(cl) # register parallel backend
parallel::clusterExport(cl, c('work_path'))

# set up indices and directory names
harvest <- seq(0, 4)
wind <- seq(0, 3)
climate <- seq(0, 6)
gdd <- seq(0, 2)
scenarios <- expand.grid(harvest, wind, climate, gdd)
ens_idx <- do.call(paste, c(scenarios, sep="-"))
ens_path <- paste('LANDIS', ens_idx, sep='-')

# loop over paths in parallel and run script

foreach::foreach(k=1:length(ens_path), .packages = c('raster')) %dopar% {
   main_path <- setwd(work_path) 
   print(ens_path[k])
   reproject(ens_path[k])
   setwd(main_path)
}

stopCluster(cl)
#mpi.quit()
