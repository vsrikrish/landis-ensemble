library(plyr)
library(parallel)
library(doParallel)
library(foreach)
library(vegan)
library(raster)
library(stringr)
library(dplyr)
library(ncdf4)

out_file <- "LANDIS_ensemble-out.nc"

# set path to overall ensemble location
work_path <- '/gpfs/group/kzk10/default/private/vxs914/landis-RDM' # raw model output

get_output <- function(scen) {
  idx <- paste(scen, collapse="-")
  path <- paste('LANDIS', idx, sep='-')
  setwd(path)
  
  # read in harvest output
  log_file <- file.path("harvest", "summary-log.csv")
  log_data <- read.csv(log_file)
  select_data <- log_data[,c(1,3,4,7,45:81)]  # select the column numbers of interest
  
  #head(select_data)
  #write.csv(select_data, paste0("Harvest_RDM_by_prescription_", Date, ".csv"))
  
  # summarize data and return
  harvest <- ddply(select_data,c("Time"),
                summarise, sum_HarvSites_ha=sum(HarvestedSites), 
                sum_Biomass_ha=sum(TotalBiomassHarvested),
                sum_Biomass_AmBeech_Mg=sum(BiomassHarvestedMg_AmBeech),
                sum_Biomass_BalsamFir_Mg=sum(BiomassHarvestedMg_BalsamFir),
                sum_Biomass_BalsamPoplar_Mg=sum(BiomassHarvestedMg_BalsamPoplar),
                sum_Biomass_BigToothAspen_Mg=sum(BiomassHarvestedMg_BigToothAspen),
                sum_Biomass_BittHickory_Mg=sum(BiomassHarvestedMg_BitternutHickory),
                sum_Biomass_BlackAsh_Mg=sum(BiomassHarvestedMg_BlackAsh),
                sum_Biomass_BlackCherry_Mg=sum(BiomassHarvestedMg_BlackCherry),
                sum_Biomass_BlackOak_Mg=sum(BiomassHarvestedMg_BlackOak),
                sum_Biomass_BlackSpruce_Mg=sum(BiomassHarvestedMg_BlackSpruce),
                sum_Biomass_BurOak_Mg=sum(BiomassHarvestedMg_BurOak),
                sum_Biomass_Butternut_Mg=sum(BiomassHarvestedMg_Butternut),
                sum_Biomass_Cottonwood_Mg=sum(BiomassHarvestedMg_Cottonwood),
                sum_Biomass_Elms_Mg=sum(BiomassHarvestedMg_Elms),
                sum_Biomass_GreenAsh_Mg=sum(BiomassHarvestedMg_GreenAsh),
                sum_Biomass_Hemlock_Mg=sum(BiomassHarvestedMg_Hemlock),
                sum_Biomass_Hophornbeam_Mg=sum(BiomassHarvestedMg_Hophornbeam),
                sum_Biomass_JackPine_Mg=sum(BiomassHarvestedMg_JackPine),
                sum_Biomass_PaperBirch_Mg=sum(BiomassHarvestedMg_PaperBirch),
                sum_Biomass_PinOak_Mg=sum(BiomassHarvestedMg_PinOak),
                sum_Biomass_QuakingAspen_Mg=sum(BiomassHarvestedMg_QuakingAspen),
                sum_Biomass_RedCedar_Mg=sum(BiomassHarvestedMg_RedCedar),
                sum_Biomass_RedMaple_Mg=sum(BiomassHarvestedMg_RedMaple),
                sum_Biomass_RedOak_Mg=sum(BiomassHarvestedMg_RedOak),
                sum_Biomass_RedPine_Mg=sum(BiomassHarvestedMg_RedPine),
                sum_Biomass_ServiceBerries_Mg=sum(BiomassHarvestedMg_ServiceBerries),
                sum_Biomass_ShagHickory_Mg=sum(BiomassHarvestedMg_ShagHickory),
                sum_Biomass_SilverMaple_Mg=sum(BiomassHarvestedMg_SilverMaple),
                sum_Biomass_SugarMaple_Mg=sum(BiomassHarvestedMg_SugarMaple),
                sum_Biomass_Tamarack_Mg=sum(BiomassHarvestedMg_Tamarack),
                sum_Biomass_WhiteAsh_Mg=sum(BiomassHarvestedMg_WhiteAsh),
                sum_Biomass_WhiteCedar_Mg=sum(BiomassHarvestedMg_WhiteCedar),
                sum_Biomass_WhiteOaks_Mg=sum(BiomassHarvestedMg_WhiteOaks),
                sum_Biomass_WhitePine_Mg=sum(BiomassHarvestedMg_WhitePine),
                sum_Biomass_WhiteSpruce_Mg=sum(BiomassHarvestedMg_WhiteSpruce),
                sum_Biomass_Willows_Mg=sum(BiomassHarvestedMg_Willows),
                sum_Biomass_YellowBirch_Mg=sum(BiomassHarvestedMg_YellowBirch))
  
  # get carbon sequestration data      
  log_out <- read.csv("NECN-succession-log-short.csv")
  soil_out <- log_out[,c(1,3,4)]  # select the column numbers of interest
  biomass_as_C <- soil_out$AGB/2
  final_C_data <- cbind.data.frame(soil_out[,c(1,2)], biomass_as_C)
  
  # get species extent metrics
  years_of_interest<- 1:100 #This is where you can specify the year of interest
  #Number of active cells
  active_cells<-96691
  
  #Then go through both matrices and do all the calculations for the years of interest.
  all_spp_matrix<-NULL
  sp_biomass_dir<- "output-biomass_rp" #directory of biomass rasters (reprojected)
  all_files<-list.files(sp_biomass_dir) #all the species biomass files +total biomass file.
  
  for (t in 1:length(years_of_interest)){
    each_year<-years_of_interest[t]
    
    all_Ref_year_files<-grep(paste0("-",each_year,".img"), all_files,value=T)
    sp_biomass_files_all<-grep("^TotalBiomass",all_Ref_year_files ,invert=T, value=T) #only the non "total biomass" files i.e. the species files.
    sp_biomass_files_img <- sp_biomass_files_all[grepl("img$", sp_biomass_files_all)]
    species_matrix<-NULL
    for (s in 1:length(sp_biomass_files_img)){#for every species biomass raster file within time step
      each_spp<-sp_biomass_files_img[s]
      each_spp_name<-sub("\\-.*", "", each_spp)
      spp_LANDIS_all<-as.data.frame(raster(file.path(sp_biomass_dir, each_spp)))#LANDIS unique spp biomass.
      #spp_LANDIS_all<-as.data.frame(raster(paste(dir,each_scenario,"/", each_spp,"-", each_time,".img",sep="")))#LANDIS unique spp biomass.
      colnames(spp_LANDIS_all)<-c("LANDIS_Biomass")
      spp_LANDIS_all[is.na(spp_LANDIS_all)]<-0 
      spp_LANDIS<-subset(spp_LANDIS_all, spp_LANDIS_all$LANDIS_Biomass>0)
      avg_biomass<-mean(as.numeric(spp_LANDIS$LANDIS))   #Units of g/m2
      extent <-length(as.numeric(spp_LANDIS$LANDIS))  #Total hectares of each species
      Total_biomass <-avg_biomass*extent #Total hectares of each species
      avg_landscape_biomass<-Total_biomass/active_cells
      LANDIS_spp_output_row<-cbind.data.frame(each_year, each_spp_name, avg_landscape_biomass, extent)
      all_spp_matrix<-rbind(all_spp_matrix, LANDIS_spp_output_row)
    } #closes species]
  }  #end of species loop  
  
  colnames(all_spp_matrix)<-c("Time", "Species", "Avg_Biomass_gm2", "Extent_ha")

  # Age Evenness
  age_matrix<-NULL
  
  cohort_stats_dir<-'cohort-outputs_rp' #directory of biomass rasters (reprojected)
  all_files<-list.files(cohort_stats_dir) #all the species biomass files +total biomass file.
  for (t in 1:length(years_of_interest)){
    each_year<-years_of_interest[t]
    all_Ref_year_files<-grep(paste0("-",each_year,".img"), all_files,value=T)
    all_Ref_year_files_img <- all_Ref_year_files[grepl("img$", all_Ref_year_files)]
    age_LANDIS_all<-as.data.frame(raster(file.path(cohort_stats_dir, all_Ref_year_files_img)))#LANDIS unique spp biomass.
    colnames(age_LANDIS_all)<-c("LANDIS_Age_Evenness")
    age_LANDIS_all[is.na(age_LANDIS_all)]<-0 
    age_LANDIS<-subset(age_LANDIS_all, age_LANDIS_all$LANDIS_Age_Evenness>0)
    avg_age<-mean(as.numeric(age_LANDIS$LANDIS_Age_Evenness))  
    LANDIS_age_output_row<-cbind.data.frame(each_year, avg_age)
    age_matrix<-rbind(age_matrix, LANDIS_age_output_row)
  } #closes age loop
  
  colnames(age_matrix)<-c("Time", "Age_Evenness")
  
  # get diversity metrics
  #This is where you can specify the year of interest
  years_of_interest<-c(50,100)
  
  #Then go through both matrices and do all the calculations for the years of interest.
  diversity_matrix<-NULL
  
  sp_biomass_dir <- "output-biomass_rp" #directory of biomass raster
  all_files<-list.files(sp_biomass_dir) #all the species biomass files +total biomass file.
  
  for (t in 1:length(years_of_interest)){
    each_year<-years_of_interest[t]
    
    all_Ref_year_files<-grep(paste0("-",each_year,".img"), all_files,value=T)
    sp_biomass_files_all<-grep("^TotalBiomass",all_Ref_year_files ,invert=T, value=T) #only the non "total biomass" files i.e. the species files.
    sp_biomass_files_img <- sp_biomass_files_all[grepl("img$", sp_biomass_files_all)]
    
    species_matrix<-NULL
    for (s in 1:length(sp_biomass_files_img)){#for every species biomass raster file within time step
      each_spp<-sp_biomass_files_img[s]
      spp<-as.vector(raster(file.path(sp_biomass_dir,each_spp)))#species raster       
      species_matrix<-cbind(species_matrix, spp)#cbind all the species together. 
      number_species<-specnumber(species_matrix)
      tree_species_diversity<-diversity(species_matrix)
      number_spp_nozeros<-subset(number_species, number_species>0)
      diversity_nozeros<-subset(tree_species_diversity, number_species>0)
      Mean_Number_spp<-mean(number_spp_nozeros)
      Mean_diversity<-mean(diversity_nozeros)
    }  #end of species loop  
    
    diversity_row<-cbind.data.frame(each_year,  Mean_Number_spp, Mean_diversity)  
    diversity_matrix<-rbind(diversity_matrix, diversity_row)  
  } # end of loop


    # return outputs
  list(Scenario=data.frame(harvest=scen[1], wind=scen[2], climate=scen[3], gdd=scen[4]), Harvest=harvest[, -c(1, 2)], Soil_OrgMat=final_C_data[nrow(final_C_data), -1], AvgBiomass=all_spp_matrix, AgeEvenness=age_matrix, Diversity=diversity_matrix)
}


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

# loop over paths in parallel and run script
main_path <- setwd(work_path)
output <- foreach::foreach(k=1:nrow(scenarios), .packages = c('raster', 'stringr', 'dplyr', 'plyr', 'vegan')) %dopar% {
  setwd(work_path)
  get_output(scenarios[k,])
}
setwd(main_path)

saveRDS(output, file="landis-output.rds")

