library(plyr)
library(parallel)
library(doParallel)
library(foreach)
library(ggplot2)

get_output <- function(path) {
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
        
  log_out <- read.csv("NECN-succession-log-short.csv")
  soil_out <- log_out[,c(1,3,4)]  # select the column numbers of interest
  biomass_as_C <- soil_out$AGB/2
  final_C_data <- cbind.data.frame(soil_out[,c(1,2)], biomass_as_C)
  # return outputs
  data.frame(Harvest=sum(harvest[, -c(1, 2)]), Soil_OrgMat=sum(final_C_data[nrow(final_C_data), -1]))
}

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
main_path <- setwd(work_path)
output <- foreach::foreach(k=1:length(ens_path), .packages = c('raster', 'plyr'), .combine='rbind') %dopar% {
  setwd(work_path)
  get_output(ens_path[k])
}
setwd(main_path)

stopCluster(cl)

out_strat <- cbind(output, strategy=as.factor(scenarios[, 1]))
out_strat$Soil_OrgMat <- out_strat$Soil_OrgMat / 1000

p <- ggplot(out_strat) +
  geom_point(aes(x=Harvest, y=Soil_OrgMat, color=strategy)) +
  scale_color_brewer('Harvesting Strategy', palette='Dark2', labels=c('BAU', 'BAU + 20%', 'BAU - 20%', 'EvenAged', 'UnevenAged')) +
  scale_x_continuous('Total Harvested Biomass, All Species (Mg)') +
  scale_y_continuous(expression('Sequestered Carbon (kg C/m^2)')) +
  theme_classic(base_size=12)

pdf('harvest_soil_plot.pdf', height=4, width=6)
p
dev.off()