#library(Rmpi)
library(parallel)
library(doParallel)
library(foreach)

# change file path below for a particular output processing script
script_file <- 'R/Harvest_comparisons_RDM.R'
source(script_file)
# provide the name of the function in the output script that will be called on each ensemble member
# this function should take in the ensemble member subpath and any other inputs
out_fun <- 'harvest_summarize'

# uncomment below lines and change file path if there is output to be written to a file
output_file <- 'output/out.csv'
# output_headers <- c('Mean', 'Shannon')

# set path to overall ensemble location
work_path <- '/gpfs/group/kzk10/default/private/vxs914/landis-RDM' # raw model output

# set up MPI cluster
print(parallel::detectCores())
ncpu <- 20
#mp_type <- 'MPI'
#cl <- parallel::makeCluster(ncpu, type=mp_type)
cl <- parallel::makeCluster(ncpu)

# set up indices and directory names
harvest <- seq(0, 4)
wind <- seq(0, 3)
climate <- seq(0, 6)
gdd <- seq(0, 2)
scenarios <- expand.grid(harvest, wind, climate, gdd)
ens_idx <- do.call(paste, c(scenarios, sep="-"))
ens_path <- paste('LANDIS', ens_idx, sep='-')

# loop over paths in parallel and run script
# uncomment below lines if the output needs to be saved
output <- foreach::foreach(k=1:length(ens_path)) %dopar% {
  main_path <- setwd(work_path)
  out <- match.fun(out_fun)(ens_path[k])
  setwd(main_path)
  out
}
#write.csv(output, output_file, col.names=output_headers)
write.csv(output, output_file)
doParallel::registerDoParallel(cl) # register parallel backend
reproject
# uncomment below line if no output needs to be saved
# foreach::foreach(k=1:length(ens_path), .packages = c('raster')) %dopar% {
#   setwd(work_path) 
#   reproject(ens_path[k])
# }

stopCluster(cl)
#mpi.quit()