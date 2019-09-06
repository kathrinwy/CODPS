##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: subnational population projections
##
## Date created: 6 September 2019
## Last updated: 6 September 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

# Setup -------------------------------------------------------------------
setwd(output)

# Set a location on disk where results will be stored.
reg.pop.dir <- "regPOPsimulation"
data.dir <- file.path("G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata")

# Location file defining geographical disaggregation

location.file <- file.path(output, "BFAlocations.txt")
locations     <- read.delim(location.file.BFA)

# Locate files with historical age- and sex-specific population. Only population at the present time is required.

popM0.file <- file.path(data.dir, "BFApopM.txt")
popF0.file <- file.path(data.dir, "BFApopF.txt")
head(read.delim(popM0.file, check.names = FALSE))

# Use e0 male and female trajectories for BFA

e0Ftraj <- read.csv(paste0(output, "mye0trajs/F/ascii_trajectories_wide.csv"), skip = 1)
e0Mtraj <- read.csv(paste0(output, "mye0trajs/M/ascii_trajectories_wide.csv"), skip = 1)

# find BFA's column
grep("Burkina.Faso", colnames(e0Ftraj)) # results in 30

BFAe0Ftraj <- cbind(rep(562,nrow(e0Ftraj)), e0Ftraj[c(2,3,30)]) 
BFAe0Mtraj <- cbind(rep(562,nrow(e0Mtraj)), e0Mtraj[c(2,3,30)]) 

colnames(BFAe0Ftraj) <- c("LocID","Year","Trajectory","e0")
colnames(BFAe0Mtraj) <- c("LocID","Year","Trajectory","e0")

write.csv(BFAe0Ftraj,file= paste0(output, "regdata/BFAe0Ftraj.csv"), row.names = FALSE)
write.csv(BFAe0Mtraj,file= paste0(output, "regdata/BFAe0Mtraj.csv"), row.names = FALSE)

# If migration shares are specified, it is used to distribute the national migration taken out of
# the wpp2017 package. Note that such approach does not deal with the between-region migration.

# Generate subnational trajectories for all regions of one country.

sim.dir <- tempfile()  

regpop.pred <- pop.predict.subnat(present.year = 2015, 
                                  wpp.year = 2017, 
                                  output.dir = reg.pop.dir,
                                  locations  = locations,
                                  inputs     = list(popM        = file.path("regdata", "BFApopM.txt"),
                                                    popF        = file.path("regdata", "BFApopF.txt"),
                                                    tfr.sim.dir = BFAtfr.dir),
                                  verbose = TRUE,
                                  nr.traj = 200,
                                  replace.output=TRUE)

regpop.pred <- get.pop.prediction(reg.pop.dir)

get.countries.table(regpop.pred)

# Explore results.
pop.trajectories.plot(regpop.pred, "Boucle du Mouhoun", sum.over.ages = TRUE)

pop.pyramid(regpop.pred, "Boucle du Mouhoun", year = 2050)


