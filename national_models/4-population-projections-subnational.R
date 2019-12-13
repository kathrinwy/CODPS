# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: subnational projections

# Date created: 29 November 2019
# Last updated: 29 November 2019

# Author: Kathrin Weny
# Maintainers: Kathrin Weny, Romesh Silva

# Setup -------------------------------------------------------------------
setwd(output)

# Set a location on disk where results will be stored.
reg.pop.dir <- "regPOPsimulation"
data.dir    <- file.path(paste0(output, "regdata/"))

tfr.sim.dir <- paste0(iso, "tfr.dir")

# Location file defining geographical disaggregation
location.file <- file.path(data.dir, paste0(iso, "locations.txt"))
locations     <- read.delim(location.file)
locations

# Locate files with historical age- and sex-specific population. Only population at the present time is required.

popM0.file <- file.path(data.dir, paste0(iso, "popM.txt"))
popF0.file <- file.path(data.dir, paste0(iso, "popF.txt"))

# Use e0 male and female trajectories
e0Ftraj <- read.csv(paste0(output, "mye0trajs/F/ascii_trajectories_wide.csv"), skip = 1)
e0Mtraj <- read.csv(paste0(output, "mye0trajs/M/ascii_trajectories_wide.csv"), skip = 1)

e0Ftraj <- cbind(rep(country.code ,nrow(e0Ftraj)), e0Ftraj[c(2,3,grep(country, colnames(e0Ftraj)))]) 
e0Mtraj <- cbind(rep(country.code ,nrow(e0Mtraj)), e0Mtraj[c(2,3,grep(country, colnames(e0Ftraj)))]) 

colnames(e0Ftraj) <- c("LocID","Year","Trajectory","e0")
colnames(e0Mtraj) <- c("LocID","Year","Trajectory","e0")

write.csv(e0Ftraj,file= paste0(output, "regdata/", "e0Ftraj.csv"), row.names = FALSE)
write.csv(e0Mtraj,file= paste0(output, "regdata/", "e0Mtraj.csv"), row.names = FALSE)

# If migration shares are specified, it is used to distribute the national migration taken out of
# the wpp2017 package. Note that such approach does not deal with the between-region migration.
setwd(output)
# Generate subnational trajectories for all regions of one country.
regpop.pred <- pop.predict.subnat(present.year = 2015, 
                                  wpp.year = 2017, 
                                  output.dir = reg.pop.dir,
                                  locations  = locations,
                                  inputs     = list(popM        = file.path("regdata", paste0(iso, "popM.txt")),
                                                    popF        = file.path("regdata", paste0(iso, "popF.txt")),
                                                    tfr.sim.dir = tfr.dir),
                                  verbose = TRUE,
                                  nr.traj = 200,
                                  replace.output=TRUE)

regpop.pred <- get.pop.prediction(reg.pop.dir)

get.countries.table(regpop.pred)

setwd(code)
# Visual inspection
# region <- "Maradi"
# pop.trajectories.plot(regpop.pred, region, sum.over.ages = TRUE)
# pop.pyramid(regpop.pred, region, year = 2015)
