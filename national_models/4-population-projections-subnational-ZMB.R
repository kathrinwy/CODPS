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
data.dir <- file.path("G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata")

# Location file defining geographical disaggregation

location.file <- file.path(output, "regdata/ZMBlocations.txt")
locations     <- read.delim(location.file)

# Locate files with historical age- and sex-specific population. Only population at the present time is required.

popM0.file <- file.path(data.dir, "ZMBpopM.txt")
popF0.file <- file.path(data.dir, "ZMBpopF.txt")
head(read.delim(popM0.file, check.names = FALSE))

# Use e0 male and female trajectories for ZMB

e0Ftraj <- read.csv(paste0(output, "mye0trajs/F/ascii_trajectories_wide.csv"), skip = 1)
e0Mtraj <- read.csv(paste0(output, "mye0trajs/M/ascii_trajectories_wide.csv"), skip = 1)

# find ZMB's column
grep("Zambia", colnames(e0Ftraj)) # results in XX

ZMBe0Ftraj <- cbind(rep(562,nrow(e0Ftraj)), e0Ftraj[c(2,3,30)]) 
ZMBe0Mtraj <- cbind(rep(562,nrow(e0Mtraj)), e0Mtraj[c(2,3,30)]) 

colnames(ZMBe0Ftraj) <- c("LocID","Year","Trajectory","e0")
colnames(ZMBe0Mtraj) <- c("LocID","Year","Trajectory","e0")

write.csv(ZMBe0Ftraj,file= paste0(output, "regdata/ZMBe0Ftraj.csv"), row.names = FALSE)
write.csv(ZMBe0Mtraj,file= paste0(output, "regdata/ZMBe0Mtraj.csv"), row.names = FALSE)

# If migration shares are specified, it is used to distribute the national migration taken out of
# the wpp2017 package. Note that such approach does not deal with the between-region migration.

# Generate subnational trajectories for all regions of one country.

regpop.pred <- pop.predict.subnat(present.year = 2015, 
                                  wpp.year = 2017, 
                                  output.dir = reg.pop.dir,
                                  locations  = locations,
                                  inputs     = list(popM        = file.path("regdata", "ZMBpopM.txt"),
                                                    popF        = file.path("regdata", "ZMBpopF.txt"),
                                                    tfr.sim.dir = ZMBtfr.dir),
                                  verbose = TRUE,
                                  nr.traj = 200,
                                  replace.output=TRUE)

regpop.pred <- get.pop.prediction(reg.pop.dir)

get.countries.table(regpop.pred)

# Explore results.
par(mfrow = c(1,2))
pop.trajectories.plot(regpop.pred, "Boucle du Mouhoun", sum.over.ages = TRUE)

pop.pyramid(regpop.pred, "Sud-Ouest", year = 2015)

setwd(code)

