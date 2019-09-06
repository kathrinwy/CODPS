##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: subnational TFR projections
##
## Date created: 6 September 2019
## Last updated: 6 September 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

# Setup -------------------------------------------------------------------

setwd(output)
#dir.create("regdata")

# Retrieve e0 trajectories ------------------------------------------------

BFAe0Ftraj <- read.csv(file = "./mye0trajs/F/ascii_trajectories.csv", header=TRUE, sep=",") %>%
    select(-Period)
write.csv(BFAe0Ftraj, paste0("./regdata/", "BFAe0Ftraj.csv"), row.names = F)

BFAe0Mtraj <- read.csv(file = "./mye0trajs/M/ascii_trajectories.csv", header=TRUE, sep=",") %>%
    select(-Period)
write.csv(BFAe0Mtraj, paste0("./regdata/", "BFAe0Mtraj.csv"), row.names = F)

# Inputs ------------------------------------------------------------------

# bayesTFR projections of the national TFR (result of tfr.predict )

nat.tfr.dir <- "TFRsimulation"

# Load data for subnational units

# PCode     region
# 01	      Boucle de Mouhoun
# 02	      Cascades
# 03	      Centre including Ouagadougou
# 04	      Centre-Est
# 05	      Centre-Nord
# 06	      Centre-Ouest
# 07	      Centre-Sud
# 08	      Est
# 09	      Hauts Basins
# 10	      Nord
# 11	      Plateau Central
# 12	      Sahel
# 13	      Sud-Ouest

my.regtfr.file.BFA <- "regdata/tfr.BFA.txt"
read.delim(my.regtfr.file.BFA , check.names = F)

# Projections -------------------------------------------------------------

# Set a location on disk where results will be stored

dir.create("regTFRsimulation")

reg.tfr.dir <- "regTFRsimulation"

# Generate projections for BFA (854)

regtfr.preds <- tfr.predict.subnat(854, 
                                   my.tfr.file = my.regtfr.file.BFA,
                                   sim.dir = nat.tfr.dir,
                                   output.dir = reg.tfr.dir)

regtfr.preds <- get.regtfr.prediction(reg.tfr.dir)

BFAtfr <- regtfr.preds[["854"]]
trajs <- get.tfr.trajectories(BFAtfr, region)
get.countries.table(BFAtfr)

#  Store the directory for Burkina Faso
BFAtfr.dir <- file.path(reg.tfr.dir, "subnat", "c854")

# Explore projections
BFAtfr <- get.tfr.prediction(BFAtfr.dir)

par(mfrow=c(1,2))

region <- "Cascades"
tfr.trajectories.plot(BFAtfr, region)

region <- "Sahel"
tfr.trajectories.plot(BFAtfr, region)


setwd(code)