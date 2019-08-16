##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: subnational TFR projections
##
## Date created: 15 August 2019
## Last updated: 15 August 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

# Setup -------------------------------------------------------------------

setwd(NER.output)
dir.create("regdata")

# Retrieve e0 trajectories ------------------------------------------------

NERe0Ftraj <- read.csv(file = "./mye0trajs/F/ascii_trajectories.csv", header=TRUE, sep=",") %>%
    select(-Period)
write.csv(NERe0Ftraj, paste0("./regdata/", "NERe0Ftraj.csv"), row.names = F)

NERe0Mtraj <- read.csv(file = "./mye0trajs/M/ascii_trajectories.csv", header=TRUE, sep=",") %>%
    select(-Period)
write.csv(NERe0Mtraj, paste0("./regdata/", "NERe0Mtraj.csv"), row.names = F)

# Inputs ------------------------------------------------------------------

# bayesTFR projections of the national TFR (result of tfr.predict )

nat.tfr.dir <- "TFRsimulation"

# Load data for subnational units

# PCode     region
# NER001    Agadez 
# NER002    Diffa
# NER003    Dosso
# NER004    Maradi
# NER005    Tahoua 
# NER006    Tillaberi
# NER007    Zinder
# NER008    Niamey 

my.regtfr.file.NER <- "regdata/tfr.NER.txt"
read.delim(my.regtfr.file.NER, check.names = F)

# Projections -------------------------------------------------------------

# Set a location on disk where results will be stored

dir.create("regTFRsimulation")

reg.tfr.dir <- "regTFRsimulation"

# Generate projections for Niger (562)

regtfr.preds <- tfr.predict.subnat(562, 
                                   my.tfr.file = my.regtfr.file.NER,
                                   sim.dir = nat.tfr.dir,
                                   output.dir = reg.tfr.dir)

regtfr.preds <- get.regtfr.prediction(reg.tfr.dir)

#  Store the directory for Niger
NERtfr.dir <- file.path(reg.tfr.dir, "subnat", "c562")

# Explore projections
NERtfr <- get.tfr.prediction(NERtfr.dir)

par(mfrow=c(1,2))

region <- "Diffa"
tfr.trajectories.plot(NERtfr, region)

region <- "Agadez"
tfr.trajectories.plot(NERtfr, region)
