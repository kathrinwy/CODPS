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

##  bayesTFR projections of the national TFR (result of tfr.predict ). We can use either our toy simulation 
##  in the “TFRsimulation” directory or the converged simulation.

nat.tfr.dir <- "TFRsimulation"

##  data for subnational units (https://bayespop.csss.washington.edu/download/#subnatTFR).

my.regtfr.file.NER <- "regdata/tfr.NER.txt"
my.regtfr.file.CAN <- "regdata/tfr.txt"

read.delim(my.regtfr.file.NER, check.names = F)
read.delim(my.regtfr.file.CAN, check.names = F)

# Required columns are country_code , reg_code , name 
# and at least one time column that corresponds to the 
# last observed time period

# Projections -------------------------------------------------------------

# Set a location on disk where results will be stored

dir.create("regTFRsimulation")

reg.tfr.dir <- "regTFRsimulation"

# Generate projections for Niger (562)

regtfr.preds <- tfr.predict.subnat(562, 
                                   my.tfr.file = my.regtfr.file.NER,
                                   sim.dir = nat.tfr.dir,
                                   output.dir = reg.tfr.dir)

regtfr.preds <- tfr.predict.subnat(124, 
                                   my.tfr.file = my.regtfr.file.CAN,
                                   sim.dir = nat.tfr.dir,
                                   output.dir = reg.tfr.dir)

regtfr.preds <- get.regtfr.prediction(reg.tfr.dir)


##Explore the content of the directory “regTFRsimulation”.
##  It contains a subdirectory “subnat” with subfolders 
##  for each country. Each such subfolder contains a 
##  directory “predictions” of the same structure as the
##  national predictions directory:
list.files(file.path(reg.tfr.dir, 
                     "subnat"))
list.files(file.path(reg.tfr.dir,
                     "subnat",
                     "c120", 
                     "predictions"))
##  We store the directory for Cameroon into an object:
NERtfr.dir <- file.path(reg.tfr.dir,
                        "subnat",
                        "c562")

# Explore projections

NERtfr <- regtfr.preds[["562"]]

# identical to
get.countries.table(NERtfr)
NERtfr <- get.tfr.prediction(NERtfr.dir)

region <- "Diffa"
tfr.trajectories.plot(NERtfr, 
                      region)

# Retrieve trajectories as a matrix ---------------------------------------
trajs <- get.tfr.trajectories(NERtfr, region)

