
# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: subnational TFR projections

# Date created: 11 September 2019
# Last updated: 11 September 2019

# Author: Kathrin Weny
# Maintainers: Kathrin Weny, Romesh Silva

# Inputs ------------------------------------------------------------------

# bayesTFR projections of the national TFR (result of tfr.predict )

# Load data for subnational units

setwd(output)

my.regtfr.file.BGD <- "regdata/tfr.txt"
read.delim(my.regtfr.file.BGD , check.names = F)

# Projections -------------------------------------------------------------

# Set a location on disk where results will be stored

dir.create("regTFRsimulation")
nat.tfr.dir <- "TFRsimulation"
reg.tfr.dir <- "regTFRsimulation"

# Generate projections for BGD (50)

# data("iso3166", package = "bayesTFR") # Find WPP and ISO code

regtfr.preds <- tfr.predict.subnat(50,
                                   my.tfr.file = my.regtfr.file.BGD,
                                   sim.dir = nat.tfr.dir,
                                   output.dir = reg.tfr.dir)

regtfr.preds <- get.regtfr.prediction(reg.tfr.dir)

#  Store the directory for Zimbabwe
BGDtfr.dir <- file.path(reg.tfr.dir, "subnat", "c50")

# Explore projections
BGDtfr <- get.tfr.prediction(BGDtfr.dir)

par(mfrow=c(1,2))

region <- "Dhaka"
tfr.trajectories.plot(BGDtfr, region)

region <- "Rajshahi/Rangpur"
tfr.trajectories.plot(BGDtfr, region)

setwd(code)