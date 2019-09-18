
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

# PCode     region
# 01	Barisal
# 02	Chittagong and Sylhet
# 03	Dhaka
# 04	Khulna
# 05	Rajshahi and Rangpur

setwd(output)

my.regtfr.file.BDA <- "regdata/tfr.BDA.txt"
read.delim(my.regtfr.file.BDA , check.names = F)

# Projections -------------------------------------------------------------

# Set a location on disk where results will be stored

dir.create("regTFRsimulation")
nat.tfr.dir <- "TFRsimulation"
reg.tfr.dir <- "regTFRsimulation"

# Generate projections for BDA (50)

# data("iso3166", package = "bayesTFR") # Find WPP and ISO code

regtfr.preds <- tfr.predict.subnat(50,
                                   my.tfr.file = my.regtfr.file.BDA,
                                   sim.dir = nat.tfr.dir,
                                   output.dir = reg.tfr.dir)

regtfr.preds <- get.regtfr.prediction(reg.tfr.dir)

#  Store the directory for Burkina Faso
BDAtfr.dir <- file.path(reg.tfr.dir, "subnat", "c50")

# Explore projections
BDAtfr <- get.tfr.prediction(BDAtfr.dir)

par(mfrow=c(1,2))

region <- "Dhaka"
tfr.trajectories.plot(BDAtfr, region)

region <- "Khulna"
tfr.trajectories.plot(BDAtfr, region)

setwd(code)