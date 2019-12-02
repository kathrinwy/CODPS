
# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: subnational TFR projections

# Date created: 29 November 2019
# Last updated: 29 November 2019

# Author: Kathrin Weny
# Maintainers: Kathrin Weny, Romesh Silva

# Inputs ------------------------------------------------------------------

# bayesTFR projections of the national TFR (result of tfr.predict )

# Load data for subnational units

# PCode     region
# ZMB10	Central
# ZMB20	Copperbelt
# ZMB40	Luapula
# ZMB50	Lusaka
# ZMB70	North-Western
# ZMB80	Southern
# ZMB90	Western
# ZMB30	Eastern
# ZMB11	Muchinga
# ZMB60	Northern

setwd(output)

my.regtfr.file.ZMB <- "regdata/tfr_cameroon.txt"
read.delim(my.regtfr.file.ZMB , check.names = F)

# Projections -------------------------------------------------------------

# Set a location on disk where results will be stored

dir.create("regTFRsimulation")
nat.tfr.dir <- "TFRsimulation"
reg.tfr.dir <- "regTFRsimulation"

# Generate projections for ZMB (894)

# data("iso3166", package = "bayesTFR") # Find WPP and ISO code

regtfr.preds <- tfr.predict.subnat(894,
                                   my.tfr.file = my.regtfr.file.ZMB,
                                   sim.dir = nat.tfr.dir,
                                   output.dir = reg.tfr.dir)

regtfr.preds <- get.regtfr.prediction(reg.tfr.dir)

#  Store the directory for Zambia
ZMBtfr.dir <- file.path(reg.tfr.dir, "subnat", "c894")

# Explore projections
ZMBtfr <- get.tfr.prediction(ZMBtfr.dir)

par(mfrow=c(5,2))

region <- "Muchinga"
tfr.trajectories.plot(ZMBtfr, region)

region <- "Western"
tfr.trajectories.plot(ZMBtfr, region)

region <- "Central"
tfr.trajectories.plot(ZMBtfr, region)

region <- "Copperbelt"
tfr.trajectories.plot(ZMBtfr, region)

region <- "Luapula"
tfr.trajectories.plot(ZMBtfr, region)

region <- "Lusaka"
tfr.trajectories.plot(ZMBtfr, region)

region <- "North-Western"
tfr.trajectories.plot(ZMBtfr, region)

region <- "Southern"
tfr.trajectories.plot(ZMBtfr, region)

region <- "Eastern"
tfr.trajectories.plot(ZMBtfr, region)

region <- "Northern"
tfr.trajectories.plot(ZMBtfr, region)

setwd(code)