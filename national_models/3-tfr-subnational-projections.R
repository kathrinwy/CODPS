
# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: subnational TFR projections

# Date created: 4 December 2019
# Last updated: 4 December 2019

# Author: Kathrin Weny
# Maintainers: Kathrin Weny, Romesh Silva

# Inputs ------------------------------------------------------------------

# bayesTFR projections of the national TFR (result of tfr.predict )

# Load data for subnational units
setwd(output)

my.regtfr.file <- "regdata/tfr.txt"
read.delim(my.regtfr.file , check.names = F) %>%
  filter(country == country)

# Projections -------------------------------------------------------------

# Set a location on disk where results will be stored

dir.create("regTFRsimulation")
nat.tfr.dir <- "TFRsimulation"
reg.tfr.dir <- "regTFRsimulation"

# data("iso3166", package = "bayesTFR") 

regtfr.preds <- tfr.predict.subnat(country.code,
                                   my.tfr.file = my.regtfr.file,
                                   sim.dir = nat.tfr.dir,
                                   output.dir = reg.tfr.dir)

regtfr.preds <- get.regtfr.prediction(reg.tfr.dir)

#  Store the directory
tfr.dir <- file.path(reg.tfr.dir, "subnat", paste0("c", country.code))

