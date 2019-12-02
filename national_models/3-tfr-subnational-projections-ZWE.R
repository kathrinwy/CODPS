
# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: subnational TFR projections

# Date created: 2 December 2019
# Last updated: 2 December 2019

# Author: Kathrin Weny
# Maintainers: Kathrin Weny, Romesh Silva

# Inputs ------------------------------------------------------------------

# bayesTFR projections of the national TFR (result of tfr.predict )

# Load data for subnational units

# PCode     region
# 01	Manicaland
# 02	Mashonaland Central
# 03	Mashonaland East
# 04	Mashonaland West
# 05	Matabeleland North
# 06	Matabeleland South
# 07	Midlands
# 08	Masvingo
# 09	Harare
# 10	Bulawayo

setwd(output)

my.regtfr.file.ZWE <- "regdata/tfr.txt"
read.delim(my.regtfr.file.ZWE , check.names = F)

# Projections -------------------------------------------------------------

# Set a location on disk where results will be stored

dir.create("regTFRsimulation")
nat.tfr.dir <- "TFRsimulation"
reg.tfr.dir <- "regTFRsimulation"

# Generate projections for ZWE (716)

# data("iso3166", package = "bayesTFR") # Find WPP and ISO code

regtfr.preds <- tfr.predict.subnat(716,
                                   my.tfr.file = my.regtfr.file.ZWE,
                                   sim.dir = nat.tfr.dir,
                                   output.dir = reg.tfr.dir)

regtfr.preds <- get.regtfr.prediction(reg.tfr.dir)

#  Store the directory for Zimbabwe
ZWEtfr.dir <- file.path(reg.tfr.dir, "subnat", "c716")

# Explore projections
ZWEtfr <- get.tfr.prediction(ZWEtfr.dir)

par(mfrow=c(1,2))

region <- "Harare"
tfr.trajectories.plot(ZWEtfr, region)

region <- "Masvingo"
tfr.trajectories.plot(ZWEtfr, region)

setwd(code)