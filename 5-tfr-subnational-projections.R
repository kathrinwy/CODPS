##======================================================
##References
##======================================================
##Ševčíková, H., Raftery, A.E. and Gerland, P. (2018). 
##Probabilistic projection of subnational total fertility 
##rates. Demographic Research, Vol. 38(60): 1843-1884.
##(https://www.demographic-research.org/volumes/vol38/60/default.htm)
##
##======================================================
##Prerequisites
##======================================================
## Directory “TFRsimulation” with (TFR projections on the national level):
## Phase II MCMC ( run.tfr.mcmc (../tfr/TFRlabs.html#tfr-phase2),
##   continue.tfr.mcmc (../tfr/TFRlabs.html#tfr-phase2-cont))
## Phase III MCMC ( run.tfr3.mcmc (../tfr/TFRlabs.html#tfr-phase3))
## TFR predictions ( tfr.predict (../tfr/TFRlabs.html#tfr-pred))
##
##======================================================
## Goals
##======================================================
##  Projecting subnational total fertility rate using bayesTFR,
##  projecting subnational population using bayesPop,
##  exploring results.
##        
##======================================================
## Setup
##======================================================
##1. Use setwd() to navigate into your working directory 
##   and load bayesPop (which automatically loads bayesTFR):
getwd()
setwd("~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop/")
library(bayesPop)

##2. We will use a few external datasets. If you have 
##   not done already, run the following code which 
##   creates a directory regdata and downloads the 
##   datasets into it.
path <- "http://bayespop.csss.washington.edu/data/ALAP2018/regdata/"
files <- c("tfr.txt", 
           "MALe0Ftraj.csv", 
           "MALe0Mtraj.csv")
dir.create("regdata")
Map(function(u, d) download.file(u, d), 
    paste0(path, files), 
    file.path("regdata", files))

##======================================================
##Subnational TFR Projections
##======================================================
## The bayesTFR package implements a method for generating 
## probabilistic TFR for subnational units, as described
## in Ševčíková et al. (2018).
##
##======================================================
##Inputs
##======================================================
##You need the following inputs:
##  bayesTFR projections of the national TFR (result of 
##  tfr.predict ). We can use either our toy simulation 
##  in the “TFRsimulation” directory or the converged 
##  simulation.
nat.tfr.dir <- "TFRsimulation"
##  data for subnational units. We will use a dataset for Canada. Data for additional countries
##  can be downloaded from here (https://bayespop.csss.washington.edu/download/#subnatTFR).
#my.regtfr.file <- "regdata/tfr.txt"
#read.delim(my.regtfr.file, 
#           check.names = FALSE)  
my.regtfr.file <- "regdata/tfr_cameroon.txt"
read.delim(my.regtfr.file,
           check.names = FALSE)

can.regtfr.file <- "regdata/tfr.txt"
read.delim(can.regtfr.file,
           check.names = FALSE)
##  Required columns are country_code , reg_code , name 
##  and at least one time column that corresponds to the 
##  last observed time period.
##
##======================================================
##Projections
##======================================================
##1. Set a location on disk where results will be stored. 
##   If not set, results are stored into the directory 
##   of the national simulation.
reg.tfr.dir <- "regTFRsimulation"
##2. Here we will generate projections for one country 
##   (Cameroon), but the function can be used to
##   project multiple countries at once.    
regtfr.preds <- tfr.predict.subnat(120, 
                                   my.tfr.file = my.regtfr.file,
                                   sim.dir = nat.tfr.dir,
                                   output.dir = reg.tfr.dir)

regtfr.preds <- tfr.predict.subnat(124, 
                                   my.tfr.file = can.regtfr.file,
                                   sim.dir = nat.tfr.dir,
                                   output.dir = reg.tfr.dir)

##To retrieve the regtfr.preds object at later time, use
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
CAMtfr.dir <- file.path(reg.tfr.dir,
                        "subnat",
                        "c120")

CANtfr.dir <-  file.path(reg.tfr.dir,
                         "subnat",
                         "c124")
##4. Explore projections.
##   The regtfr.preds object is a list with one element
##   per country. We’ll explore results for Cameroon
names(regtfr.preds)
CAMtfr <- regtfr.preds[["120"]]
CANtfr <- regtfr.preds[["124"]]

class(CAMtfr)
names(CAMtfr)
# identical to
get.countries.table(CAMtfr)
CANtfr <- get.tfr.prediction(CANtfr.dir)

##    Results can be viewed in the same way as results 
##    from the national projections (../tfr/TFRlabs.html#tfr-pred-view).
region <- "Adamaoua"
tfr.trajectories.plot(CAMtfr, 
                      region)
tfr.trajectories.table(CAMtfr, 
                       region)
summary(CAMtfr, 
        region)

region <- "Nord"
tfr.trajectories.plot(CAMtfr, 
                      region)
tfr.trajectories.table(CAMtfr, 
                       region)
summary(CAMtfr, 
        region)

region <- "Extreme Nord"
tfr.trajectories.plot(CAMtfr, 
                      region)
tfr.trajectories.table(CAMtfr, 
                       region)
summary(CAMtfr, 
        region)

region <- "Centre"
tfr.trajectories.plot(CAMtfr, 
                      region)
tfr.trajectories.table(CAMtfr, 
                       region)
summary(CAMtfr, 
        region)

region <- "Sud"
tfr.trajectories.plot(CAMtfr, 
                      region)
tfr.trajectories.table(CAMtfr, 
                       region)
summary(CAMtfr, 
        region)

region <- "Est"
tfr.trajectories.plot(CAMtfr, 
                      region)
tfr.trajectories.table(CAMtfr, 
                       region)
summary(CAMtfr, 
        region)

region <- "Ouest"
tfr.trajectories.plot(CAMtfr, 
                      region)
tfr.trajectories.table(CAMtfr, 
                       region)
summary(CAMtfr, 
        region)

region <- "Littoral"
tfr.trajectories.plot(CAMtfr, 
                      region)
tfr.trajectories.table(CAMtfr, 
                       region)
summary(CAMtfr, 
        region)

region <- "Nord Ouest"
tfr.trajectories.plot(CAMtfr, 
                      region)
tfr.trajectories.table(CAMtfr, 
                       region)
summary(CAMtfr, 
        region)

region <- "Sud Ouest"
tfr.trajectories.plot(CAMtfr, 
                      region)
tfr.trajectories.table(CAMtfr, 
                       region)
summary(CAMtfr, 
        region)

##     Compare national to regional projections
region <- "Adamaoua"
nat.tfr.pred <- get.tfr.prediction(nat.tfr.dir)
tfr.trajectories.plot(CAMtfr, 
                      region, 
                      pi = 80, 
                      half.child.variant = FALSE)
tfr.trajectories.plot(nat.tfr.pred,
                      "Cameroon",
                      half.child.variant = FALSE, 
                      pi = 80,
                      add = TRUE, 
                      col = rep("darkgreen", 5),
                      nr.traj = 0, 
                      show.legend = FALSE)

region <- "Nord"
nat.tfr.pred <- get.tfr.prediction(nat.tfr.dir)
tfr.trajectories.plot(CAMtfr, 
                      region, 
                      pi = 80, 
                      half.child.variant = FALSE)
tfr.trajectories.plot(nat.tfr.pred,
                      "Cameroon",
                      half.child.variant = FALSE, 
                      pi = 80,
                      add = TRUE, 
                      col = rep("darkgreen", 5),
                      nr.traj = 0, 
                      show.legend = FALSE)

region <- "Extreme Nord"
nat.tfr.pred <- get.tfr.prediction(nat.tfr.dir)
tfr.trajectories.plot(CAMtfr, 
                      region, 
                      pi = 80, 
                      half.child.variant = FALSE)
tfr.trajectories.plot(nat.tfr.pred,
                      "Cameroon",
                      half.child.variant = FALSE, 
                      pi = 80,
                      add = TRUE, 
                      col = rep("darkgreen", 5),
                      nr.traj = 0, 
                      show.legend = FALSE)

region <- "Nord Ouest"
nat.tfr.pred <- get.tfr.prediction(nat.tfr.dir)
tfr.trajectories.plot(CAMtfr, 
                      region, 
                      pi = 80, 
                      half.child.variant = FALSE)
tfr.trajectories.plot(nat.tfr.pred,
                      "Cameroon",
                      half.child.variant = FALSE, 
                      pi = 80,
                      add = TRUE, 
                      col = rep("darkgreen", 5),
                      nr.traj = 0, 
                      show.legend = FALSE)


##      Retrieve trajectories as a matrix
trajs <- get.tfr.trajectories(CAMtfr, 
                              region)

dim(trajs)
summary(t(trajs))