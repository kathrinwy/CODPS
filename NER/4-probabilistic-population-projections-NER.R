##
##
##=========================================
## Population Projection (national level)
##=========================================
##The main function to generate population projection with bayesPop 
##is pop.predict.
##==
##1. Load the bayesPop library Also, 
getwd()
setwd("/Users/romesh/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop")
##set the I/O directories. 
tfr.dir <- "TFRsimulation"
e0.dir <- "e0simulation"
pop.dir <- "POPsimulation"
## check with sessionInfo() that version bayesPop_7.0-0 is:
sessionInfo()
##==
##2. Generate projections using our probabilistic TFR and from previous labs:
pop.pred <- pop.predict(
  end.year = 2030, 
  start.year = 1950, 
  present.year = 2005,
  wpp.year = 2015, 
  output.dir = pop.dir, 
  nr.traj = 50,
  inputs = list(tfr.sim.dir = tfr.dir,
                e0F.sim.dir = e0.dir,
                e0M.sim.dir = "joint_"),
  keep.vital.events = FALSE,
  replace.output=TRUE)
##To retrieve the prediction object from disk e.g. at a later time point, use
pop.pred <- get.pop.prediction(pop.dir)
##==
##3. All inputs needed for projections are defined in the argument inputs 
##   which is a list with named elements. The following table explains 
##   the datasets including their sources:
##4. Investigate the content of the simulation directory. 
##   Folder predictions was added:
list.files(pop.dir)
head(list.files(file.path(pop.dir, "predictions")), 20)
tail(list.files(file.path(pop.dir, "predictions")), 20)
##  There are two files per country. If projections were generated with
##  keep.vital.events = FALSE , there is only one file per country.
##==
##5. View projection results:
##   Choose a country and summarize projection results.
country <- "Cameroon"
summary(pop.pred, country = country)
pop.trajectories.table(pop.pred, country = country,
                       pi = c(80, 90), half.child.variant = TRUE)
pop.byage.table(pop.pred, country = country,
                pi = c(80, 90), year = 2030)
pop.byage.table(pop.pred, country = country,
                pi = c(80, 90), year = 2020)
##Plot projection trajectories.
pop.trajectories.plot(pop.pred, country = country,
                      sum.over.ages = TRUE, nr.traj = 30) 
points(c(2005,2010,2015,2020),c(17463,19648,22179,26133),type="b",col="blue",lwd=2.5)
pop.trajectories.plot(pop.pred, country = country, sex = "female",
                      age = 4:10, sum.over.ages = TRUE, nr.traj = 0)
points(c(2005,2010,2015,2020),c(4248,4841,5506,6521),type="b",col="blue",lwd=2.5)

##Ages 0-4...,15-19
par(mfrow=c(1,4))
pop.trajectories.plot(pop.pred, country = country, sex = "female",
                      age = 1, nr.traj = 10)
points(c(2005,2010,2015,2020),c(1459,1630,1819,2008),type="b",col="blue",lwd=2.5)

pop.trajectories.plot(pop.pred, country = country, sex = "female",
                      age = 2, nr.traj = 10)
points(c(2005,2010,2015,2020),c(1234,1356,1514,1754),type="b",col="blue",lwd=2.5)

pop.trajectories.plot(pop.pred, country = country, sex = "female",
                      age = 3, nr.traj = 10)
points(c(2005,2010,2015,2020),c(1053,1166,1301,1551),type="b",col="blue",lwd=2.5)

pop.trajectories.plot(pop.pred, country = country, sex = "female",
                      age = 4, nr.traj = 10)
points(c(2005,2010,2015,2020),c(993,1143,1300,1359),type="b",col="blue",lwd=2.5)

pop.byage.plot(pop.pred, country = country, year = 2030)
pop.byage.plot(pop.pred, country = country,age=seq(1,17,1), year = 2020)
points(1:17, c(4028,3512,3099,2740,2397,2055,1841,1541,1266,993,761,621,478,357,198,141,51),type="b",col="blue",lwd=2.5)

##  Age indices correspond to the following age groups:
##    0-4, 1
##    5-9, 2
##    ...
##    125-129, 26
##    130+, 127
##  Plot cohort trajectories.
pop.cohorts.plot(pop.pred, country = country,
                 cohorts = c(1980, 2020))
##  Probabilistic population pyramids.
##  Classic pyramids (can display up to two time periods and one set 
##  of probability intervals):
par(mfrow=c(1,1))
pop.pyramid(pop.pred, country, year = c(2020, 2005))
##  Trajectory pyramid (can include any number of years and any 
##  number of probability intervals):
pop.trajectories.pyramid(pop.pred, country,
                         year = c(2019, 2005, 1950), 
                         nr.traj = 10,
                         proportion = FALSE, 
                         age = 1:20,
                         pi = 80)
##  Maps:
pop.map(pop.pred, year = 2030)
pop.map.gvis(pop.pred, year = 2030)