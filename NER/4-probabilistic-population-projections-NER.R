##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: Population Projection (national level)
##
## Date created: 14 August 2019
## Last updated: 16 August 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

setwd(NER.output)

tfr.dir <- "TFRsimulation"
e0.dir <- "e0simulation"
pop.dir <- "POPsimulation"

# Projections using our probabilistic TFR ---------------------------------

pop.pred <- pop.predict( end.year = 2030, 
                         start.year = 1950, 
                         present.year = 2015,   # 2015 projected from 2012 
                         wpp.year = 2017,       # ERROR MESSAGE: vwBaseYear2019 not found: migration error, so use wpp2017
                         output.dir = pop.dir, 
                         nr.traj = 50,
                         inputs = list(tfr.sim.dir = tfr.dir,
                                      e0F.sim.dir = e0.dir,
                                      e0M.sim.dir = "joint_"),
                         keep.vital.events = FALSE,
                         replace.output=TRUE)

pop.pred <- get.pop.prediction(pop.dir)

# Plot projection trajectories.
pop.trajectories.plot(pop.pred, country = country,
                      sum.over.ages = TRUE, nr.traj = 30) 
