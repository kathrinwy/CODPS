##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: Population Projection (national level)
##
## Date created: 14 August 2019
## Last updated: 15 August 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

setwd(NER.output)

tfr.dir <- "TFRsimulation"
e0.dir <- "e0simulation"
pop.dir <- "POPsimulation"

# Projections using our probabilistic TFR and from previous labs:
pop.pred <- pop.predict(
  end.year = 2030, 
  start.year = 1950, 
  #present.year = 2010,  # 2012 not availbe
  wpp.year = 2017, 
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
