##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: subnational population projections
##
## Date created: 10 September 2019
## Last updated: 10 September 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

# Probabilistic population pyramids ---------------------------------------

par(mfrow=c(1,1))
pop.pyramid(pop.pred, country, year = c(2020, 2005))

pop.trajectories.pyramid(pop.pred, country,
                         year = c(2019, 2005, 1950), 
                         nr.traj = 10,
                         proportion = FALSE, 
                         age = 1:20,
                         pi = 80)

# Poptrajectories plot ----------------------------------------------------

sim.dir <- file.path(find.package("bayesPop"), "ex-data", "Pop")
pred <- get.pop.prediction(sim.dir)
pop.trajectories.plot(pred, country="Ecuador", pi=c(80, 95))
pop.trajectories.table(pred, country="Ecuador", pi=c(80, 95))
# female population of Ecuador in child bearing ages (by time)
pop.trajectories.plot(pred, expression="PEC_F[4:10]")
# Population by age in Netherands for two different years
pop.byage.plot(pred, country="Netherlands", year=2050)
pop.byage.plot(pred, expression="PNL{}", year=2000)


