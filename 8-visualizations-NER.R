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