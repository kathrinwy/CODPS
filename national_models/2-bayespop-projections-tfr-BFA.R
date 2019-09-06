##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: script to project tfr
##
## Date created: 6 September 2019
## Last updated: 6 September 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

##---------------------------
## Estimation of the TFR model
##---------------------------

# Set a location on disk for TFR results
setwd(output)
tfr.dir <- "TFRsimulation"
country <- "Burkina Faso"

# Run Phase II MCMCs ------------------------------------------------------

# This is a toy simulation and not enough to make the series converge.
#m2 <- run.tfr.mcmc(output.dir = tfr.dir, 
 #                  iter = 100,            # iter=62000
  #                 nr.chains = 2,
   #                wpp.year = 2017,       # wpp 2017
    #               start.year = 1950, 
     #              present.year = 2015,   # year of census
      #             replace.output = TRUE)

# m2 <- get.tfr.mcmc(tfr.dir)
# We will use the fully converged version: https://bayespop.csss.washington.edu/download/#subnatTFR

# Run Phase III MCMCs -----------------------------------------------------
# m3 <- run.tfr3.mcmc(sim.dir = tfr.dir, 
  #                  iter = 100,                     
   #                 nr.chains = 2, 
    #                thin = 2,
     #               replace.output = TRUE)

# m3 <- get.tfr3.mcmc(tfr.dir)

# No simulations are run as Niger has not yet entered Phase III
# We will use the fully converged version: https://bayespop.csss.washington.edu/download/#subnatTFR

# Projection tfr ----------------------------------------------------------
# tfr.pred <- tfr.predict(sim.dir = tfr.dir, 
  #                      end.year = 2030,
   #                     burnin = 20,              # burnin=2000
    #                    burnin3 = 10,             # burnin3=1000
     #                   nr.traj = 50,             # nr.traj=5000
      #                  use.correlation = TRUE,
       #                 replace.output = TRUE)    

# We will use the fully converged version: https://bayespop.csss.washington.edu/download/#subnatTFR
tfr.pred <- get.tfr.prediction(tfr.dir)

# Inspect results
par(mfrow=c(1,1))
tfr.trajectories.plot(tfr.pred, country = country, nr.traj = 500)

# Export TFR trajectories 
convert.tfr.trajectories(tfr.dir, n = 50, output.dir = "mytfrtrajs") 

setwd(code)