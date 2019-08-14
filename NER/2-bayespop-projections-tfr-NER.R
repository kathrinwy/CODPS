##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: script to project tfr
##
## Date created: 14 August 2019
## Last updated: 14 August 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

##---------------------------
## Estimation of the TFR model
##---------------------------

# Set a location on disk for TFR results

setwd(NER.output)
tfr.dir <- "TFRsimulation"
country <- "Niger"

# Run Phase II MCMCs ------------------------------------------------------

m2 <- run.tfr.mcmc(output.dir = tfr.dir, 
                   iter = 100,            # iter=62000
                   nr.chains = 2,
                   wpp.year = 2017,       # wpp 2017
                   start.year = 1950, 
                   present.year = 2012,   # year of census
                   replace.output = TRUE)

m2 <- get.tfr.mcmc(tfr.dir)


# Run Phase III MCMCs -----------------------------------------------------

m3 <- run.tfr3.mcmc(sim.dir = tfr.dir, 
                    iter = 100,                     
                    nr.chains = 2, 
                    thin = 2,
                    replace.output = TRUE)

m3 <- get.tfr3.mcmc(tfr.dir)

# No simulations are run as Niger has not yet entered Phase III

# Projection tfr ----------------------------------------------------------

tfr.pred <- tfr.predict(sim.dir = tfr.dir, 
                        end.year = 2030,
                        burnin = 20,              # burnin=2000
                        burnin3 = 10,             # burnin3=1000
                        nr.traj = 20,             # nr.traj=5000
                        use.correlation = TRUE,
                        replace.output = TRUE)    

tfr.pred <- get.tfr.prediction(tfr.dir)

# Inspect results

par(mfrow=c(1,1))
tfr.trajectories.plot(tfr.pred, 
                      country = country, 
                      nr.traj = 40)

# Export TFR trajectories 

# convert.tfr.trajectories(tfr.dir, n = 40, output.dir = "mytfrtrajs") # error

# Obtain trajectories as a matrix -----------------------------------------

trajs <- get.tfr.trajectories(tfr.pred, country)
