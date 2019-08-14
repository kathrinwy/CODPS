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

setwd(NER.output)

# Set a location on disk for TFR results
tfr.dir <- "TFRsimulation"

# Run Phase II MCMCs. Here we chose a toy simulation

# run.tfr.mcmc
m2 <- run.tfr.mcmc(output.dir = tfr.dir, 
                   iter = 100,#0,         # iter=62000
                   nr.chains = 2,
                   wpp.year = 2017, 
                   start.year = 1950, 
                   present.year = 2012,   # year of census
                   replace.output = TRUE)

# m2 is an object containing all the Phase-II MCMC meta 

m2 <- get.tfr.mcmc(tfr.dir)

# View the content of the simulation directory, 

list.files(tfr.dir)
head(list.files(file.path(tfr.dir,  "mc1")), 15)

# View parameter traces for world parameters 

tfr.partraces.plot(m2)

# View parameter traces for country-specific 
# parameters for a chosen country.

country <- "Niger"
tfr.partraces.cs.plot(m2, country = country)

# Plot its double logistic curves

par(mfrow=c(1,1))
DLcurve.plot(m2, 
             country = country, 
             burnin = 30, 
             nr.curves = 30)

# 5. Continue Phase II MCMCs in each chain by 40 iterations

m2 <- continue.tfr.mcmc(output.dir = tfr.dir, iter = 40)                  
summary(m2)

# 6. Inspect the content of the m2 object:

typeof(m2)
names(m2)
names(m2$meta)
length(m2$mcmc.list)
names(m2$mcmc.list[[1]]) 

# 7. Run Phase III MCMCs, again a toy simulation here

m3 <- run.tfr3.mcmc(sim.dir = tfr.dir, 
                    iter = 100,                     
                    nr.chains = 2, 
                    thin = 2,
                    replace.output = TRUE)

# Save m3 object for later usage

m3 <- get.tfr3.mcmc(tfr.dir)  

# 8. Check the content of the simulation directory

list.files(tfr.dir)
list.files(file.path(tfr.dir, "phaseIII"))
list.files(file.path(tfr.dir, "phaseIII", "mc1"))

# 9. View Phase III estimation results/parameter traces.

tfr3.partraces.plot(m3)
tfr3.partraces.cs.plot(m3, country = country)

## Summarize estimation results.

summary(m3, country = country)

# Projection tfr ----------------------------------------------------------

# 1. Generate TFR projections for all countries

tfr.pred <- tfr.predict(sim.dir = tfr.dir, 
                        end.year = 2030,
                        burnin = 20,#0,              ##burnin=2000
                        burnin3 = 10,#0,            ##burnin3=1000
                        nr.traj = 20,             ##nr.traj=5000
                        use.correlation = TRUE,
                        replace.output = TRUE)  ##use.correlation=TRUE

## Note: We are using use.correlation=FALSE to save run time. 
## Normally, you should set it to TRUE in order to 
## account for correlation between countries.

## retrieve the tfr.pred object at later time

tfr.pred <- get.tfr.prediction(tfr.dir)

## 2. View the content of the simulation directory. 
## Folders predictions and thinned_mcmc_2_70 were added.

list.files(tfr.dir)
head(list.files(file.path(tfr.dir, 
                          "predictions")), 
     10)
list.files(file.path(tfr.dir, 
                     "thinned_mcmc_2_70"))

## The thinned_mcmc_2_70 directory contains one MCMC
## chain derived from the two original MCMCs 
## ( m2 with 120 iterations each) by applying a 
## burnin of 70 to each chain, collapsing into one 
## chain and thinning by two, in order to generate 
## 50 trajectories.

## 3. Inspect the content of the tfr.pred object:

typeof(tfr.pred)
names(tfr.pred)
names(tfr.pred$mcmc.set)
summary(tfr.pred$mcmc.set)

# Note that one can use the tfr.pred$mcmc.set 
# object in the functions of the previous
# Section if an exploration of the collapsed, 
# thinned and burned chain is desired. In such
# cases burnin is set to 0. For example, in

par(mfrow=c(1,1))
DLcurve.plot(tfr.pred$mcmc.set, 
             country = country, 
             burnin = 0)

# each curve corresponds to one trajectory in the prediction.

# 4. View projection results

summary(tfr.pred, 
        country = country)
tfr.trajectories.table(tfr.pred, 
                       country = country,
                       pi=c(80, 90))

# Plot projection trajectories
par(mfrow=c(1,1))
tfr.trajectories.plot(tfr.pred, 
                      country = country, 
                      nr.traj = 40)

# Use tfr.trajectories.plot.all for creating plots for all countries at once

# Generate maps of TFR:

tfr.map(tfr.pred)
tfr.map.gvis(tfr.pred)
tfr.map.gvis(tfr.pred, 
             year = 2030)

## 5. Export TFR trajectories. 

## By default tfr.predict  stores trajectories as 
## ASCII files in predictions/ascii_trajectories.csv and
## predictions/ascii_trajectories_wide.csv , which is
## controlled by the argument save.as.ascii . 
## To export trajectories manually, use the following 
## function (here exporting 20 trajectories into a 
## directory mytfrtrajs ):
convert.tfr.trajectories(tfr.dir, 
                         n = 40, 
                         output.dir = "mytfrtrajs")

##------------------------------------------------
## Useful Functions
##------------------------------------------------

# Names and codes of countries involved in the simulation
get.countries.table(tfr.pred)

# ... or in the Phase III estimation
get.countries.table(m3)

# Get country code, name, index
#get.country.object("Colombia", 
#                   m2$meta)

# ... the same as
#get.country.object(170, 
#                   m2$meta)

# ... and as
#get.country.object(178, 
#                   m2$meta, 
#                   index = TRUE)

# Obtain trajectories as a matrix
trajs <- get.tfr.trajectories(tfr.pred, "Niger")
dim(trajs)
