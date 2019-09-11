##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: Bayespop projections
##
## Date created: 6 September 2019
## Last updated: 6 September 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

# Estimation of the TFR model ---------------------------------------------

# Set a location on disk for TFR results
setwd(output)
tfr.dir <- "TFRsimulation"

# Run Phase II MCMCs ------------------------------------------------------

# This is a toy simulation and not enough to make the series converge.
# m2 <- run.tfr.mcmc(output.dir = tfr.dir, 
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

# Export TFR trajectories 
convert.tfr.trajectories(tfr.dir, n = 50, output.dir = "mytfrtrajs") 

# Estimation of the life expectancy  model --------------------------------

e0.dir <- "e0simulation"

# Run MCMCs to estimate the female model. 
# me0 <- run.e0.mcmc(sex = "Female", output.dir = e0.dir, 
#                 iter = 80,          
#                nr.chains = 2,     
#               thin = 5,           
#              verbose.iter = 20,  
#             wpp.year = 2017, 
#            start.year = 1873, 
#           present.year = 2015,
#          replace.output = TRUE)

# me0 <- get.e0.mcmc(e0.dir)
# We will use the fully converged version: https://bayespop.csss.washington.edu/download/#subnatTFR

# Joint projection of female and male -------------------------------------

# Estimates gap model and projects female and male jointly 
# e0.pred <- e0.predict(sim.dir = e0.dir, 
#                    end.year = 2030,  
#                   burnin = 70, 
#                  nr.traj = 50)

# We will use the fully converged version: https://bayespop.csss.washington.edu/download/#subnatTFR
e0.pred <- get.e0.prediction(e0.dir)

# Retrieve male prediction object (from directory or from the female prediction object):
e0M.pred <- get.e0.prediction(e0.dir, joint.male = TRUE)
e0M.pred <- get.e0.jmale.prediction(e0.pred)

# converts both female and male trajectories into ASCII
convert.e0.trajectories(e0.dir, output.dir = "mye0trajs")

# Estimation pop projecitons - national level -----------------------------

tfr.dir <- "TFRsimulation"
e0.dir  <- "e0simulation"
pop.dir <- "POPsimulation"

# Projections using our probabilistic TFR ---------------------------------

pop.pred <- pop.predict( end.year = 2030, 
                         start.year = 1950, 
                         present.year = 2015,   # 2015 projected from 2012 at later stage for Niger/BFA
                         wpp.year = 2017,       # ERROR MESSAGE: vwBaseYear2019 not found: migration error, so use wpp2017
                         output.dir = pop.dir, 
                         nr.traj = 50,
                         inputs = list(tfr.sim.dir = tfr.dir,
                                       e0F.sim.dir = e0.dir,
                                       e0M.sim.dir = "joint_"),
                         keep.vital.events = FALSE,
                         replace.output=TRUE)

pop.pred <- get.pop.prediction(pop.dir)

setwd(code)

