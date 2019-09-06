##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: Bayesian Projections of Life Expectancy
##
## Date created: 6 September 2019
## Last updated: 6 September 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

setwd(output)
e0.dir <- "e0simulation"
country <- "Burkina Faso"

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

# Plot projection trajectories --------------------------------------------
par(mfrow=c(1,2))
e0.trajectories.plot(e0.pred, country = country, 
                     both.sexes = TRUE)

#  Plot joint distribution of female male 
e0.joint.plot(e0.pred, country = country,
              years = c(2010, 2015, 2020, 2025, 2030))

# Retrieve male prediction object (from directory or from the female prediction object):
e0M.pred <- get.e0.prediction(e0.dir, joint.male = TRUE)
e0M.pred <- get.e0.jmale.prediction(e0.pred)

# converts both female and male trajectories into ASCII
convert.e0.trajectories(e0.dir, output.dir = "mye0trajs")

setwd(code)