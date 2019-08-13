##=======================================
##Bayesian Projections of Life Expectancy
##=======================================

##Goals
##References
##Setup
##Estimation of the life expectancy model
##Joint projection of female and male
##Useful Functions
##Small/Special-Case Countries
##Using Your Own Data and Imputation
##Converged Simulation
##Exercise

##=========================================
##Goals
##=========================================

##Basic familiarity with bayesLife,
##estimating Bayesian hierarchical model for female ,
##estimating the gap model for male and female ,
##projecting female and male for all countries into the future,
##viewing and exploring results,
##projecting for small/special-case countries,
##using own data for estimation and projection (TFR and ).

##=========================================
##References

##=========================================
##Raftery, A.E., Chunn, J.L., Gerland, P. and Ševčíková , H. (2013). 
##Bayesian Probabilistic Projections of Life Expectancy for All Countries. 
##Demography, 50:777-801.
##(http://link.springer.com/content/pdf/10.1007%2Fs13524-012-0193-x.pdf)
##
##Raftery, A.E., Lalic, N. and Gerland, P. (2014). Joint Probabilistic 
##Projection of Female and Male Life Expectancy. Demographic Research, 
##30:795-822. (http://www.demographicresearch.org/volumes/vol30/27/30-27.pdf)
##
##H. Ševčíková, A.E. Raftery and P. Gerland. Bayesian Probabilistic Population Projections:
##Do It Yourself. Presented at the 2014 Annual Meeting of the Population 
##Association of America, Boston, MA, May 2014. (http://paa2014.princeton.edu/abstracts/141301)
##
##WPP 2017 Methodological Report of the United Nations
##(https://esa.un.org/unpd/wpp/publications/Files/WPP2017_Methodology.pdf)
##
##Castanheira, H., Pelletier, F. and Ribeiro, I. (2017). A Sensitivity 
##Analysis of the Bayesian Framework for Projecting Life Expectancy at 
##Birth, UN Population Division, Technical Paper No. 7. New York: United Nations.
##(http://www.un.org/en/development/desa/population/publications/pdf/technical/TP2017-7.pdf)

##==========================================
##Setup
##==========================================
##In this session we will use the package bayesLife. If you followed Pre-requisites
##(../prerequisites.html), it should be installed on your computer. Load it into your
##namespace and use setwd() to navigate into your working directory:
library(bayesPop)
library(bayesLife)
library(wpp2015)
getwd()
setwd("~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop")

##Use sessionInfo() to make sure you are using version 3.2-0. If you get a lower version
##number, you might try to re-install the package using
#install.packages("bayesLife", dependencies = TRUE)

##==========================================
##Estimation of the life expectancy model
##==========================================
##1. Set a location on disk for bayesLife results:
e0.dir <- "e0simulation"

## add in Cameroon data for inclusion 
data(include_2015, package = "bayesLife")
countries <- subset(include_2015, 
                    name == "Cameroon")$country_code

##2. Run MCMCs to estimate the female model. Here we choose a toy simulation with
##   iterations to save computation time (40sec):
me0 <- run.e0.mcmc(sex = "Female", output.dir = e0.dir, 
                   iter = 1000,          #iter = 160000
                   nr.chains = 2,      ##nr.chains=3
                   thin = 5,           ##thin=10
                   verbose.iter = 20,  ##verbose.iter=100
                   wpp.year = 2015, 
                   start.year = 1873, 
                   present.year = 2005,
                   replace.output = TRUE)

me0 <- run.e0.mcmc.extra(sim.dir = e0.dir,
                         countries = countries,
                         burnin = 0,
                         verbose.iter = 20)
summary(me0)                                                                                                                                                                                                                                  

##  me0 is an object containing the MCMC meta info and pointing to the disk where MCMC
##  traces are stored. To obtain the object at later time, use
me0 <- get.e0.mcmc(e0.dir)

##3. View the content of the simulation directory, either in a file browser, 
##   terminal or in R:
list.files(e0.dir)
head(list.files(file.path(e0.dir, "mc1")), 20)

##4. View estimation results:
##View parameter traces for world parameters and for country-specific 
##parameters of a chosen country.
e0.partraces.plot(me0)
country <- "Cameroon"
e0.partraces.cs.plot(me0, country = country)
##Note: If you work in RStudio, you might get an error “figure margins too large”. 
##In such a case, preceed the call with dev.new() which opens a new
##graphic device that can be enlarged.

##Summarize estimation results.
summary(me0, country = country)

##Plot double logistic curves.
e0.DLcurve.plot(me0, country = country, 
                burnin = 200,
                nr.curves = 50)

##5. Continue MCMCs in each chain by 40 iterations (20sec):
me0 <- continue.e0.mcmc(output.dir = e0.dir, 
                        iter = 40)           ##iter=80000
summary(me0)

##6. Inspect the content of the me0 object:
names(me0)
names(me0$meta)
me0$meta$e0.matrix[, 1:5] # data for 1950-2015
me0$meta$suppl.data$e0.matrix[, 1:5] # data for 1870-1950
dim(me0$meta$suppl.data$e0.matrix)

##==========================================    
##  Joint projection of female and male
##==========================================    
##  1. The following function estimates the gap model and projects female 
##  and male jointly (40sec):
e0.pred <- e0.predict(sim.dir = e0.dir, 
                      end.year = 2030,
                      burnin = 70,            ##burnin=10000
                      nr.traj = 50,           ##NR.TRAJ=???  
                      replace.output = TRUE)

e0.pred <- e0.predict.extra(sim.dir = e0.dir)
##  To estimate and predict the gap model manually, use e0.jmale.estimate
##  and e0.jmale.predict .
##  To obtain the prediction object at later time use
e0.pred <- get.e0.prediction(e0.dir)

##  2. View the content of the simulation directory. Folders predictions ,
##  predictions/joint_male and thinned_mcmc_2_70 were added:
list.files(e0.dir)
head(list.files(file.path(e0.dir, "predictions")), 10)
head(list.files(file.path(e0.dir, "predictions", "joint_male")), 10)

##  3. View projection results:
##  Choose a country and summarize projection results.
country <- "Cameroon"
summary(e0.pred, country = country)
e0.trajectories.table(e0.pred, country = country,
                      both.sexes = TRUE, pi = c(60, 80, 90))
##  Plot projection trajectories.
e0.trajectories.plot(e0.pred, country = country, 
                     nr.traj = 30)
e0.trajectories.plot(e0.pred, country = country, 
                     both.sexes = TRUE)
e0.trajectories.plot(e0.pred, country = country, 
                     both.sexes = "A")
##  To generate such plots for all countries at once, 
##  use e0.trajectories.plot.all .
##  Plot the gap distribution.
e0.gap.plot(e0.pred, country = country, 
            nr.traj = 30)
##  Plot joint distribution of female male .
e0.joint.plot(e0.pred, country = country,
              years = c(2010, 2015, 2020, 2025, 2030))
##  Generate maps of .
e0.map(e0.pred)
e0.map.gvis(e0.pred)
e0.map.gvis(e0.pred, year = 2030, pi = 95)

##  4. Retrieve male prediction object (from directory or from the female prediction object):
e0M.pred <- get.e0.prediction(e0.dir, joint.male = TRUE)
e0M.pred <- get.e0.jmale.prediction(e0.pred)
summary(e0M.pred, country = country)

##  5. Retrieve trajectories (female and male):
trajF <- get.e0.trajectories(e0.pred, country = country)
trajM <- get.e0.trajectories(e0M.pred, country = country)
##  Probability that female will be more than 4 years 
##  above the male in all projected time  periods:
sum(apply(trajF - trajM > 2, 2, all))/ncol(trajF)

##  Useful Functions
# names and codes of countries involved in the simulation
get.countries.table(e0.pred)
# get country code, name, index
#  get.country.object("Canada", me0$meta)
# converts both female and male trajectories into ASCII
convert.e0.trajectories(e0.dir, output.dir = "mye0trajs")
