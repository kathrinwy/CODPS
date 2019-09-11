##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: subnational population projections
##
## Date created: 15 August 2019
## Last updated: 16 August 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

# Setup -------------------------------------------------------------------
setwd(NER.output)

# Set a location on disk where results will be stored.
reg.pop.dir <- "regPOPsimulation"

# Location file defining geographical disaggregation

location.file.NER <- file.path(NER.output, "NERlocations.txt")
locations.NER     <- read.delim(location.file.NER)

# Locate files with historical age- and sex-specific population. Only population at the present time is required.
 popM0.file     <- file.path(NER.output, "NERpopM.adm1.csv")
 popF0.file     <- file.path(NER.output, "NERpopF.adm1.csv")
 
 NERpopM        <- read.csv(popM0.file, check.names = FALSE)
 names(NERpopM) <- c("name", "reg_code", "sex", "age1", "pop", "age")
 
 temp        <- NERpopM %>% 
   group_by(name, age) %>%
   dplyr::summarize(new  = sum(pop ))       # replace single year pop counts with 5 year counts

 NERpopM        <- NERpopM[,-c(3, 4, 5)]                                   # remove single year age column
 NERpopM        <- distinct(NERpopM)
 NERpopM$'2012' <- temp$new 
 
 # extrapolate population to 2015
 NERpopM$'2013' <- NERpopM$'2012'*(as.numeric(growth.NER[1,2])/100 +1)
 NERpopM$'2014' <- NERpopM$'2013'*(as.numeric(growth.NER[1,2])/100 +1)
 NERpopM$'2015' <- NERpopM$'2014'*(as.numeric(growth.NER[1,2])/100 +1)
 
 NERpopF <- read.csv(popF0.file, check.names = FALSE)
 names(NERpopF) <- c("name", "reg_code", "sex", "age1", "pop", "age")
 temp        <- NERpopF %>% 
   group_by(name, age) %>% 
   dplyr::summarize(new = sum(pop)) # replace single year pop counts with 5 year counts
 NERpopF        <- NERpopF[,-c(3,4,5)]                                   # remove single year age column
 NERpopF        <- distinct(NERpopF)
 NERpopF$'2012' <- temp$new 
 
 # extrapolate population to 2015
 NERpopF$'2013' <- NERpopF$'2012'*(as.numeric(growth.NER[1,2])/100 +1)
 NERpopF$'2014' <- NERpopF$'2013'*(as.numeric(growth.NER[1,2])/100 +1)
 NERpopF$'2015' <- NERpopF$'2014'*(as.numeric(growth.NER[1,2])/100 +1)
 
 # convert to tab delimited file
 write.table(NERpopM[,c(2,1,3,7)],file="regdata/NERpopM.txt",sep = "\t") # \t tab separated
 write.table(NERpopF[,c(2,1,3,7)],file="regdata/NERpopF.txt",sep = "\t")
 
# NERpopM <- read.delim(file="regdata/NERpopM.txt", comment.char='#', check.names=FALSE)
# NERpopF <- read.delim(file="regdata/NERpopF.txt", comment.char='#', check.names=FALSE)

# Optionally, if region-specific net migration counts are not available, migration patterns for
# distributing national migration can be specified. Locate the example file with migration shares.

NERpatterns <- data.table(read.delim(file="regdata/NERpopM.txt", comment.char='#', check.names=FALSE))
NERpatterns <- unique(NERpatterns[, .(reg_code, name)])

 NERpatterns$inmigrationM_share <- 
 NERpatterns$inmigrationF_share  <- 
 NERpatterns$outmigrationM_share <- 
 NERpatterns$outmigrationF_share <- 
  rep(0.001,nrow(NERpatterns))

write_delim(NERpatterns, path = file.path(NER.output,"regdata/NERpatterns.txt"), delim = "\t")

pattern.file <- file.path(NER.output,"regdata/NERpatterns.txt")

#  Use e0 male and female trajectories for Mali

e0Ftraj <- read.csv(paste0(NER.output, "mye0trajs/F/ascii_trajectories_wide.csv"), skip = 1)
e0Mtraj <- read.csv(paste0(NER.output, "mye0trajs/M/ascii_trajectories_wide.csv"), skip = 1)

# find Niger's column
grep("Niger", colnames(e0Ftraj)) # results in 134 (and 135 which is Nigeria)

NERe0Ftraj <- cbind(rep(562,nrow(e0Ftraj)), e0Ftraj[c(2,3,134)]) 
NERe0Mtraj <- cbind(rep(562,nrow(e0Mtraj)), e0Mtraj[c(2,3,134)]) 

colnames(NERe0Ftraj) <- c("LocID","Year","Trajectory","e0")
colnames(NERe0Mtraj) <- c("LocID","Year","Trajectory","e0")

write.csv(NERe0Ftraj,file= paste0(NER.output, "regdata/NERe0Ftraj.csv"), row.names = FALSE)
write.csv(NERe0Mtraj,file= paste0(NER.output, "regdata/NERe0Mtraj.csv"), row.names = FALSE)

# If migration shares are specified, it is used to distribute the national migration taken out of
# the wpp2017 package. Note that such approach does not deal with the between-region migration.

# Generate subnational trajectories for all regions of one country.
 
regpop.pred <- pop.predict.subnat(present.year = 2015, 
                                  wpp.year = 2017, 
                                  output.dir = reg.pop.dir,
                                  locations = locations.NER ,
                                  inputs = list(popM = file.path("regdata", "NERpopM.txt"),
                                                popF = file.path("regdata", "NERpopF.txt"),
                                                tfr.sim.dir = NERtfr.dir,
                                                e0F.file = file.path("regdata", "NERe0Ftraj.csv"),
                                                e0M.file = file.path("regdata", "NERe0Mtraj.csv"),
                                                patterns = pattern.file))

regpop.pred <- get.pop.prediction(reg.pop.dir)
dim(NERe0Mtraj)
dim(NERpopM)
# Explore results.
pop.trajectories.plot(regpop.pred,
                      "Nord Ouest", 
                      sum.over.ages = TRUE)

pop.pyramid(regpop.pred, 
            "Nord Ouest", 
            year = 2020)

get.countries.table(regpop.pred)

##    Note that many bayesPop functions accept the argument country , which in the subnational
##    case means sub-region.



##9. Prediction objects can be used for expressions 
##   as usual, including combining regions with 
##   aggregations.
# Ratio of Western regions to Canada
pop.trajectories.plot(regpop.pred,
                      expression = "(P658 + P659)/P124",
                      main = "Region-Country Population Ratio (Canada)",
                      nr.traj = 10, 
                      show.legend = FALSE, 
                      ylim = c(0.1, 0.4))
# Ratio of Eastern regions to Canada
pop.trajectories.plot(regpop.pred,
                      expression = "(P668 + P662 + P664 + P661 + P667)/P124",
                      nr.traj = 10,
                      col = rep("blue", 4),
                      add = TRUE,
                      show.legend = FALSE)
legend("topright", 
       legend = c("West", "East"),
       col = c("red", "blue"), 
       lty = 1, 
       bty= "n",
       lwd = 2)

setwd(NER.code)