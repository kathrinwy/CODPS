##======================================================
##Subnational Population Projections
##======================================================
## We will now propagate the uncertainty in TFR subnational
## projections into subnational population projections, 
## using bayesPop. For life expectancy, we will use the
## same projections for all regions, namely the national 
## probabilistic projections generated in previous sessions.
library(readr)
##1. Set a location on disk where results will be stored.
reg.pop.dir <- "regPOPsimulation"

##2. One of the required inputs is a file with definitions 
##   of regions. The package contains an example of such 
##   location file for Canada. Let’s look at it.
data.dir <- file.path(find.package("bayesPop"), 
                      "extdata")
location.file <- file.path(data.dir, 
                           "CAMlocations.txt")
read.delim(location.file)
##
##    The region-specific ID column is called reg_code .
##    Regions with location_type = 4 will be processed 
##    while the rest will be ignored. The last entry 
##    (Canada) is needed for aggregation as well as for
##    determining the “default” country in pop.predict.subnat , 
##    see below.
##
##3. Locate files with historical age- and sex-specific 
##   population. Only population at the present time is
##   required.
# popM0.file <- file.path(data.dir, 
#                         "CAMpopM.csv")
# popF0.file <- file.path(data.dir, 
#                         "CAMpopF.csv")
# CAMpopM <- read.csv(popM0.file, 
#                     check.names = FALSE)
# CAMpopF <- read.csv(popF0.file, 
#                     check.names = FALSE)
colnames(CAMpopF) <- colnames(CAMpopM) <- c("reg_code","name","age","2005")
#write.table(CAMpopM[,2:5],file="regdata/CAMpopM.txt",sep = "\t",row.names=FALSE)
#write.table(CAMpopF[,2:5],file="regdata/CAMpopF.txt",sep = "\t",row.names=FALSE)

#write_delim(CAMpopM[,1:4],path="regdata/CAMpopM.txt",delim="\t")
#write_delim(CAMpopF[,1:4],path="regdata/CAMpopF.txt",delim="\t")
CAMpopM <- read.delim(file="regdata/CAMpopM.txt", comment.char='#', check.names=FALSE)
CAMpopF <- read.delim(file="regdata/CAMpopF.txt", comment.char='#', check.names=FALSE)
##Can delete following code -- was used for debugging
library(data.table)
popm <- data.table(read.delim(file="regdata/CAMpopM.txt", comment.char='#', check.names=FALSE))
unique(popm[, .(reg_code, name)])
# 
# 
# popm <- read.delim(file="regdata/CAMpopM.txt", comment.char='#', check.names=FALSE)
# popf <- read.delim(file="regdata/CAMpopF.txt", comment.char='#', check.names=FALSE)
# head(popm)
# head(popf)
# 
# cpopm <- read.delim(file="regdata/CANpopM.txt", comment.char='#', check.names=FALSE)
# cpopf <- read.delim(file="regdata/CANpopF.txt", comment.char='#', check.names=FALSE)
# head(cpopm)
# head(cpopf)


##4. Optionally, if region-specific net migration counts 
##   are not available, migration patterns for
##   distributing national migration can be specified.
##   Locate the example file with migration shares.
##pattern.file <- file.path(data.dir,
##                          "CANpatterns.txt")
##read.delim(pattern.file)

CAMpatterns <- data.table(read.delim(file="regdata/CAMpopM.txt", comment.char='#', check.names=FALSE))
CAMpatterns <- unique(CAMpatterns[, .(reg_code, name)])
CAMpatterns$inmigrationM_share <- 
  CAMpatterns$inmigrationF_share  <- 
  CAMpatterns$outmigrationM_share <- 
  CAMpatterns$outmigrationF_share <- 
  rep(0.001,nrow(CAMpatterns))

write_delim(CAMpatterns, 
            path = file.path(data.dir,"CAMpatterns.txt"),
            delim = "\t")
pattern.file <- file.path(data.dir,"CAMpatterns.txt")

##  Use e0 male and female trajectories for Mali
e0Ftraj <- read.csv("~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop/mye0trajs/F/ascii_trajectories_wide.csv", skip = 1)
e0Mtraj <- read.csv("~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop/mye0trajs/M/ascii_trajectories_wide.csv", skip = 1)
CAMe0Ftraj <- cbind(rep(120,nrow(e0Ftraj)), e0Ftraj[c(2,3,102)]) 
CAMe0Mtraj <- cbind(rep(120,nrow(e0Mtraj)), e0Mtraj[c(2,3,102)]) 
colnames(CAMe0Ftraj) <- c("LocID","Year","Trajectory","e0")
colnames(CAMe0Mtraj) <- c("LocID","Year","Trajectory","e0")
write.csv(CAMe0Ftraj,file="~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop/regdata/CAMe0Ftraj.csv", row.names = FALSE)
write.csv(CAMe0Mtraj,file="~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop/regdata/CAMe0Mtraj.csv", row.names = FALSE)

##   If migration shares are specified, it is used to 
##   distribute the national migration taken out of
##   the wpp2017 package. Note that such approach does 
##   not deal with the between-region migration.
##
##5. Generate subnational trajectories for all regions 
##   of one country.

sim.dir <- tempfile()  
regpop.pred <- pop.predict.subnat(end.year = 2030, 
                                  start.year = 1950, 
                                  present.year = 2005, 
                                  wpp.year = 2015, 
                                  output.dir = reg.pop.dir,
                                  locations = location.file,
                                  inputs = list(popM = file.path("regdata", "CAMpopM.txt"),
                                                popF = file.path("regdata", "CAMpopF.txt"),
                                                tfr.sim.dir = CAMtfr.dir,
                                                #e0F.sim.dir = file.path("regdata", "CAMe0Ftraj.csv"),
                                                #e0M.sim.dir =  file.path("regdata", "CAMe0Mtraj.csv"),
                                                # tfr.file="median_",
                                                e0F.file = file.path("regdata", "CAMe0Ftraj.csv"),
                                                e0M.file = file.path("regdata", "CAMe0Mtraj.csv"),
                                                patterns = pattern.file),
                                  verbose = TRUE, 
                                  nr.traj =30,
                                  replace.output=TRUE)

##   See ?pop.predict.subnat for the whole set of inputs. 
##   Missing inputs are replaced be the corresponding 
##   dataset of the “default” country from wpp2017. The 
##   default country is determined either by the record 
##   in location.file that have location_type = 0 , or 
##   it can be specifically given as an argument, here
##   it would be default.country = 124 .
##   Since the subnational simulation object has the same 
##   directory structure as in the national case, we can 
##   use the usual bayesPop functions for retrieving as
##   well as exploring the object.
##   For example, to retrieve the regpop.pred object at 
##   later time, use
regpop.pred <- get.pop.prediction(reg.pop.dir)
##6. Explore results.
pop.trajectories.plot(regpop.pred,
                      "Nord Ouest", 
                      sum.over.ages = TRUE)
pop.trajectories.plot(regpop.pred,
                      "Extreme-Nord", 
                      sum.over.ages = TRUE)
pop.trajectories.plot(regpop.pred,
                      "Nord", 
                      sum.over.ages = TRUE)
pop.pyramid(regpop.pred, 
            "Nord Ouest", 
            year = 2020)
pop.pyramid(regpop.pred, 
            "Extreme-Nord", 
            year = 2020)
pop.pyramid(regpop.pred, 
            "Nord", 
            year = 2020)
get.countries.table(regpop.pred)
##    Note that many bayesPop functions accept the
##    argument country , which in the subnational
##    case means sub-region.
##7. Aggregate regions to country.
regpop.aggr <- pop.aggregate.subnat(regpop.pred, 
                                    regions = 120,
                                    locations = location.file)
##    Currently, only aggregations to the country-level 
##    is available.
##8. Compare the aggregation to the direct national 
##   projections from our yesterday’s session.
nat.pop.dir <- "POPsimulation"
nat.pop.pred <- get.pop.prediction(nat.pop.dir)
pop.trajectories.plot(nat.pop.pred,
                      "Cameroon", 
                      sum.over.ages = TRUE)
pop.trajectories.plot(regpop.aggr,
                      "Cameroon", 
                      sum.over.ages = TRUE,
                      nr.traj = 0,
                      add = TRUE,
                      col = rep("blue",
                                4),
                      show.legend = FALSE)
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
       legend = c("West",
                  "East"),
       col = c("red", 
               "blue"), 
       lty = 1, 
       bty= "n",
       lwd = 2)
##======================================================
##Exercise
##======================================================
##Determine the probability that population of Western 
##states of Canada will be larger than
##population of the Eastern states by 2035.
##Tip: Construct an expression using numerators 
##from bullet 9. Use get.pop.ex() to obtain
##trajectories of that expression.

nso.adm1.proj <- read.csv("~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop/external-data/CAM_NSO_ADM1_projections.csv")

male.NO.2020 <- pop.byage.table(regpop.pred, 
                                country = "Nord Ouest",
                                pi = c(80, 90), year = 2020, sex = "male")[1:18,1]
male.NO.2020.l <- pop.byage.table(regpop.pred, 
                                  country = "Nord Ouest",
                                  pi = c(80, 90), year = 2020, sex = "male")[1:18,2]
male.NO.2020.u <- pop.byage.table(regpop.pred, 
                                  country = "Nord Ouest",
                                  pi = c(80, 90), year = 2020, sex = "male")[1:18,5]
##Nord Ouest
male.NO.2020 <- pop.byage.plot(regpop.pred, age=1:3,
                               country = "Nord Ouest",
                               year = 2020, sex = "male",las=1,ylab="",show.legend = FALSE,main="")
points(1:18, nso.adm1.proj$male.nordouest, type="l", col="darkgreen",lwd=2)
title(main="Males, Nord Ouest")

##Nord Ouest
plot(1:18,male.NO.2020,type="b",col="red",lwd=2,xaxt="n",xlab="",ylab="",las=1)
points(1:18,male.NO.2020.l,type="l",col="red",lwd=2)
points(1:18,male.NO.2020.u,type="l",col="red",lwd=2)
points(1:18, nso.adm1.proj$male.nordouest, type="b", col="darkgreen",lwd=2)
title(main="Nord Oest")
axis(side=1,
     at=1:18,
     labels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89"),
     las=2)
legend(13,150000,
       legend=c("BayesPop","NSO"),
       col=c("red","darkgreen"),
       lty = c(1,3),
       lwd=c(2,2),
       cex=.9,
       box.lty=0) 

##Extrem-Nord
male.EN.2020 <- pop.byage.table(regpop.pred, 
                                country = "Extreme-Nord",
                                pi = c(80, 90), year = 2020, sex = "male")[1:18,1]

male.EN.2020.l <- pop.byage.table(regpop.pred, 
                                  country = "Extreme-Nord",
                                  pi = c(80, 90), year = 2020, sex = "male")[1:18,2]
male.EN.2020.u <- pop.byage.table(regpop.pred, 
                                  country = "Extreme-Nord",
                                  pi = c(80, 90), year = 2020, sex = "male")[1:18,5]
nso.adm1.proj$male.extnord


plot(1:18,male.EN.2020,type="b",col="red",ylim=c(0,500000),lwd=2,xaxt="n",xlab="",ylab="",las=1)
points(1:18,male.EN.2020.l,type="l",col="red",lwd=2)
points(1:18,male.EN.2020.u,type="l",col="red",lwd=2)
points(1:18, nso.adm1.proj$male.extnord, type="b", col="darkgreen",lwd=2)
title(main="Extreme-Nord")
axis(side=1,
     at=1:18,
     labels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89"),
     las=2)
legend(13,150000,
       legend=c("BayesPop","NSO"),
       col=c("red","darkgreen"),
       lty = c(1,3),
       lwd=c(2,2),
       cex=.9,
       box.lty=0) 

##Nord
male.N.2020 <- pop.byage.table(regpop.pred, 
                               country = "Nord",
                               pi = c(80, 90), year = 2020, sex = "male")[1:18,1]
male.N.2020.l <- pop.byage.table(regpop.pred, 
                                 country = "Nord",
                                 pi = c(80, 90), year = 2020, sex = "male")[1:18,2]
male.N.2020.u <- pop.byage.table(regpop.pred, 
                                 country = "Nord",
                                 pi = c(80, 90), year = 2020, sex = "male")[1:18,5]

nso.adm1.proj$male.nord
plot(1:18,male.N.2020,type="b",col="red",lwd=2,xaxt="n",ylim=c(0,300000),xlab="",ylab="",las=1)
points(1:18,male.N.2020.l,type="l",col="red",lwd=2)
points(1:18,male.N.2020.u,type="l",col="red",lwd=2)
points(1:18, nso.adm1.proj$male.nord, type="b", col="darkgreen",lwd=2)
title(main="Nord")
axis(side=1,
     at=1:18,
     labels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89"),
     las=2)
legend(13,150000,
       legend=c("BayesPop","NSO"),
       col=c("red","darkgreen"),
       lty = c(1,3),
       lwd=c(2,2),
       cex=.9,
       box.lty=0) 

##Adamoua
male.A.2020 <- pop.byage.table(regpop.pred, 
                               country = "Adamaoua",
                               pi = c(80, 90), year = 2020, sex = "male")[1:18,1]
male.A.2020.l <- pop.byage.table(regpop.pred, 
                                 country = "Adamaoua",
                                 pi = c(80, 90), year = 2020, sex = "male")[1:18,2]
male.A.2020.u <- pop.byage.table(regpop.pred, 
                                 country = "Adamaoua",
                                 pi = c(80, 90), year = 2020, sex = "male")[1:18,5]

nso.adm1.proj$male.adamoua
plot(1:18,male.A.2020,type="b",col="red",lwd=2,xaxt="n",ylim=c(0,300000),xlab="",ylab="",las=1)
points(1:18,male.A.2020.l,type="l",col="red",lwd=2)
points(1:18,male.A.2020.u,type="l",col="red",lwd=2)
points(1:18, nso.adm1.proj$male.adamoua, type="b", col="darkgreen",lwd=2)
title(main="Adamaoua")
axis(side=1,
     at=1:18,
     labels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89"),
     las=2)
legend("topright",
       legend=c("BayesPop","NSO"),
       col=c("red","darkgreen"),
       lty = c(1,3),
       lwd=c(2,2),
       cex=.9,
       box.lty=0) 

pop.byage.plot(regpop.pred$quantilesMage[1:10,1:18,c(1,3,7,9),1:6],country="Nord") 

