library(bayesLife)

out.dir <- "regdata"

##e0.sim.dir <- "regdata/e0/e0simulationPredExtra"
e0.sim.dir <- "e0simulation/predictions/prediction.rda"
##e0F.pred <- get.e0.prediction(e0.sim.dir)
e0F.pred <- get.e0.prediction(sim.dir = file.path(getwd(),"e0simulation"))
e0M.pred <- get.e0.jmale.prediction(e0F.pred)

data.dir <- file.path(find.package("bayesPop"), "extdata")
location.file <- file.path(data.dir, "CAMlocations.txt")
regions <- subset(read.delim(location.file), location_type == 4)$reg_code

copy.national.to.regional.e0 <- function(e0.pred, regions) {
  traj <- get.e0.trajectories(e0.pred, "Cameroon")
  ntraj <- ncol(traj)
  colnames(traj) <- paste0("e0_", 1:ntraj)
  trajw <- cbind(Year = rownames(traj), data.frame(traj))
  trajl <- reshape(trajw, direction = "long", varying = 2:(ntraj+1), 
                   sep = "_", timevar = "Trajectory")
  trajl$id <- NULL
  
  reg.e0traj <- NULL
  for(reg in regions) 
    reg.e0traj <- rbind(reg.e0traj, cbind(LocID = reg, trajl))
  reg.e0traj
}

regtrajF <- copy.national.to.regional.e0(e0F.pred, regions)
regtrajM <- copy.national.to.regional.e0(e0M.pred, regions)

write.csv(regtrajF, file = file.path(out.dir, "CAMe0Ftraj.csv"), row.names = FALSE, quote = FALSE)
write.csv(regtrajM, file = file.path(out.dir, "CAMe0Mtraj.csv"), row.names = FALSE, quote = FALSE)
