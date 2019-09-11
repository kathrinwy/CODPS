# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: export

# Date created: 11 September 2019
# Last updated: 11 September 2019

# Author: Kathrin Weny
# Maintainers: Kathrin Weny, Romesh Silva

setwd(output)

out.dir <- "regdata"

# e0.sim.dir <- "regdata/e0/e0simulationPredExtra"
# e0.sim.dir <- "e0simulation/predictions/prediction.rda"

# e0F.pred <- get.e0.prediction(e0.sim.dir)
# e0F.pred <- get.e0.prediction(sim.dir = file.path(getwd(),"e0simulation"))
# e0M.pred <- get.e0.jmale.prediction(e0F.pred)

# Sub-regional
location.file  <- file.path(output, "BFAlocations.txt")
regions        <- (subset(read.delim(location.file), location_type == 4))[,3:4]

# 2015 and 2020 populations for 13 rebions in BFA
traj <- get.pop.prediction(reg.pop.dir)

# Male
popM_2015  <- as.data.frame(traj$'quantilesMage'[1:13,1:27,5,1]) # select 1:13 regions, 1:27 age groups, 0.5 quantile and 2015
popM_2015  <- rownames_to_column(popM_2015, "reg_code")
popM_2015  <- popM %>%
  gather(age, popM_2015, -reg_code)


popM_2020 <- as.data.frame(traj$'quantilesMage'[1:13,1:27,5,1]) # select 1:13 regions, 1:27 age groups, 0.5 quantile and 2020
popM_2020  <- rownames_to_column(popM_2020, "reg_code")
popM_2020  <- popM %>%
  gather(age, popM_2020, -reg_code)



popM <- merge(popM_2015, by = c("reg_code", "age"))

# intrapolate years between 2015 and 2020
i <- 1
test2$'2016' <- test2[i,]+(test2[i,4] - test2[i,3])/5



write.csv(regtrajF, file = file.path(out.dir, "BFAe0Ftraj.csv"), row.names = FALSE, quote = FALSE)
write.csv(regtrajM, file = file.path(out.dir, "BFAe0Mtraj.csv"), row.names = FALSE, quote = FALSE)

setwd(code)