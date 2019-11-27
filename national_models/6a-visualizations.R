##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: subnational population projections
##
## Date created: 10 September 2019
## Last updated: 16 September 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

# PCode     region
# BFA01     Boucle de Mouhoun
# BFA02     Cascades
# BFA03     Centre including Ouagadougou
# BFA04     Centre-Est
# BFA05     Centre-Nord
# BFA06     Centre-Ouest
# BFA07     Centre-Sud
# BFA08     Est
# BFA09     Hauts Basins
# BFA10     Nord
# BFA11     Plateau Central
# BFA12     Sahel
# BFA13     Sud-Ouest

# Probabilistic population pyramids (2019) ---------------------------------------
setwd(output)

pop      <- read.csv("G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/BFA_adm1_pop_2015_2020.csv")

pop.plot <- as.data.frame(dplyr::select(pop, c("ADM1_EN", "ADM1_PCODE", "Age", "Sex", "pop_2020")))

# Prepare data for pop-pyramid
pop.plot$pop_2020 <- ifelse(pop.plot$Sex == "male", -1*pop.plot$pop_2020, pop.plot$pop_2020)

# Adjust levels (age is stored as factor)
levels(pop.plot$Age) <- c("0", "1-4",  "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                          "40-44", "45-49", "5-9", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+", "80+")

# Order age groups
pop.plot$Age = factor(pop.plot$Age, levels(pop.plot$Age)[c(1, 2, 11, 3:10,  12:18)]) 

# Prepare loop (results are to be stored in the plot.list)
regions   <- unique(pop.plot$ADM1_EN)
plot.list <- list()

# Loop over age pyramids
for(i in regions){
  
temp <- pop.plot %>%
  filter(ADM1_EN == i)

plot.list[[i]] <- ggplot(temp , aes(x = Age, y = pop_2020, fill = Sex)) +   # Fill column
                  geom_bar(stat = "identity", width = .85) +   # draw the bars
                  coord_flip() + 
                  labs(title= paste(i, "- 2020"), y = "Population")+
                  theme(plot.title = element_text(hjust = .5), axis.title.y=element_blank(),
                  axis.ticks = element_blank()) +   
                  scale_y_continuous(breaks = seq(from = -100000, to = 100000, by = 50000), # so that x-axis does not contain negative values
                                     labels=c( 100000,  50000, 0, 50000, 100000)) +
                  scale_fill_manual(values=c("#899DA4", "#C93312")) 
}

# Export plot.list as png

g <- grid.arrange(plot.list[[1]], plot.list[[2]],plot.list[[3]],plot.list[[4]],plot.list[[5]],
             plot.list[[6]], plot.list[[7]],plot.list[[8]],plot.list[[9]],plot.list[[10]],
             plot.list[[11]], plot.list[[12]],plot.list[[13]], ncol = 3)

ggsave(file = "plots/BFA/subnat.pyramids.png", g, height = 40, width=35, units = "cm")

# Poptrajectories plot ----------------------------------------------------

# 1-5 
png("plots/BFA/pop.trajectoriesI.png", width = 30, height = 50, units = "cm", res=350)

par(mfrow=c(5,3)) 
for(i in regions[1:5]){
pop.trajectories.plot(regpop.pred, country= i, sum.over.ages = TRUE)
pop.trajectories.plot(regpop.pred, country= i, age = 4:10, sex = c("female"), sum.over.ages = TRUE) # WRA 15-49
pop.trajectories.plot(regpop.pred, country= i, age = 3:5,  sum.over.ages = TRUE) # Youth 10-24
}

dev.off()

# 6-10
png("plots/BFA/pop.trajectoriesII.png", width = 30, height = 50, units = "cm", res=350)

par(mfrow=c(5,3)) 
for(i in regions[6:10]){
  pop.trajectories.plot(regpop.pred, country= i, sum.over.ages = TRUE)
  pop.trajectories.plot(regpop.pred, country= i, age = 4:10, sex = c("female"), sum.over.ages = TRUE) # WRA 15-49
  pop.trajectories.plot(regpop.pred, country= i, age = 3:5,  sum.over.ages = TRUE) # Youth 10-24
}

dev.off()

# 11-13

png("plots/BFA/pop.trajectoriesIII.png", width = 20, height = 20, units = "cm", res=350)

par(mfrow=c(3,3)) 
for(i in regions[11:13]){
  pop.trajectories.plot(regpop.pred, country= i, sum.over.ages = TRUE)
  pop.trajectories.plot(regpop.pred, country= i, age = 4:10, sex = c("female"), sum.over.ages = TRUE) # WRA 15-49
  pop.trajectories.plot(regpop.pred, country= i, age = 3:5,  sum.over.ages = TRUE) # Youth 10-24
}

dev.off()


getwd()