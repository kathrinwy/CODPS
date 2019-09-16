##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: subnational population projections
##
## Date created: 10 September 2019
## Last updated: 10 September 2019
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

pop.plot <- as.data.frame(dplyr::select(pop, c("ADM1_EN", "ADM1_PCODE", "Age", "Sex", "pop_2019")))

pop.plot$pop_2019 <- ifelse(pop.plot$Sex == "male", -1*pop.plot$pop_2019, pop.plot$pop_2019)

levels(pop.plot$Age) <- c("0-4", "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",  
                          "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+", "80+",
                          "80+",  "80+",  "80+",  "80+",  "80+",  "80+",  "80+",  "80+",  "80+")

regions   <- unique(pop.plot$ADM1_EN)
plot.list <- list()

for(i in regions){
  
temp <- pop.plot %>%
  filter(ADM1_EN == i)

plot.list[[i]] <- ggplot(temp , aes(x = Age, y = pop_2019, fill = Sex)) +   # Fill column
                  geom_bar(stat = "identity", width = .85) +   # draw the bars
                  coord_flip() + 
                  labs(title= paste(i, "- Population 2019"), y = "Population")+
                  theme(plot.title = element_text(hjust = .5), axis.title.y=element_blank(),
                  axis.ticks = element_blank()) +   
                  scale_fill_manual(values=c("#899DA4", "#C93312")) 
}

g <- grid.arrange(plot.list[[1]], plot.list[[2]],plot.list[[3]],plot.list[[4]],plot.list[[5]],
             plot.list[[6]], plot.list[[7]],plot.list[[8]],plot.list[[9]],plot.list[[10]],
             plot.list[[11]], plot.list[[12]],plot.list[[13]], ncol = 2)

ggsave(file = "subnat.pyramids.jpg", g)

# Pyramid Projections (2020, 2025, 2030) ----------------------------------
par(mfrow=c(1,1))
pop.trajectories.pyramid(regpop.pred, "Est",
                         year = c(2020, 2025, 2030), 
                         nr.traj = 10,
                         proportion = FALSE, 
                         age = 1:20,
                         pi = 80)

# Poptrajectories plot ----------------------------------------------------
jpeg("rplot.jpg", width = 350, height = "350")
pop.trajectories.plot(regpop.pred, country="Est", sum.over.ages = TRUE)
dev.off()

pop.trajectories.plot(regpop.pred, country="Est", age = 4:10, sex = c("female"), sum.over.ages = TRUE) # WRA 15-49

pop.trajectories.plot(regpop.pred, country="Est", age = 3:5,  sum.over.ages = TRUE) # Youth 10-24
pop.trajectories.plot(regpop.pred, country="Est", age = 3:4,  sum.over.ages = TRUE) # Adolescents


