##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: subnational population projections
##
## Date created: 10 September 2019
## Last updated: 4 December 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

# Probabilistic population pyramids (2019) ---------------------------------------
setwd(output)
dir.create(paste0("plots/",iso))

pop      <- read.csv(paste0(output,iso, "_adm1_pop_2015_2020.csv"))

pop$Age <- as.character(pop$Age)

pop$Age <- ifelse(pop$Age == "'5-9'", "5-9",
                  ifelse(pop$Age == "'10-14'", "10-14", 
                         ifelse(pop$Age == "'0-4'", "0-4", pop$Age)))

pop$Age <- ifelse(pop$Age == "80-84", "80+",
                  ifelse(pop$Age == "85-89", "80+", 
                         ifelse(pop$Age == "90-94", "80+", 
                                ifelse(pop$Age == "95-99", "80+",
                                       ifelse(pop$Age == "100-104", "80+",
                                              ifelse(pop$Age == "105-109", "80+",
                                                     ifelse(pop$Age == "110-114", "80+",
                                                            ifelse(pop$Age == "115-119", "80+",
                                                                   ifelse(pop$Age == "120-124","80+",
                                                                          ifelse(pop$Age == "125-129", "80+", pop$Age))))))))))

pop.plot <- as.data.frame(dplyr::select(pop, c("ADM1_EN", "ADM1_PCODE", "Age", "Sex", "pop_2020")))

# Prepare data for pop-pyramid
pop.plot$pop_2020 <- ifelse(pop.plot$Sex == "male", -1*pop.plot$pop_2020, pop.plot$pop_2020)

# Order age groups
pop.plot$Age <- as.factor(pop.plot$Age)
pop.plot$Age = factor(pop.plot$Age, levels(pop.plot$Age)[c(1, 10, 2:9,  11:18)]) 

# Prepare loop (results are to be stored in the plot.list)
regions   <- c(as.character(unique(pop.plot$ADM1_EN)))
plot.list <- list()

# Loop over age pyramids
for(i in regions){

temp <- pop.plot %>%
  filter(ADM1_EN == i)

if((match(i, regions) %% 2) == 0) {
  
plot.list[[i]] <- ggplot(temp , aes(x = Age, y = pop_2020, fill = Sex)) +   # Fill column
                  geom_bar(stat = "identity", width = .85) +   # draw the bars
                  coord_flip() + 
                  labs(title= paste(i, "- 2020"), y = "Population")+
                  theme(plot.title = element_text(hjust = .5, size =22), axis.title.y=element_blank(),
                  axis.ticks = element_blank(), axis.text.y=element_blank(),
                  axis.text.x = element_text(size = 16), axis.title.x = element_text(size = 16), 
                  legend.text = element_text(size=16), legend.title = element_text(size=16), legend.position = c(0.88,0.8)) +   
                  scale_y_continuous(breaks = seq(from = -max(temp$pop_2020), to = max(temp$pop_2020), by = max(temp$pop_2020)/2),
                                     labels=c(round(max(temp$pop_2020)/1000,0)*1000,  round(max(temp$pop_2020)/2000,0)*1000, 
                                              0, 
                                              round(max(temp$pop_2020)/2000,0)*1000, round(max(temp$pop_2020)/1000,0)*1000)) +
                  scale_fill_manual(values=c("#899DA4", "#C93312")) 
}

else {
  
  plot.list[[i]] <- ggplot(temp , aes(x = Age, y = pop_2020, fill = Sex)) +   # Fill column
    geom_bar(stat = "identity", width = .85) +   # draw the bars
    coord_flip() + 
    labs(title= paste(i, "- 2020"), y = "Population")+
    theme(plot.title = element_text(hjust = .5, size =22), axis.title.y=element_blank(),
          axis.ticks = element_blank(), axis.text.y = element_text(size = 16),
          legend.text = element_text(size=16), legend.title = element_text(size=16), legend.position = c(0.88,0.8),
          axis.text.x = element_text(size = 16), axis.title.x = element_text(size = 16)) +   
    scale_y_continuous(breaks = seq(from = -max(temp$pop_2020), to = max(temp$pop_2020), by = max(temp$pop_2020)/2),
                       labels=c(round(max(temp$pop_2020)/1000,0)*1000,  round(max(temp$pop_2020)/2000,0)*1000, 
                                0, 
                                round(max(temp$pop_2020)/2000,0)*1000, round(max(temp$pop_2020)/1000,0)*1000)) +
    scale_fill_manual(values=c("#899DA4", "#C93312")) 
}
}

# Export plot.list as png
png(paste0("plots/", iso, "/subnat.pyramids.png"), width = 40, height = length(regions)*7, units = "cm", res=350)
do.call(grid.arrange,c(plot.list, ncol = 2))
dev.off()

# Poptrajectories plot ----------------------------------------------------

for(i in regions[1:nrow(traj$countries)]){

png(paste0("plots/",iso, "/sex_age_",iso, i, ".png"), width = 30, height = 10, units = "cm", res=350)

par(mfrow=c(1,3))  
pop.trajectories.plot(regpop.pred, country= i, sum.over.ages = TRUE)
pop.trajectories.plot(regpop.pred, country= i, age = 4:10, sex = c("female"), sum.over.ages = TRUE) # WRA 15-49
pop.trajectories.plot(regpop.pred, country= i, age = 3:5,  sum.over.ages = TRUE) # Youth 10-24
dev.off()

}

