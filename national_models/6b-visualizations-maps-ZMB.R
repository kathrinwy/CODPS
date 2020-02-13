##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: maps
##
## Date created: 1 December 2020
## Last updated: 1 December 2020
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

options(scipen = 999)  # disable scientific notation in R

# Statistics South Africa mid-2019 estimates ----------------------------
iso <- "ZFA"

setwd(output)
dir.create(paste0("plots/",iso))

pop      <- read_xlsx(paste0(output,iso, "_adm0_adm1_pop_2019.xlsx"), sheet=2)

pop$Age <- as.character(pop$Age)


pop.plot <- as.data.frame(dplyr::select(pop, c("ADM1_EN", "ADM1_PCODE", "Age", "Sex", "pop_2019")))

# Prepare data for pop-pyramid
pop.plot$pop_2020 <- ifelse(pop.plot$Sex == "male", -1*pop.plot$pop_2019, pop.plot$pop_2019)

# Order age groups
pop.plot$Age <- as.factor(pop.plot$Age)
pop.plot$Age = factor(pop.plot$Age, levels(pop.plot$Age)[c(1,10,2:9,11:17)]) 

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
getwd()
# Export plot.list as png
png(paste0("plots/", iso, "/subnat.pyramids.png"), width = 40, height = length(regions)*7, units = "cm", res=350)
do.call(grid.arrange,c(plot.list, ncol = 2))
dev.off()

setwd(code)

