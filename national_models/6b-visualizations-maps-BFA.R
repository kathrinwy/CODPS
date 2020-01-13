##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: maps
##
## Date created: 11 September 2020
## Last updated: 16 September 2020
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

options(scipen = 999)  # disable scientific notation in R

# Prepare data for pop-pyramid
pop.plot$pop_2020 <- ifelse(pop.plot$Sex == "male", -1*pop.plot$pop_2020, pop.plot$pop_2020)

pop.plot$Age <- as.character(pop.plot$Age)
WRA <- pop.plot %>%
  filter(Age == "15-19" | Age == "20-24" | Age == "25-29	" |Age == "30-34" | Age == "35-39" | Age == "40-44" | Age == "45-49") %>%
  filter(Sex == "female") %>%
  group_by(ADM1_EN) %>%  
  dplyr::summarise(pop = sum(pop_2020))  


Youth <- pop.plot%>%
  filter(Age == "10-14" | Age == "15-19" | Age == "20-24") %>%
  group_by(ADM1_EN) %>%  
  dplyr::summarise(pop = sum(pop_2020))   

setwd(paste0(input, iso, "-shapefiles"))

geo <- readOGR(".", "sdr_subnational_boundaries") # load shapefile for  DHS data
geo@data$DHSREGEN <- as.character(geo@data$DHSREGEN)
unique(geo@data$DHSREGEN)
geo[geo@data$DHSREGEN == "Boucle du Mouhoun", "DHSREGEN"] <- "Boucle du Mouhoun"
geo[geo@data$DHSREGEN == "Hauts-Bassins", "DHSREGEN"] <- "Hauts-Bassins"
geo[geo@data$DHSREGEN == "Centre", "DHSREGEN"] <- "Centre including Ouagadougou"

subset <- geo
subset@data$ID2 <- paste(subset@data$DHSREGEN)

#Correct country names
subset_data <- tidy(subset)%>% #Command takes medium amount of time
  tbl_df()

#df3 linking data, called geo_data_1
geo_data_1 <- subset@data%>%
  tbl_df()%>%
  rownames_to_column(var="id")%>%
  dplyr::select(id,ID2)%>%
  mutate(ID2 = as.character(ID2))

geo_data_1$id <- as.numeric(geo_data_1$id)-1

geo_data_1$id <- as.character(geo_data_1$id)


# Women of reproductive age (15-49) ---------------------------------------

data <- WRA 

data_match <- data %>%
  tbl_df()

colnames(data_match) <- c("ID2", "Data") 

data_match <- left_join(data_match, geo_data_1,by=c("ID2"="ID2"))

data_match <- data_match %>%
  group_by(id)

#Second link (new) df2(data_match) and df4

mapping <- left_join(subset_data,data_match)
b <- as.data.frame(unique(mapping$ID2))
a <- as.data.frame(subset@data$ID2)

# Maps

plot <- ggplot(data=mapping, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  geom_polygon()+
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse", limits = c(max(mapping$Data), 0), 
                       breaks = c(0, max(mapping$Data)*0.25, 
                                  max(mapping$Data)*0.5, 
                                  max(mapping$Data)*0.75, 
                                  max(mapping$Data)),
                       labels=c(0, round(max(mapping$Data)*0.00025,0)*1000, 
                                round(max(mapping$Data)*0.0005,0)*1000, 
                                round(max(mapping$Data)*0.00075,0)*1000, 
                                round(max(mapping$Data/1000,0))*1000))+
  labs(title = "Women of Reproductive Age (15-49), by Administrative Region 1", 
       fill  = "Number of women \n of reproductive age (15-49)")+
  theme_void()+
  theme(legend.position = c(0.15, 0.8))+
  coord_equal()

grob.BFA01 <- grobTree(textGrob("Boucle du \n Mouhoun", x=0.23,  y=0.57, hjust=0,
                              gp=gpar(col="black", fontsize=9, fontface="italic"))) # OK
grob.BFA02 <- grobTree(textGrob("Cascades", x=0.12,  y=0.22, hjust=0,
                              gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.BFA03 <- grobTree(textGrob("Centre \n incl. \n Ouaga.", x=0.45,  y=0.52, hjust=0,
                              gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.BFA04 <- grobTree(textGrob("Centre-Est", x=0.6,  y=0.43, hjust=0,
                              gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.BFA05 <- grobTree(textGrob("Centre-Nord", x=0.48,  y=0.7, hjust=0,
                              gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.BFA06 <- grobTree(textGrob("Centre-Ouest", x=0.35,  y=0.45, hjust=0,
                              gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.BFA07 <- grobTree(textGrob("Centre-Sud", x=0.53,  y=0.38, hjust=0,
                              gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.BFA08 <- grobTree(textGrob("Est", x=0.8,  y=0.485, hjust=0,
                              gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.BFA09 <- grobTree(textGrob("Hauts-Bassin", x=0.15,  y=0.35, hjust=0,
                              gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.BFA10 <- grobTree(textGrob("Nord", x=0.4,  y=0.75, hjust=0,
                              gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.BFA11 <- grobTree(textGrob("Plateau \n Central", x=0.55,  y=0.52, hjust=0,
                              gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.BFA12 <- grobTree(textGrob("Sahel", x=0.6,  y=0.9, hjust=0,
                              gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.BFA13 <- grobTree(textGrob("Sud Ouest", x=0.25,  y=0.22, hjust=0,
                              gp=gpar(col="black", fontsize=9, fontface="italic")))

plot.label <- plot +
  annotation_custom(grob.BFA01) +
  annotation_custom(grob.BFA02) +
  annotation_custom(grob.BFA03) +
  annotation_custom(grob.BFA04) +
  annotation_custom(grob.BFA05) +
  annotation_custom(grob.BFA06) +
  annotation_custom(grob.BFA07) +
  annotation_custom(grob.BFA08) +
  annotation_custom(grob.BFA09) +
  annotation_custom(grob.BFA10) +
  annotation_custom(grob.BFA11) +
  annotation_custom(grob.BFA12) +
  annotation_custom(grob.BFA13) 

setwd(output)
ggsave(file = paste("plots/BFA/WRA_total.png"), print(plot.label), dpi = 900)


# Youth -------------------------------------------------------------------

data <- Youth 

data_match <- data %>%
  tbl_df()

colnames(data_match) <- c("ID2", "Data") 

data_match <- left_join(data_match, geo_data_1,by=c("ID2"="ID2"))

data_match <- data_match %>%
  group_by(id)

#Second link (new) df2(data_match) and df4

mapping <- left_join(subset_data,data_match)
b <- as.data.frame(unique(mapping$ID2))
a <- as.data.frame(subset@data$ID2)

# Maps

plot <- ggplot(data=mapping, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  geom_polygon()+
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse", limits = c(max(mapping$Data), 0), 
                       breaks = c(0, max(mapping$Data)*0.25, 
                                  max(mapping$Data)*0.5, 
                                  max(mapping$Data)*0.75, 
                                  max(mapping$Data)),
                       labels=c(0, round(max(mapping$Data)*0.00025,0)*1000, 
                                round(max(mapping$Data)*0.0005,0)*1000, 
                                round(max(mapping$Data)*0.00075,0)*1000, 
                                round(max(mapping$Data/1000,0))*1000))+
  labs(title = "Youth (10-24), by Administrative Region 1", 
       fill  = "Youth (10-24)")+
  theme_void()+
  theme(legend.position = c(0.12, 0.8))+
  coord_equal()

plot.label <- plot +
  annotation_custom(grob.BFA01) +
  annotation_custom(grob.BFA02) +
  annotation_custom(grob.BFA03) +
  annotation_custom(grob.BFA04) +
  annotation_custom(grob.BFA05) +
  annotation_custom(grob.BFA06) +
  annotation_custom(grob.BFA07) +
  annotation_custom(grob.BFA08) +
  annotation_custom(grob.BFA09) +
  annotation_custom(grob.BFA10) +
  annotation_custom(grob.BFA11) +
  annotation_custom(grob.BFA12) +
  annotation_custom(grob.BFA13) 

setwd(output)
ggsave(file = paste("plots/BFA/Youth_total.png"), print(plot.label), dpi = 900)




# Comparison with Institut Geographique National de la Statistique --------

setwd(output)

pop      <- read.csv("bfa_population_statistic_2019_ign.csv")

pop.plot <- as.data.frame(dplyr::select(pop, c("ADM1_EN", "ADM1_PCODE", "Age", "Sex", "pop_2020")))

pop.plot

# Prepare data for pop-pyramid
pop.plot$pop_2020 <- ifelse(pop.plot$Sex == "male", -1*pop.plot$pop_2020, pop.plot$pop_2020)

pop.plot$Age = factor(pop.plot$Age, levels(pop.plot$Age)[c(1,10, 2:9,  11:13)]) 

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
png(paste0("plots/", iso, "/subnat.pyramids.INS.png"), width = 40, height = length(regions)*7, units = "cm", res=350)
do.call(grid.arrange,c(plot.list, ncol = 2))
dev.off()

setwd(code)
