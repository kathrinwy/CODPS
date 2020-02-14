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

# ADM1
setwd(output)

pop.plot      <- read_xlsx(paste0(output,iso, "_adm0_adm1_adm2_2020.xlsx"), sheet=2)

pop.plot$pop_2020 <- ifelse(pop.plot$Sex == "male", -1*pop.plot$pop_2020, pop.plot$pop_2020)

pop.plot$Age <- as.character(pop.plot$Age)
WRA <- pop.plot %>%
  filter(Age == "15-19" | Age == "20-24" | Age == "25-29" |Age == "30-34" | Age == "35-39" | Age == "40-44" | Age == "45-49") %>%
  filter(Sex == "Female") %>%
  group_by(ADM1_EN) %>%  
  dplyr::summarise(pop = sum(pop_2020))  

Youth <- pop.plot 

Youth <- Youth%>%
  filter(Age == "10-14" | Age == "15-19" | Age == "20-24") %>%
  group_by(ADM1_EN) %>%  
  dplyr::summarise(pop = sum(pop_2020))  


setwd(paste0(input, iso, "-shapefiles", "/ADM1"))

geo <- readOGR(".", "geo1_zm2010") # load shapefile for  DHS data
geo@data$ADMIN_NAME <- as.character(geo@data$ADMIN_NAME)
#geo@data[geo@data$ADMIN_NAME == "Eastern, Muchinga, Northern", "ADMIN_NAME"] <- "Eastern, Northern, Muchinga"

subset <- geo
subset@data$ID2 <- paste(subset@data$ADMIN_NAME)

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

grob.ZMB01 <- grobTree(textGrob("North Western", x=0.23,  y=0.57, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic"))) # OK
grob.ZMB02 <- grobTree(textGrob("Western", x=0.12,  y=0.22, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.ZMB03 <- grobTree(textGrob("Copperbelt", x=0.45,  y=0.5, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.ZMB04 <- grobTree(textGrob("Eastern, Northern, \n Muchinga", x=0.75,  y=0.65, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.ZMB06 <- grobTree(textGrob("Lusaka", x=0.55,  y=0.29, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.ZMB07 <- grobTree(textGrob("Center", x=0.53,  y=0.38, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.ZMB09 <- grobTree(textGrob("Southern", x=0.35,  y=0.15, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.ZMB10 <- grobTree(textGrob("Luapula", x=0.58,  y=0.65, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic")))


plot.label <- plot +
  annotation_custom(grob.ZMB01) +
  annotation_custom(grob.ZMB02) +
  annotation_custom(grob.ZMB03) +
  annotation_custom(grob.ZMB04) +
  annotation_custom(grob.ZMB06) +
  annotation_custom(grob.ZMB07) +
  annotation_custom(grob.ZMB09) +
  annotation_custom(grob.ZMB10) 

ggsave(file = paste(paste0(output, "plots/", iso, "/WRA_total.png")), print(plot.label), dpi = 900)


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
  annotation_custom(grob.ZMB01) +
  annotation_custom(grob.ZMB02) +
  annotation_custom(grob.ZMB03) +
  annotation_custom(grob.ZMB04) +
  annotation_custom(grob.ZMB06) +
  annotation_custom(grob.ZMB07) +
  annotation_custom(grob.ZMB09) +
  annotation_custom(grob.ZMB10)   

ggsave(file = paste(paste0(output, "plots/", iso, "/Youth_total.png")), print(plot.label), dpi = 900)


# ADM2

pop.plot      <- read_xlsx(paste0(output,iso, "_adm0_adm1_adm2_2020.xlsx"), sheet=3)

pop.plot$Age <- as.character(pop.plot$Age)

WRA <- pop.plot %>%
  dplyr::filter(Age == "15-19" | Age =="20-24" | 
                  Age =="25-29" | Age =="30-34" | 
                  Age =="35-39" | Age =="40-44" | 
                  Age =="45-49") %>%
  dplyr::filter(Sex == "Female") %>%
  group_by(ADM2_EN) %>%  
  dplyr::summarise(pop = sum(pop_2020))  

Youth <- pop.plot %>%
  dplyr::filter(Age == "10-14" | Age == "15-19" | Age == "20-24") %>%
  group_by(ADM2_EN) %>%  
  dplyr::summarise(pop = sum(pop_2020))  


setwd(paste0(input, iso, "-shapefiles/ADM2"))

geo <- readOGR(".", "geo2_zm2010") # load shapefile for  DHS data

unique(geo@data$ADMIN_NAME)
geo@data$ADMIN_NAME <- as.character(geo@data$ADMIN_NAME)

geo@data[geo@data$ADMIN_NAME == "Itezhi Tezhi", "ADMIN_NAME"]                 <- "Itezhi-tezhi"
geo@data[geo@data$ADMIN_NAME == "Kapiri Mposhi", "ADMIN_NAME"] <- "Kapiri Mkushi"
geo@data[geo@data$ADMIN_NAME == "Mufumbwe (Chizera)", "ADMIN_NAME"]     <- "Mufumbwe"

subset <- geo
subset@data$ID2 <- paste(subset@data$ADMIN_NAME)

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
 # xlim(10, 35) +
#  ylim(-35, max(mapping$lat) )+
  labs(title = "Women of Reproductive Age (15-49), by Administrative Region 2", 
       fill  = "Number of women \n of reproductive age (15-49)")+
  theme_void()+
  theme(legend.position = c(0.15, 0.8))+
  coord_equal()

ggsave(file = paste(paste0(output, "plots/", iso, "/WRA_total_adm2.png")), print(plot), dpi = 900)


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
  labs(title = "Youth (10-24), by Administrative Region 2", 
       fill  = "Youth (10-24)")+
  #xlim(10, 35) +
  #ylim(-35, max(mapping$lat) )+
  theme_void()+
  theme(legend.position = c(0.12, 0.8))+
  coord_equal()

ggsave(file = paste(paste0(output, "plots/", iso, "/Youth_total_adm2.png")), print(plot), dpi = 900)

# Zambia projections Central Statistics Office ----------------------------

setwd(output)
dir.create(paste0("plots/",iso))

pop      <- read_xlsx(paste0(output,iso, "_adm0_adm1_pop_2019.xlsx"), sheet=2)

pop$Age <- as.character(pop$Age)


pop.plot <- as.data.frame(dplyr::select(pop, c("ADM1_EN", "ADM1_PCODE", "Age", "Sex", "pop_2019")))

# Prepare data for pop-pyramid
pop.plot$pop_2020 <- ifelse(pop.plot$Sex == "Male", -1*pop.plot$pop_2019, pop.plot$pop_2019)

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

