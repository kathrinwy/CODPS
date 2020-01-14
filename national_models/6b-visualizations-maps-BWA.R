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

geo <- readOGR(".", "geo1_bw1981_2011") # load shapefile for  DHS data
geo@data$ADMIN_NAME <- as.character(geo@data$ADMIN_NAME)
unique(geo@data$ADMIN_NAME)
geo@data[geo@data$ADMIN_NAME == "Central Tutume, Sowa", "ADMIN_NAME"] <- "Central Tutume Sowa"
geo@data[geo@data$ADMIN_NAME == "Ngwaketse, Ngwaketse West, Ngwaketse Southern, Southern, Jwaneng", "ADMIN_NAME"] <- "Ngwaketse Ngwaketse West Ngwaketse Southern Southern Jwaneng"
geo@data[geo@data$ADMIN_NAME == "Kweneng, Kweneng South, Kweneng North", "ADMIN_NAME"] <- "Kweneng Kweneng South Kweneng North"
geo@data[geo@data$ADMIN_NAME == "Central Serowe/ Palapye", "ADMIN_NAME"] <- "Central Serowe Palapye"
geo@data[geo@data$ADMIN_NAME == "Central Boteti, Orapa", "ADMIN_NAME"] <- "Central Boteti Orapa"
geo@data[geo@data$ADMIN_NAME == "Ngamiland West, Delta", "ADMIN_NAME"] <- "Ngamiland West Delta"
geo@data[geo@data$ADMIN_NAME == "Ghanzi, Central Kgalagadi Game Reserve (CKGR)\r\n", "ADMIN_NAME"] <- "Ghanzi Central Kgalagadi Game Reserve (CKGR)"
geo@data[geo@data$ADMIN_NAME == "Hukunsti (Kgalagadi North)", "ADMIN_NAME"] <- "Hukunsti"

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
  theme(legend.position = c(1, 0.25))+
  coord_equal()

grob.BWA01 <- grobTree(textGrob("Ghanzi Central \n Kgalagadi Game \n Reserve (CKGR)", x=0.2,  y=0.55, hjust=0,
                              gp=gpar(col="black", fontsize=8, fontface="italic"))) 
grob.BWA02 <- grobTree(textGrob("Tshabong \n (Kgalagadi South)", x=0.1,  y=0.22, hjust=0,
                              gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA03 <- grobTree(textGrob("Central Boteti, \n Orapa", x=0.475,  y=0.6, hjust=0,
                              gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA04 <- grobTree(textGrob("Kgatleng", x=0.65,  y=0.32, hjust=0,
                              gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA05 <- grobTree(textGrob("Central Tutume, \n Sowa", x=0.55,  y=0.75, hjust=0,
                              gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA06 <- grobTree(textGrob("Borolong", x=0.55,  y=0.16, hjust=0,
                              gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA07 <- grobTree(textGrob("Gaborone", x=0.62,  y=0.26, hjust=0,
                              gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA08 <- grobTree(textGrob("Central \n Bobonong", x=0.85,  y=0.47, hjust=0,
                              gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA09 <- grobTree(textGrob("Hukunsti", x=0.15,  y=0.35, hjust=0,
                              gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA10 <- grobTree(textGrob("Ngamiland \n East", x=0.4,  y=0.755, hjust=0,
                              gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA11 <- grobTree(textGrob("Central Serowe/ \n Palapye", x=0.58,  y=0.52, hjust=0,
                              gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA12 <- grobTree(textGrob("Chobe", x=0.48,  y=0.9, hjust=0,
                              gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA13 <- grobTree(textGrob("Ngwaketse, \n Ngwaketse West, \n Ngwaketse Southern, \n Jwaneng", x=0.35,  y=0.25, hjust=0,
                              gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA14 <- grobTree(textGrob("Ngamiland West, \n Delta", x=0.17,  y=0.85, hjust=0,
                                gp=gpar(col="black", fontsize=8, fontface="italic"))) # OK
grob.BWA15 <- grobTree(textGrob("South East", x=0.60,  y=0.24, hjust=0,
                                gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA16 <- grobTree(textGrob("Lobaste", x=0.6,  y=0.2, hjust=0,
                                gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA17 <- grobTree(textGrob("Central \n Mahalapye", x=0.6,  y=0.43, hjust=0,
                                gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA18 <- grobTree(textGrob("North East", x=0.75,  y=0.675, hjust=0,
                                gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA19 <- grobTree(textGrob("Francistown", x=0.77,  y=0.63, hjust=0,
                                gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA20 <- grobTree(textGrob("Kweneng \n Kweneng South \n Kweneng North", x=0.45,  y=0.35, hjust=0,
                                gp=gpar(col="black", fontsize=8, fontface="italic")))
grob.BWA21 <- grobTree(textGrob("Selebi Phikwe", x=0.82,  y=0.525, hjust=0,
                                gp=gpar(col="black", fontsize=8, fontface="italic")))

plot.label <- plot +
  annotation_custom(grob.BWA01) +
  annotation_custom(grob.BWA02) +
  annotation_custom(grob.BWA03) +
  annotation_custom(grob.BWA04) +
  annotation_custom(grob.BWA05) +
  annotation_custom(grob.BWA06) +
  annotation_custom(grob.BWA07) +
  annotation_custom(grob.BWA08) +
  annotation_custom(grob.BWA09) +
  annotation_custom(grob.BWA10) +
  annotation_custom(grob.BWA11) +
  annotation_custom(grob.BWA12) +
  annotation_custom(grob.BWA13) +
  annotation_custom(grob.BWA14) +
  annotation_custom(grob.BWA15) +
  annotation_custom(grob.BWA16) +
  annotation_custom(grob.BWA17) +
  annotation_custom(grob.BWA18) +
  annotation_custom(grob.BWA19) +
  annotation_custom(grob.BWA20) +
  annotation_custom(grob.BWA21) 

setwd(output)
ggsave(file = paste0("plots/", iso, "/WRA_total.png"), print(plot.label), dpi = 900)


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
  theme(legend.position = c(1, 0.25))+
  coord_equal()

plot.label <- plot +
  annotation_custom(grob.BWA01) +
  annotation_custom(grob.BWA02) +
  annotation_custom(grob.BWA03) +
  annotation_custom(grob.BWA04) +
  annotation_custom(grob.BWA05) +
  annotation_custom(grob.BWA06) +
  annotation_custom(grob.BWA07) +
  annotation_custom(grob.BWA08) +
  annotation_custom(grob.BWA09) +
  annotation_custom(grob.BWA10) +
  annotation_custom(grob.BWA11) +
  annotation_custom(grob.BWA12) +
  annotation_custom(grob.BWA13) +
  annotation_custom(grob.BWA14) +
  annotation_custom(grob.BWA15) +
  annotation_custom(grob.BWA16) +
  annotation_custom(grob.BWA17) +
  annotation_custom(grob.BWA18) +
  annotation_custom(grob.BWA19) +
  annotation_custom(grob.BWA20) +
  annotation_custom(grob.BWA21)  

setwd(output)
ggsave(file = paste0("plots/", iso, "/Youth_total.png"), print(plot.label), dpi = 900)

setwd(code)


