##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: maps
##
## Date created: 2 December 2019
## Last updated: 2 December 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

options(scipen = 999)  # disable scientific notation in R

#if a primary sapling unit has only a single observation, R will crash
# option adjust calculates conservative standard errors
# reference: http://faculty.washington.edu/tlumley/survey/example-lonely.html

# Prepare data for pop-pyramid
pop.plot$pop_2020 <- ifelse(pop.plot$Sex == "male", -1*pop.plot$pop_2020, pop.plot$pop_2020)

pop.plot$Age <- as.character(pop.plot$Age)
WRA <- pop.plot %>%
  filter(Age == "15-19" | Age == "20-24" | Age == "25-29" |Age == "30-34" | Age == "35-39" | Age == "40-44" | Age == "45-49") %>%
  filter(Sex == "female") %>%
  group_by(ADM1_EN) %>%  
  dplyr::summarise(pop = sum(pop_2020))  

Youth <- pop.plot 

Youth <- Youth%>%
  filter(Age == "10-14" | Age == "15-19" | Age == "20-24") %>%
  group_by(ADM1_EN) %>%  
  dplyr::summarise(pop = sum(pop_2020))  

setwd(paste0(input, iso, "-shapefiles"))

geo <- readOGR(".", "sdr_subnational_boundaries") # load shapefile for  DHS data
geo@data$DHSREGEN <- as.character(geo@data$DHSREGEN )
geo@data[geo@data$DHSREGEN == "Tombouctou", "DHSREGEN"] <- "Toumbouctou"

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

grob.MLI01 <- grobTree(textGrob("Bamako", x=0.22,  y=0.18, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic"))) # OK
grob.MLI02 <- grobTree(textGrob("Mopti", x=0.47,  y=0.33, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.MLI03 <- grobTree(textGrob("Kidal", x=0.73,  y=0.65, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.MLI04 <- grobTree(textGrob("Segou", x=0.35,  y=0.25, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.MLI05 <- grobTree(textGrob("Koulikoro", x=0.29,  y=0.32, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.MLI07 <- grobTree(textGrob("Sikasso", x=0.27,  y=0.1, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.MLI08 <- grobTree(textGrob("Gao", x=0.8,  y=0.46, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.MLI09 <- grobTree(textGrob("Kayes", x=0.1,  y=0.25, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic")))
grob.MLI10 <- grobTree(textGrob("Toumbouctou", x=0.4,  y=0.8, hjust=0,
                                gp=gpar(col="black", fontsize=9, fontface="italic")))


plot.label <- plot +
  annotation_custom(grob.MLI01) +
  annotation_custom(grob.MLI02) +
  annotation_custom(grob.MLI03) +
  annotation_custom(grob.MLI04) +
  annotation_custom(grob.MLI05) +
  annotation_custom(grob.MLI07) +
  annotation_custom(grob.MLI08) +
  annotation_custom(grob.MLI09) +
  annotation_custom(grob.MLI10) 

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
  annotation_custom(grob.MLI01) +
  annotation_custom(grob.MLI02) +
  annotation_custom(grob.MLI03) +
  annotation_custom(grob.MLI04) +
  annotation_custom(grob.MLI05) +
  annotation_custom(grob.MLI07) +
  annotation_custom(grob.MLI08) +
  annotation_custom(grob.MLI09) +
  annotation_custom(grob.MLI10)  

ggsave(file = paste(paste0(output, "plots/", iso, "/Youth_total.png")), print(plot.label), dpi = 900)

setwd(code)