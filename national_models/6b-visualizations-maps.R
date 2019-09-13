##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: maps
##
## Date created: 10 September 2019
## Last updated: 10 September 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

options(scipen = 999)  # disable scientific notation in R

#if a primary sapling unit has only a single observation, R will crash
# option adjust calculates conservative standard errors
# reference: http://faculty.washington.edu/tlumley/survey/example-lonely.html
options(survey.lonely.psu = "adjust")

WRA <- pop.plot %>%
  filter(Age == "15-19" | Age == "20-24" | Age == "25-29	" |Age == "30-34" | Age == "35-39" | Age == "40-44" | Age == "45-49") %>%
  filter(Sex == "female") %>%
  group_by(ADM1_EN) %>%  
  dplyr::summarise(pop = sum(pop_2019))  

Youth <- pop.plot %>%
  filter(Age == "10-14" | Age == "15-19" | Age == "20-24") %>%
  group_by(ADM1_EN) %>%  
  dplyr::summarise(pop = sum(pop_2019))  

setwd("G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/input/BFA-shapfefiles")
geo <- readOGR(".", "sdr_subnational_boundaries") # load shapefile for  DHS data

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

#geo_data_1$id <- as.numeric(geo_data_1$id)-1

geo_data_1$id <- as.character(geo_data_1$id)

data <- WRA 

data_match <- data %>%
  tbl_df()

colnames(data_match) <- c("ID2", "Data") 

data_match <- left_join(data_match, geo_data_1,by=c("ID2"="ID2"))

data_match <- data_match %>%
  group_by(id)

#Second link (new) df2(data_match) and df4

mapping <- left_join(subset_data,data_match)
b <- as.data.frame(unique(mapping$ID))
a <- as.data.frame(subset@data$ID)

# Maps

plot <- ggplot(data=mapping, mapping = aes(x=long, y=lat, group=group, fill = Data))+
  geom_polygon()+
  theme_void()+
  coord_equal()

ggsave(file = paste("WRA_total.jpg"), print(plot))

