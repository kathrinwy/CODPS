##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: script to load census, DHS, MICS input data files
##
## Date created: 14 August 2019
## Last updated: 14 August 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

# Last census in Niger was in 


# Read in data: INS projections -------------------------------------------

# Adm0

# Adm1

# Adm2

# Adm3



##================================================================
## Section: Read in and prepare IPUMS Census microdata
CAM.census.2005 <- "input/ipumsi_00030.dat"
census.ddi <- read_ipums_ddi("input/ipumsi_00030.xml")
census.data <- read_ipums_micro(census.ddi,
                                verbose = FALSE)
## DHS_IPUMSI_CM		DHS-IPUMS-I Cameroon regions, 1976-2011 [consistent boundaries, GIS]
# 02		Centre, Sud
# 03		Est
# 04		Nord, Adamoua , Extrème Nord
# 05		Littoral, Douala
# 06		Nord Ouest
# 07		Ouest
# 10		Sud Ouest

## Construct age/sex and age distributions, both by 1-yr and 5-yr
##adm0
male.adm0.census1 <- census.data %>% 
  filter(SEX==1) %>%
  group_by(AGE) %>%  
  summarise(sum_age = sum(PERWT))  					    
female.adm0.census1 <- census.data %>%
  filter(SEX==2) %>%
  group_by(AGE) %>%  
  summarise(sum_age = sum(PERWT)) 
total.adm0.census1 <- census.data %>% 
  group_by(AGE) %>%  
  summarise(sum_age = sum(PERWT))
male.adm0.census5 <- census.data %>% 
  filter(SEX==1) %>%
  group_by(AGE2) %>%  
  summarise(sum_age = sum(PERWT))  					    
female.adm0.census5 <- census.data %>%
  filter(SEX==2) %>%
  group_by(AGE2) %>%  
  summarise(sum_age = sum(PERWT)) 
total.adm0.census5 <- census.data %>% 
  group_by(AGE2) %>%  
  summarise(sum_age = sum(PERWT))
##adm1
male.adm1.census1 <- census.data %>% 
  filter(SEX==1) %>%
  group_by(AGE, DHS_IPUMSI_CM) %>%  
  summarise(sum_age = sum(PERWT))
female.adm1.census1 <- census.data %>%
  filter(SEX==2) %>%
  group_by(AGE, DHS_IPUMSI_CM) %>%  
  summarise(sum_age = sum(PERWT)) 
total.adm1.census1 <- census.data %>% 
  group_by(AGE, DHS_IPUMSI_CM) %>%  
  summarise(sum_age = sum(PERWT))
male.adm1.census5 <- census.data %>% 
  filter(SEX==1) %>%
  group_by(AGE2, DHS_IPUMSI_CM) %>%  
  summarise(sum_age = sum(PERWT))  					    
female.adm1.census5 <- census.data %>%
  filter(SEX==2) %>%
  group_by(AGE2, DHS_IPUMSI_CM) %>%  
  summarise(sum_age = sum(PERWT)) 
total.adm1.census5 <- census.data %>% 
  group_by(AGE2, DHS_IPUMSI_CM) %>%  
  summarise(sum_age = sum(PERWT))  		

## convert age2 to age.cat
## convert region.no to region-name 
male.adm1.census5 <-
  mutate(male.adm1.census5, agegroup = ifelse(AGE2 == 1, "0-4",
                                              ifelse(
                                                AGE2 == 2, "5-9",
                                                ifelse(AGE2 == 3, "10-14",
                                                       ifelse(
                                                         AGE2 == 4, "15-19",
                                                         ifelse(AGE2 == 8, "20-24",
                                                                ifelse(
                                                                  AGE2 == 9, "25-29",
                                                                  ifelse(AGE2 == 10, "30-34",
                                                                         ifelse(
                                                                           AGE2 == 11, "35-39",
                                                                           ifelse(AGE2 == 12, "40-44",
                                                                                  ifelse(
                                                                                    AGE2 == 13, "45-49",
                                                                                    ifelse(AGE2 == 14, "50-54",
                                                                                           ifelse(
                                                                                             AGE2 == 15, "55-59",
                                                                                             ifelse(AGE2 == 16, "60-64",
                                                                                                    ifelse(
                                                                                                      AGE2 == 17, "65-69",
                                                                                                      ifelse(AGE2 ==
                                                                                                               18, "70-74",
                                                                                                             ifelse(
                                                                                                               AGE2 == 19, "75-79",
                                                                                                               ifelse(AGE2 ==
                                                                                                                        20, "80+")
                                                                                                             ))
                                                                                                    ))
                                                                                           ))
                                                                                  ))
                                                                         ))
                                                                ))
                                                       ))
                                              )))
male.adm1.census5 <-
  mutate(male.adm1.census5,
         name = ifelse(
           DHS_IPUMSI_CM == 2,
           "Centre, Sud",
           ifelse(
             DHS_IPUMSI_CM == 3,
             "Est",
             ifelse(
               DHS_IPUMSI_CM == 4,
               "Nord, Adamoua, Extreme-Nord",
               ifelse(
                 DHS_IPUMSI_CM == 5,
                 "Littoral, Douala",
                 ifelse(
                   DHS_IPUMSI_CM == 6,
                   "Nord Ouest",
                   ifelse(
                     DHS_IPUMSI_CM == 7,
                     "Ouest",
                     ifelse(DHS_IPUMSI_CM == 10, "Sud Ouest", "NA"))))))))

female.adm1.census5 <-
  mutate(female.adm1.census5, 
         agegroup = ifelse(AGE2 == 1, "0-4",
                           ifelse(AGE2 == 2, "5-9",
                                  ifelse(AGE2 == 3, "10-14",
                                         ifelse(AGE2 == 4, "15-19",
                                                ifelse(AGE2 == 8, "20-24",
                                                       ifelse(AGE2 == 9, "25-29",
                                                              ifelse(AGE2 == 10, "30-34",
                                                                     ifelse(AGE2 == 11, "35-39",
                                                                            ifelse(AGE2 == 12, "40-44",
                                                                                   ifelse(AGE2 == 13, "45-49",
                                                                                          ifelse(AGE2 == 14, "50-54",
                                                                                                 ifelse(AGE2 == 15, "55-59",
                                                                                                        ifelse(AGE2 == 16, "60-64",
                                                                                                               ifelse(AGE2 == 17, "65-69",
                                                                                                                      ifelse(AGE2 ==18, "70-74",
                                                                                                                             ifelse(AGE2 == 19, "75-79",
                                                                                                                                    ifelse(AGE2 ==20, "80+"))))))))))))))))))

female.adm1.census5 <-  mutate(female.adm1.census5, name = ifelse(DHS_IPUMSI_CM == 2, "Centre, Sud",
                                                                  ifelse(DHS_IPUMSI_CM == 3, "Est",
                                                                         ifelse(DHS_IPUMSI_CM == 4, "Nord, Adamoua, Extreme-Nord",
                                                                                ifelse(DHS_IPUMSI_CM == 5, "Littoral, Douala",
                                                                                       ifelse(DHS_IPUMSI_CM == 6, "Nord Ouest",
                                                                                              ifelse(DHS_IPUMSI_CM == 7, "Ouest",
                                                                                                     ifelse(DHS_IPUMSI_CM == 10, "Sud Ouest", "NA"))))))))




female.adm1.census5 <- 
  mutate(female.adm1.census5,
         reg_code = ifelse(DHS_IPUMSI_CM == 2, 102,
                           ifelse(DHS_IPUMSI_CM == 3, 103,
                                  ifelse(DHS_IPUMSI_CM == 4, 106,
                                         ifelse(DHS_IPUMSI_CM == 5, 105,
                                                ifelse(DHS_IPUMSI_CM == 6, 107,
                                                       ifelse(DHS_IPUMSI_CM==7,108,
                                                              ifelse(DHS_IPUMSI_CM==10,110,0))))))))

male.adm1.census5 <- 
  mutate(male.adm1.census5,
         reg_code = ifelse(DHS_IPUMSI_CM == 2, 102,
                           ifelse(DHS_IPUMSI_CM == 3, 103,
                                  ifelse(DHS_IPUMSI_CM == 4, 106,
                                         ifelse(DHS_IPUMSI_CM == 5, 105,
                                                ifelse(DHS_IPUMSI_CM == 6, 107,
                                                       ifelse(DHS_IPUMSI_CM==7,108,
                                                              ifelse(DHS_IPUMSI_CM==10,110,0))))))))
################################################################
### Edit baseline subnational populaiton data during last census 
## Create sud dummmy matrix  ###################################
##females
library(stringr)
sud.female <- female.adm1.census5[female.adm1.census5$DHS_IPUMSI_CM==2,]
sud.female$y2005 <- 0 
sud.female$y2005 <- .17 * as.numeric(sud.female$sum_age)
sud.female <- sud.female %>% mutate(`name` = str_replace(`name`, "Centre, Sud", "Sud"))
##males 
sud.male <- male.adm1.census5[male.adm1.census5$DHS_IPUMSI_CM==2,]
sud.male$y2005 <- 0 
sud.male$y2005 <- .17 * as.numeric(sud.male$sum_age)
sud.male <- sud.male %>%  mutate(`name` = str_replace(`name`, "Centre, Sud", "Sud"))
## Create Admoua dummy matrix  ################################### 
##females
adamoua.female <- female.adm1.census5[female.adm1.census5$DHS_IPUMSI_CM==4,]
adamoua.female$y2005 <- 0 
adamoua.female$y2005 <- .16 * as.numeric(adamoua.female$sum_age)
adamoua.female <- adamoua.female %>% mutate(`name` = str_replace(`name`, "Nord, Adamoua, Extreme-Nord", "Adamoua"))
##males
adamoua.male <- male.adm1.census5[male.adm1.census5$DHS_IPUMSI_CM==4,]
adamoua.male$y2005 <- 0 
adamoua.male$y2005 <- .16 * as.numeric(adamoua.male$sum_age)
adamoua.male <- adamoua.male %>% mutate(`name` = str_replace(`name`, "Nord, Adamoua, Extreme-Nord", "Adamoua"))
## Create Extreme-Nord dummy matrix  #############################
#females
extnord.female <- male.adm1.census5[female.adm1.census5$DHS_IPUMSI_CM==4,]
extnord.female$y2005 <- 0 
extnord.female$y2005 <- .54 * as.numeric(extnord.female$sum_age)
extnord.female <- extnord.female %>% mutate(`name` = str_replace(`name`, "Nord, Adamoua, Extreme-Nord", "Extreme-Nord"))
#males
extnord.male <- male.adm1.census5[male.adm1.census5$DHS_IPUMSI_CM==4,]
extnord.male$y2005 <- 0
extnord.male$y2005 <- .54 * as.numeric(extnord.male$sum_age)
extnord.male <- extnord.male %>% mutate(`name` = str_replace(`name`, "Nord, Adamoua, Extreme-Nord", "Extreme-Nord"))

##Edit subnat pop data matrix#####################################
##Rename ADM1 entities
female.pop.2005 <- female.adm1.census5
female.pop.2005 <- female.pop.2005 %>% mutate(`name` = str_replace(`name`,"Centre, Sud", "Centre"))
female.pop.2005 <- female.pop.2005 %>% mutate(`name` = str_replace(`name`,"Nord, Adamoua, Extreme-Nord", "Nord"))
female.pop.2005 <- female.pop.2005 %>% mutate(`name` = str_replace(`name`,"Littoral, Douala", "Littoral"))

male.pop.2005 <- male.adm1.census5
male.pop.2005 <- male.pop.2005 %>% mutate(`name` = str_replace(`name`,"Centre, Sud", "Centre"))
male.pop.2005 <- male.pop.2005 %>% mutate(`name` = str_replace(`name`, "Nord, Adamoua, Extreme-Nord", "Nord"))
male.pop.2005 <- male.pop.2005 %>% mutate(`name` = str_replace(`name`,"Littoral, Douala", "Littoral"))  
##Recalc pop size per unmerged areas
female.pop.2005$y2005 <- 0  
female.pop.2005$y2005[female.pop.2005$name == "Nord"] <- .3 * female.pop.2005$sum_age[female.pop.2005$name == "Nord"]
female.pop.2005$y2005[female.pop.2005$name == "Centre"] <- .83 * female.pop.2005$sum_age[female.pop.2005$name == "Centre"]
female.pop.2005$y2005[female.pop.2005$name == "Est" |
                        female.pop.2005$name == "Littoral" | 
                        female.pop.2005$name == "Nord Ouest" |
                        female.pop.2005$name == "Ouest" |
                        female.pop.2005$name ==  "Sud Ouest"] <- female.pop.2005$sum_age[female.pop.2005$name == "Est" |
                                                                                           female.pop.2005$name == "Littoral" | 
                                                                                           female.pop.2005$name == "Nord Ouest" |
                                                                                           female.pop.2005$name == "Ouest" |
                                                                                           female.pop.2005$name ==  "Sud Ouest"]
male.pop.2005$y2005 <- 0  
male.pop.2005$y2005[male.pop.2005$name == "Nord"] <- .3 * male.pop.2005$sum_age[male.pop.2005$name == "Nord"]
male.pop.2005$y2005[male.pop.2005$name == "Centre"] <- .83 * male.pop.2005$sum_age[male.pop.2005$name == "Centre"]
male.pop.2005$y2005[male.pop.2005$name == "Est" |
                      male.pop.2005$name == "Littoral" | 
                      male.pop.2005$name == "Nord Ouest" |
                      male.pop.2005$name == "Ouest" |
                      male.pop.2005$name ==  "Sud Ouest"] <- male.pop.2005$sum_age[male.pop.2005$name == "Est" |
                                                                                     male.pop.2005$name == "Littoral" | 
                                                                                     male.pop.2005$name == "Nord Ouest" |
                                                                                     male.pop.2005$name == "Ouest" |
                                                                                     male.pop.2005$name ==  "Sud Ouest"]
##Add in ADM1 regions of Sud, Adamoua and Extreme-Nord
female.pop.2005 <- rbind(female.pop.2005, 
                         sud.female,
                         adamoua.female,
                         extnord.female)
male.pop.2005 <- rbind(male.pop.2005, 
                       sud.male,
                       adamoua.male,
                       extnord.male)

CAMpopF <- female.pop.2005[,c(6,5,4,7)]
CAMpopM <- male.pop.2005[,c(6,5,4,7)]
colnames(CAMpopF) <- 
  colnames(CAMpopM) <- c("reg_code","name","age","2015")


write.csv(CAMpopF, 
          "~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop/regdata/CAMpopF.csv", 
          row.names = TRUE)
write.csv(CAMpopM, 
          "~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop/regdata/CAMpopM.csv", 
          row.names = TRUE)
popF0.file <- CAMpopF
popM0.file <- CAMpopM

## Fixing dupliczate row names, so we can successfully run the subnational population projections

#delete two lines below!
#CAMpopF <- read.csv("~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop/regdata/CAMpopF.csv")
#CAMpopM <- read.csv("~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop/regdata/CAMpopM.csv")

CAMpopF <- read_delim("~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop/regdata/CAMpopF.txt", delim="\t")
CAMpopM <- read_delim("~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop/regdata/CAMpopM.txt", delim="\t")


CAMpopF$reg_code[CAMpopF$name == "Adamoua"] <- 101
CAMpopF$reg_code[CAMpopF$name == "Centre"] <- 102
CAMpopF$reg_code[CAMpopF$name == "Est"] <- 103
CAMpopF$reg_code[CAMpopF$name == "Extreme-Nord"] <- 104
CAMpopF$reg_code[CAMpopF$name == "Littoral"] <- 105
CAMpopF$reg_code[CAMpopF$name == "Nord"] <- 106
CAMpopF$reg_code[CAMpopF$name == "Nord Ouest"] <- 107
CAMpopF$reg_code[CAMpopF$name == "Ouest"] <- 108
CAMpopF$reg_code[CAMpopF$name == "Sud"] <- 109
CAMpopF$reg_code[CAMpopF$name == "Sud Ouest"] <- 110

CAMpopM$reg_code[CAMpopM$name == "Adamoua"] <- 101
CAMpopM$reg_code[CAMpopM$name == "Centre"] <- 102
CAMpopM$reg_code[CAMpopM$name == "Est"] <- 103
CAMpopM$reg_code[CAMpopM$name == "Extreme-Nord"] <- 104
CAMpopM$reg_code[CAMpopM$name == "Littoral"] <- 105
CAMpopM$reg_code[CAMpopM$name == "Nord"] <- 106
CAMpopM$reg_code[CAMpopM$name == "Nord Ouest"] <- 107
CAMpopM$reg_code[CAMpopM$name == "Ouest"] <- 108
CAMpopM$reg_code[CAMpopM$name == "Sud"] <- 109
CAMpopM$reg_code[CAMpopM$name == "Sud Ouest"] <- 110

write.csv(CAMpopF, 
          "~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop/regdata/CAMpopF.csv", 
          row.names = TRUE)
write.csv(CAMpopM, 
          "~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop/regdata/CAMpopM.csv", 
          row.names = TRUE)
popF0.file <- CAMpopF
popM0.file <- CAMpopM



