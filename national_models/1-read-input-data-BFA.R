##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: script to load census, DHS, MICS input data files
##
## Date created: 6 September 2019
## Last updated: 6 September 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

# Last census in Niger was in 2012, the same year a DHS took place. 
# No micro data are available for the 2012 census, however, INS has published single year 
# population estimates. The latest DHS was conducted in 2017, but deemed to low
# quality to be published (https://dhsprogram.com/pubs/pdf/OD76/OD76.pdf). 

# Read in data: INS projections -------------------------------------------

setwd(input)

# Adm0 population growth from WPP -----------------------------------------

## Section: Read in and prepare IPUMS Census microdata
BFA.census.2006 <- "ipumsi_00012.dat"
census.ddi <- read_ipums_ddi("ipumsi_00012.xml")
census.data <- read_ipums_micro(census.ddi,
                                verbose = FALSE)
unique(census.data$DHS_IPUMSI_BF)
## DHS_IPUMSI_CM		DHS-IPUMS-I BFA regions,  1996-2010  [consistent boundaries, GIS]
#01	Boucle de Mouhoun
#02	Cascades
#03	Centre including Ouagadougou
#04	Centre-Est
#05	Centre-Nord
#06	Centre-Ouest
#07	Centre-Sud
#08	Est
#09	Hauts Basins
#10	Nord
#11	Plateau Central
#12	Sahel
#13	Sud-Ouest

## Construct age/sex and age distributions, both by 1-yr and 5-yr
##adm0
male.adm0.census1 <- census.data %>% 
  filter(SEX==1) %>%
  group_by(AGE) %>%  
  dplyr::summarize(sum_age = sum(PERWT))  					    
female.adm0.census1 <- census.data %>%
  filter(SEX==2) %>%
  group_by(AGE) %>%  
  dplyr::summarize(sum_age = sum(PERWT)) 
total.adm0.census1 <- census.data %>% 
  group_by(AGE) %>%  
  dplyr::summarize(sum_age = sum(PERWT))


male.adm0.census5 <- census.data %>% 
  filter(SEX==1) %>%
  group_by(AGE2) %>%  
  dplyr::summarize(sum_age = sum(PERWT))  					    
female.adm0.census5 <- census.data %>%
  filter(SEX==2) %>%
  group_by(AGE2) %>%  
  dplyr::summarize(sum_age = sum(PERWT)) 
total.adm0.census5 <- census.data %>% 
  group_by(AGE2) %>%  
  dplyr::summarize(sum_age = sum(PERWT))


##adm1
male.adm1.census1 <- census.data %>% 
  filter(SEX==1) %>%
  group_by(AGE, DHS_IPUMSI_BF) %>%  
  dplyr::summarize(sum_age = sum(PERWT))
female.adm1.census1 <- census.data %>%
  filter(SEX==2) %>%
  group_by(AGE, DHS_IPUMSI_BF) %>%  
  dplyr::summarize(sum_age = sum(PERWT)) 
total.adm1.census1 <- census.data %>% 
  group_by(AGE, DHS_IPUMSI_BF) %>%  
  dplyr::summarize(sum_age = sum(PERWT))

male.adm1.census5 <- census.data %>% 
  filter(SEX==1) %>%
  group_by(AGE2, DHS_IPUMSI_BF) %>%  
  dplyr::summarize(sum_age = sum(PERWT))  					    
female.adm1.census5 <- census.data %>%
  filter(SEX==2) %>%
  group_by(AGE2, DHS_IPUMSI_BF) %>%  
  dplyr::summarize(sum_age = sum(PERWT)) 
total.adm1.census5 <- census.data %>% 
  group_by(AGE2, DHS_IPUMSI_BF) %>%  
  dplyr::summarize(sum_age = sum(PERWT))  		

## convert age2 to age.cat
## convert region.no to region-name 
male.adm1.census5 <-
  mutate(male.adm1.census5, 
         agegroup = ifelse(AGE2 == 1, "0-4",
                           ifelse(AGE2 == 2, "5-9",
                                  ifelse(AGE2 == 3, "10-14",
                                         ifelse(AGE2 == 4, "15-19",
                                                ifelse(AGE2 ==12, "20-24",
                                                       ifelse(AGE2 == 13, "25-29",
                                                              ifelse(AGE2 == 14, "30-34",
                                                                     ifelse(AGE2 == 15, "35-39",
                                                                            ifelse(AGE2 == 16, "40-44",
                                                                                   ifelse(AGE2 == 17, "45-49",
                                                                                          ifelse(AGE2 == 18, "50-54",
                                                                                                 ifelse(AGE2 == 19, "55-59",
                                                                                                        ifelse(AGE2 == 20, "60-64",
                                                                                                               ifelse(AGE2 == 21, "65-69",
                                                                                                                      ifelse(AGE2 ==22, "70-74",
                                                                                                                             ifelse(AGE2 == 23, "75-79",
                                                                                                                                    ifelse(AGE2 ==24, "80+", NA))))))))))))))))))

male.adm1.census5 <-
  mutate(male.adm1.census5,
         name = ifelse(DHS_IPUMSI_BF == 1, "Boucle de Mouhoun", 
                ifelse(DHS_IPUMSI_BF == 2, "Cascades",
                ifelse(DHS_IPUMSI_BF == 3, "Centre including Ouagadougou",
                ifelse(DHS_IPUMSI_BF == 4, "Centre-Est",
                ifelse(DHS_IPUMSI_BF == 5, "Centre-Nord",
                ifelse(DHS_IPUMSI_BF == 6, "Centre-Ouest",
                ifelse(DHS_IPUMSI_BF == 7, "Centre-Sud",
                ifelse(DHS_IPUMSI_BF == 8, "Est",
                ifelse(DHS_IPUMSI_BF == 9, "Hauts Basins",
                ifelse(DHS_IPUMSI_BF ==10, "Nord",
                ifelse(DHS_IPUMSI_BF ==11, "Plateau Central",
                ifelse(DHS_IPUMSI_BF ==12, "Sahel",
                ifelse(DHS_IPUMSI_BF ==13, "Sud-Ouest", NA))))))))))))))
     
female.adm1.census5 <-
  mutate(female.adm1.census5, 
         agegroup = ifelse(AGE2 == 1, "0-4",
                           ifelse(AGE2 == 2, "5-9",
                                  ifelse(AGE2 == 3, "10-14",
                                         ifelse(AGE2 == 4, "15-19",
                                                ifelse(AGE2 ==12, "20-24",
                                                       ifelse(AGE2 == 13, "25-29",
                                                              ifelse(AGE2 == 14, "30-34",
                                                                     ifelse(AGE2 == 15, "35-39",
                                                                            ifelse(AGE2 == 16, "40-44",
                                                                                   ifelse(AGE2 == 17, "45-49",
                                                                                          ifelse(AGE2 == 18, "50-54",
                                                                                                 ifelse(AGE2 == 19, "55-59",
                                                                                                        ifelse(AGE2 == 20, "60-64",
                                                                                                               ifelse(AGE2 == 21, "65-69",
                                                                                                                      ifelse(AGE2 ==22, "70-74",
                                                                                                                             ifelse(AGE2 == 23, "75-79",
                                                                                                                                    ifelse(AGE2 ==24, "80+", NA))))))))))))))))))

female.adm1.census5 <-
  mutate(female.adm1.census5,
         name = ifelse(DHS_IPUMSI_BF == 1, "Boucle de Mouhoun", 
                       ifelse(DHS_IPUMSI_BF == 2, "Cascades",
                              ifelse(DHS_IPUMSI_BF == 3, "Centre including Ouagadougou",
                                     ifelse(DHS_IPUMSI_BF == 4, "Centre-Est",
                                            ifelse(DHS_IPUMSI_BF == 5, "Centre-Nord",
                                                   ifelse(DHS_IPUMSI_BF == 6, "Centre-Ouest",
                                                          ifelse(DHS_IPUMSI_BF == 7, "Centre-Sud",
                                                                 ifelse(DHS_IPUMSI_BF == 8, "Est",
                                                                        ifelse(DHS_IPUMSI_BF == 9, "Hauts Basins",
                                                                               ifelse(DHS_IPUMSI_BF ==10, "Nord",
                                                                                      ifelse(DHS_IPUMSI_BF ==11, "Plateau Central",
                                                                                             ifelse(DHS_IPUMSI_BF ==12, "Sahel",
                                                                                                    ifelse(DHS_IPUMSI_BF ==13, "Sud-Ouest", NA))))))))))))))

# Add p-codes -------------------------------------------------------------

female.adm1.census5 <- 
  mutate(female.adm1.census5,
         reg_code = ifelse(DHS_IPUMSI_BF == 1, 46, 
                    ifelse(DHS_IPUMSI_BF == 2, 47,
                    ifelse(DHS_IPUMSI_BF == 3, 13,
                    ifelse(DHS_IPUMSI_BF == 4, 48,
                    ifelse(DHS_IPUMSI_BF == 5, 49,
                    ifelse(DHS_IPUMSI_BF == 6, 50,
                    ifelse(DHS_IPUMSI_BF == 7, 51,
                    ifelse(DHS_IPUMSI_BF == 8, 52,
                    ifelse(DHS_IPUMSI_BF == 9, 53,
                    ifelse(DHS_IPUMSI_BF ==10, 54,
                           ifelse(DHS_IPUMSI_BF ==11, 55,
                                  ifelse(DHS_IPUMSI_BF == 12, 56,
                                         ifelse(DHS_IPUMSI_BF ==13, 57,NA))))))))))))))

male.adm1.census5 <- 
  mutate(male.adm1.census5,
         reg_code = ifelse(DHS_IPUMSI_BF == 1, 46, 
                           ifelse(DHS_IPUMSI_BF == 2, 47,
                                  ifelse(DHS_IPUMSI_BF == 3, 13,
                                         ifelse(DHS_IPUMSI_BF == 4, 48,
                                                ifelse(DHS_IPUMSI_BF == 5, 49,
                                                       ifelse(DHS_IPUMSI_BF == 6, 50,
                                                              ifelse(DHS_IPUMSI_BF == 7, 51,
                                                                     ifelse(DHS_IPUMSI_BF == 8, 52,
                                                                            ifelse(DHS_IPUMSI_BF == 9, 53,
                                                                                   ifelse(DHS_IPUMSI_BF ==10, 54,
                                                                                          ifelse(DHS_IPUMSI_BF ==11, 55,
                                                                                                 ifelse(DHS_IPUMSI_BF == 12, 56,
                                                                                                        ifelse(DHS_IPUMSI_BF ==13, 57,NA))))))))))))))

# Save file ---------------------------------------------------------------

female.pop.2005 <- female.adm1.census5 %>%
  filter(!is.na(agegroup))
         
male.pop.2005 <-male.adm1.census5 %>% 
     filter(!is.na(agegroup))

BFApopF <- female.pop.2005[,c(6,5,4,3)]
BFApopM <-   male.pop.2005[,c(6,5,4,3)]

colnames(BFApopF) <- c("reg_code","name","age","2015") # Why 2015?
colnames(BFApopM) <- c("reg_code","name","age","2015") # Why 2015?

write.csv(BFApopF, "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BFApopF.csv", row.names = TRUE)
write.csv(BFApopM, "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BFApopM.csv", row.names = TRUE)

write.table(BFApopF, "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BFApopF.txt", sep = "\t", row.names = FALSE)
write.table(BFApopM, "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BFApopM.txt", sep = "\t", row.names = FALSE)

popF0.file <- BFApopF
popM0.file <- BFApopM

BFApopF <- read_delim("G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BFApopF.txt", delim="\t")
BFApopM <- read_delim("G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/regdata/BFApopM.txt", delim="\t")

