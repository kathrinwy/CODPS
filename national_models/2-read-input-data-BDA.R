# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: load subnational population data, DHS/MICS fertilty data

# Date created: 11 September 2019
# Last updated: 11 September 2019

# Author: Kathrin Weny
# Maintainers: Kathrin Weny, Romesh Silva

# Read in data: INS projections -------------------------------------------

setwd(input)

# Adm0 population growth from WPP -----------------------------------------
url1                    <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F02_POPULATION_GROWTH_RATE.xlsx"
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
growth                  <- read_excel(tf, 1L, skip = 16)

growth                  <- growth %>%
                           dplyr::select(c("Region, subregion, country or area *", "2005-2010", "2010-2015"))%>%
                           filter(`Region, subregion, country or area *` == "Bangladesh")

## Section: Read in and prepare IPUMS Census microdata
BDA.census.2011         <- "ipumsi_00019.dat"
census.ddi              <- read_ipums_ddi("ipumsi_00019.xml")
census.data             <- read_ipums_micro(census.ddi, verbose = FALSE)

# table(census.data$AGE2)
# table(census.data$AGE)

# table(census.data$GEOLEV1)
# 50010	Barisal
# 50020	Chittagong
# 50030	Dhaka
# 50040	Khulna
# 50050	Rajshahi/Rangpur
# 50060	Sylhet


## Construct age/sex and age distributions, both by 1-yr and 5-yr

## Age-Sex distribution, ADM0
asd.adm0 <- census.data$PERWT[1] * table(census.data$AGE,
                       census.data$SEX)

## Age-Sex distribution, ADM1
asd.adm1 <- table(census.data$AGE,
                  census.data$SEX,
                  census.data$GEOLEV1)*census.data$PERWT[1]


###########################################################################################
# Smoothing
f.pop.all <- list()
m.pop.all   <- list()
##Calculate Noumbissi Index for ADM1 level data:

#asd.adm1[1,2,3]
#c("age", "sex", "region")

female.Noumbissi0 <- rep(0,length(asd.adm1[1,1,])) # length across all regions
male.Noumbissi0   <- rep(0,length(asd.adm1[1,1,]))

for (j in 1:length(asd.adm1[1,1,])){
  female.Noumbissi0[j] <- Noumbissi(asd.adm1[,1,j],0:99,ageMin = 20, ageMax = 80, digit=0) #Note highest age is 99
  male.Noumbissi0[j] <- Noumbissi(asd.adm1[,2,j],0:99,ageMin = 20, ageMax = 80, digit=0)   #Note highest age is 99
} 

female.Noumbissi5 <- rep(0,length(asd.adm1[1,2,]))
male.Noumbissi5 <- rep(0,length(asd.adm1[1,1,]))

for (j in 1:length(asd.adm1[1,1,])){
  female.Noumbissi5[j] <- Noumbissi(asd.adm1[,1,j],0:99, ageMin = 20, ageMax = 80,digit=5) #Note highest age is 99
  male.Noumbissi5[j] <- Noumbissi(asd.adm1[,2,j],0:99, ageMin = 20, ageMax = 80,digit=5)   #Note highest age is 99
} 

##Calculate mean relative difference for digit preference between ages 
## ending in -0 and -5  
rel.diff.female.05 <- 100*(female.Noumbissi0 - female.Noumbissi5)/(0.5*(female.Noumbissi0 + female.Noumbissi5))
rel.diff.male.05 <- 100*(male.Noumbissi0 - male.Noumbissi5)/(0.5*(male.Noumbissi0 + male.Noumbissi5))
mean(rel.diff.female.05)
mean(rel.diff.male.05)

##If female age preference for ages ending  0s and 5s differs by moe than 15%,
##then smooth single-year ages for females using Spencer's smoothing technique 
##(for ages 10-89), else use the raw data directly
f.pop <- asd.adm1[,2,]

for(i in 1:length(asd.adm1[1,1,])){
  
  ifelse(mean(rel.diff.female.05) > 15, 
         f.pop <- spencer(asd.adm1[,2,i],0:99),
         f.pop <- asd.adm1[,2,i]
  )
  
  # Replace 0-9 and ages above 90 with asd.adm1 by default
  f.pop[1:10]    <- asd.adm1[1:10,2,i]
  f.pop[90:98]  <- c(asd.adm1[91:99,2,i])
  
  f.pop.all[[i]] <- f.pop
}
##If male age preference for ages ending  0s and 5s differs by moe than 15%,
##then smooth single-year ages for males using Spencer's smoothing technique 
##(for ages 10-89), else use the raw data directly

m.pop <- asd.adm1[,1,]

for(i in 1:length(asd.adm1[1,1,])){
  ifelse(mean(rel.diff.male.05) > 15, 
         m.pop <- spencer(asd.adm1[,1,i],0:99),
         m.pop <- asd.adm1[,1,i]
  )
  
  # Replace 0-9 and ages above 90 with asd.adm1 by default
  m.pop[1:10]    <- asd.adm1[1:10,1,i]
  m.pop[90:98]  <- c(asd.adm1[91:99,1,i])
  
  m.pop.all[[i]] <- m.pop
}

###########################################################################################

# Flat dataset
male.adm1.census1 <- data.frame(matrix(, nrow=0, ncol=3))
names(male.adm1.census1) <- c("AGE2", "PERWT", "GEOLEV1")
i <- 1
for(i in 1:length(asd.adm1[1,1,])){
  data <- as.data.frame(m.pop.all[i]) %>%
    tibble::rownames_to_column("Age")%>%
    mutate(GEOLEV1 = i) 
  
  names(data) <- c("AGE2", "PERWT", "GEOLEV1")
  
  male.adm1.census1 <- rbind(male.adm1.census1, data)
  
}

female.adm1.census1 <- data.frame(matrix(, nrow=0, ncol=3))
names(female.adm1.census1) <- c("AGE2", "PERWT", "GEOLEV1")

for(i in 1:length(asd.adm1[1,1,])){
  data <- as.data.frame(f.pop.all[i]) %>%
    tibble::rownames_to_column("Age")%>%
    mutate(GEOLEV1 = i) 
  
  names(data) <- c("AGE2", "PERWT", "GEOLEV1")
  
  female.adm1.census1 <- rbind(female.adm1.census1, data)
  
}


## convert region.no to region-name 

male.adm1.census1$AGE2 <- as.numeric(male.adm1.census1$AGE2)

male.adm1.census1 <-
  mutate(male.adm1.census1, 
         agegroup = ifelse(0 <= AGE2 & AGE2 <= 4, "0-4",
                           ifelse(5 <= AGE2 & AGE2 <= 9, "5-9",
                                  ifelse(10 <= AGE2 & AGE2 <= 14, "10-14",
                                         ifelse(15 <= AGE2&  AGE2 <= 19, "15-19",
                                                ifelse(20 <= AGE2 & AGE2 <= 24, "20-24",
                                                       ifelse(25 <= AGE2 & AGE2 <= 29, "25-29",
                                                              ifelse(30 <= AGE2 & AGE2 <= 34, "30-34",
                                                                     ifelse(35 <= AGE2 & AGE2 <= 39, "35-39",
                                                                            ifelse(40 <= AGE2 & AGE2 <= 44, "40-44",
                                                                                   ifelse(45 <= AGE2 & AGE2 <= 49, "45-49",
                                                                                          ifelse(50 <= AGE2 & AGE2 <= 54, "50-54",
                                                                                                 ifelse(55 <= AGE2 & AGE2 <= 59, "55-59",
                                                                                                        ifelse(60 <= AGE2 & AGE2 <= 64, "60-64",
                                                                                                               ifelse(65 <= AGE2 & AGE2 <= 69, "65-69",
                                                                                                                      ifelse(70 <= AGE2 & AGE2 <= 74, "70-74",
                                                                                                                             ifelse(75 <= AGE2 & AGE2 <= 79, "75-79",
                                                                                                                                    ifelse(AGE2 >=80, "80+", NA))))))))))))))))))

# Create 5-year age groups

male.adm1.census5 <- male.adm1.census1 %>%
  dplyr::select(c("PERWT", "GEOLEV1", "agegroup")) %>%
  group_by(agegroup, GEOLEV1) %>%  
  dplyr::summarize(sum_age = sum(PERWT, na.rm= T)) 

male.adm1.census5$agegroup <- factor(male.adm1.census5$agegroup, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                                                            "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                                                            "60-64", "65-69", "70-74", "75-79", "80+"))
table(male.adm1.census5$GEOLEV1)


male.adm1.census5 <-
  mutate(male.adm1.census5,
         name = ifelse(GEOLEV1 == 1, "Barisal", 
                       ifelse(GEOLEV1 == 2, "Chittagong",
                              ifelse(GEOLEV1 == 3, "Dhaka",
                                     ifelse(GEOLEV1 == 4, "Khulna",
                                            ifelse(GEOLEV1 == 5, "Rajshahi/Rangpur",
                                                   ifelse(GEOLEV1 == 6, "Sylhet",
                                                          ifelse(GEOLEV1 == 7, "Sylhet", NA))))))))

female.adm1.census1$AGE2 <- as.numeric(female.adm1.census1$AGE2)

female.adm1.census1 <-
  mutate(female.adm1.census1, 
         agegroup = ifelse(0 <= AGE2 & AGE2 <= 4, "0-4",
                           ifelse(5 <= AGE2 & AGE2 <= 9, "5-9",
                                  ifelse(10 <= AGE2 & AGE2 <= 14, "10-14",
                                         ifelse(15 <= AGE2&  AGE2 <= 19, "15-19",
                                                ifelse(20 <= AGE2 & AGE2 <= 24, "20-24",
                                                       ifelse(25 <= AGE2 & AGE2 <= 29, "25-29",
                                                              ifelse(30 <= AGE2 & AGE2 <= 34, "30-34",
                                                                     ifelse(35 <= AGE2 & AGE2 <= 39, "35-39",
                                                                            ifelse(40 <= AGE2 & AGE2 <= 44, "40-44",
                                                                                   ifelse(45 <= AGE2 & AGE2 <= 49, "45-49",
                                                                                          ifelse(50 <= AGE2 & AGE2 <= 54, "50-54",
                                                                                                 ifelse(55 <= AGE2 & AGE2 <= 59, "55-59",
                                                                                                        ifelse(60 <= AGE2 & AGE2 <= 64, "60-64",
                                                                                                               ifelse(65 <= AGE2 & AGE2 <= 69, "65-69",
                                                                                                                      ifelse(70 <= AGE2 & AGE2 <= 74, "70-74",
                                                                                                                             ifelse(75 <= AGE2 & AGE2 <= 79, "75-79",
                                                                                                                                    ifelse(AGE2 >=80, "80+", NA))))))))))))))))))

# Create 5-year age groups
female.adm1.census5 <- female.adm1.census1 %>%
  group_by(agegroup, GEOLEV1) %>%  
  dplyr::summarize(sum_age = sum(PERWT, na.rm= T)) 

female.adm1.census5$agegroup <- factor(female.adm1.census5$agegroup, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                                                                "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                                                                "60-64", "65-69", "70-74", "75-79", "80+"))

female.adm1.census5 <-
  mutate(female.adm1.census5,
         name = ifelse(GEOLEV1 == 1, "Barisal", 
                       ifelse(GEOLEV1 == 2, "Chittagong",
                              ifelse(GEOLEV1 == 3, "Dhaka",
                                     ifelse(GEOLEV1 == 4, "Khulna",
                                            ifelse(GEOLEV1 == 5, "Rajshahi/Rangpur",
                                                   ifelse(GEOLEV1 == 6, "Sylhet",
                                                          ifelse(GEOLEV1 == 7, "Sylhet", NA))))))))

# Add p-codes -------------------------------------------------------------

female.adm1.census5 <- 
  mutate(female.adm1.census5,
         reg_code = ifelse(GEOLEV1 == 1, 10, 
                           ifelse(GEOLEV1 == 2, 20,
                                  ifelse(GEOLEV1 == 3, 30,
                                         ifelse(GEOLEV1 == 4, 40,
                                                ifelse(GEOLEV1 == 5, 50,
                                                       ifelse(GEOLEV1 == 6, 60, NA)))))))

male.adm1.census5 <- 
  mutate(male.adm1.census5,
         reg_code = ifelse(GEOLEV1 == 1, 10, 
                           ifelse(GEOLEV1 == 2, 20,
                                  ifelse(GEOLEV1 == 3, 30,
                                         ifelse(GEOLEV1 == 4, 40,
                                                ifelse(GEOLEV1 == 5, 50,
                                                       ifelse(GEOLEV1 == 6, 60, NA)))))))

# Save file ---------------------------------------------------------------

female.pop.2011 <- female.adm1.census5%>%
  filter(!is.na(agegroup))

male.pop.2011   <- male.adm1.census5 %>%
  filter(!is.na(agegroup))

BGDpopF <- female.pop.2011[,c(5,4,1,3)]
BGDpopM <-   male.pop.2011[,c(5,4,1,3)]

# Project to 2015 ---------------------------------------------------------

# use growth rate for 2010-2015
BGDpopM$'2012' <- BGDpopM$'sum_age'*(as.numeric(growth[1,2])/100 +1)
BGDpopM$'2013' <- BGDpopM$'2012'*(as.numeric(growth[1,2])/100 +1)
BGDpopM$'2014' <- BGDpopM$'2013'*(as.numeric(growth[1,2])/100 +1)
BGDpopM$'2015' <- BGDpopM$'2014'*(as.numeric(growth[1,2])/100 +1)

# use growth rate for 2010-2015
BGDpopF$'2012' <- BGDpopF$'sum_age'*(as.numeric(growth[1,2])/100 +1)
BGDpopF$'2013' <- BGDpopF$'2012'*(as.numeric(growth[1,2])/100 +1)
BGDpopF$'2014' <- BGDpopF$'2013'*(as.numeric(growth[1,2])/100 +1)
BGDpopF$'2015' <- BGDpopF$'2014'*(as.numeric(growth[1,2])/100 +1)

BGDpopF <- BGDpopF[,c(1,2,3,7)]
BGDpopM <- BGDpopM[,c(1,2,3,7)]

# Export ------------------------------------------------------------------

colnames(BGDpopF) <- c("reg_code","name","age","2015") # Why 2015?
colnames(BGDpopM) <- c("reg_code","name","age","2015") # Why 2015?

# Attnetion: Hack, order of factor was not retained when saving --- manually altered order of age groups, thus this code is usually commented out
# write.table(BGDpopF, paste0(output, "regdata/BGDpopF.txt"), sep = "\t", row.names = FALSE)
# write.table(BGDpopM, paste0(output, "regdata/BGDpopM.txt"), sep = "\t", row.names = FALSE)

setwd(output)

# Retrieve e0 trajectories ------------------------------------------------

BGDe0Ftraj <- read.csv(file = "./mye0trajs/F/ascii_trajectories.csv", header=TRUE, sep=",") %>%
  dplyr::select(-Period)
write.csv(BGDe0Ftraj, paste0("./regdata/", "BGDe0Ftraj.csv"), row.names = F)

BGDe0Mtraj <- read.csv(file = "./mye0trajs/M/ascii_trajectories.csv", header=TRUE, sep=",") %>%
  dplyr::select(-Period)
write.csv(BGDe0Mtraj, paste0("./regdata/", "BGDe0Mtraj.csv"), row.names = F)

# TFR input ------------------------------------------------------------------

# bayesTFR projections of the national TFR (result of tfr.predict )

# Find country code
country.code <- iso3166[iso3166$name == country, ][,4]

# Load TFR file
my.regtfr.file.BGD <- "regdata/tfr.txt"
read.delim(my.regtfr.file.BGD , check.names = F)

setwd(code)



