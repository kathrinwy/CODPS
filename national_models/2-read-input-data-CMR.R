# Background --------------------------------------------------------------

# Project: COD-PS Construction
# Script purpose: load subnational population data, DHS/MICS fertilty data

# Date created: 4 December 2019
# Last updated: 9 January 2020

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
                           filter(`Region, subregion, country or area *` == country)

## Section: Read in and prepare IPUMS Census microdata
census                  <- "ipumsi_00023.dat"
census.ddi              <- read_ipums_ddi("ipumsi_00023.xml")
census.data             <- read_ipums_micro(census.ddi, verbose = FALSE)

table(census.data$AGE2)
# Age2 98 is an NA
census.data$AGE2[which(census.data$AGE2 == 98)] <- NA

table(census.data$AGE)
# Age 999 is an NA
census.data$AGE[which(census.data$AGE == 999)] <- NA

table(census.data$GEO1_CM2005)
# 01	Adamoua
# 02	Centre
# 03	Est
# 04	Extreme Nord
# 05	Littoral
# 06	Nord
# 07	Nord Ouest
# 07	Ouest
# 08	Sud
# 09	Sud Ouest


## Construct age/sex and age distributions, both by 1-yr and 5-yr

## Age-Sex distribution, ADM0
asd.adm0 <- census.data$PERWT[1] * table(census.data$AGE,
                       census.data$SEX)

## Age-Sex distribution, ADM1
asd.adm1 <- table(census.data$AGE,
                  census.data$SEX,
                  census.data$GEO1_CM2005)*census.data$PERWT[1]


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
  female.Noumbissi0[j] <- Noumbissi(asd.adm1[,1,j],0:98,ageMin = 20, ageMax = 80, digit=0) #Note highest age is 98
  male.Noumbissi0[j] <- Noumbissi(asd.adm1[,2,j],0:98,ageMin = 20, ageMax = 80, digit=0)   #Note highest age is 98
} 

female.Noumbissi5 <- rep(0,length(asd.adm1[1,2,]))
male.Noumbissi5 <- rep(0,length(asd.adm1[1,1,]))

for (j in 1:length(asd.adm1[1,1,])){
  female.Noumbissi5[j] <- Noumbissi(asd.adm1[,1,j],0:98, ageMin = 20, ageMax = 80,digit=5) #Note highest age is 98
  male.Noumbissi5[j] <- Noumbissi(asd.adm1[,2,j],0:98, ageMin = 20, ageMax = 80,digit=5)   #Note highest age is 98
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
         f.pop <- spencer(asd.adm1[,2,i],0:98),
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
names(male.adm1.census1) <- c("AGE2", "PERWT", "GEO1_CM2005")
i <- 1
for(i in 1:length(asd.adm1[1,1,])){
  data <- as.data.frame(m.pop.all[i]) %>%
    tibble::rownames_to_column("Age")%>%
    mutate(GEO1_CM2005 = i) 

  names(data) <- c("AGE2", "PERWT", "GEO1_CM2005")
  
  male.adm1.census1 <- rbind(male.adm1.census1, data)
  
}

female.adm1.census1 <- data.frame(matrix(, nrow=0, ncol=3))
names(female.adm1.census1) <- c("AGE2", "PERWT", "GEO1_CM2005")

for(i in 1:length(asd.adm1[1,1,])){
  data <- as.data.frame(f.pop.all[i]) %>%
    tibble::rownames_to_column("Age")%>%
    mutate(GEO1_CM2005 = i) 
  
  names(data) <- c("AGE2", "PERWT", "GEO1_CM2005")

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
  dplyr::select(c("PERWT", "GEO1_CM2005", "agegroup")) %>%
  group_by(agegroup, GEO1_CM2005) %>%  
  dplyr::summarize(sum_age = sum(PERWT, na.rm= T)) 

male.adm1.census5$agegroup <- factor(male.adm1.census5$agegroup, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                                                            "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                                                            "60-64", "65-69", "70-74", "75-79", "80+"))

male.adm1.census5 <-
  mutate(male.adm1.census5,
         name = ifelse(GEO1_CM2005 == 1, "Adamoua", 
                ifelse(GEO1_CM2005 == 2, "Centre",
                ifelse(GEO1_CM2005 == 3, "Est",
                ifelse(GEO1_CM2005 == 4, "Extreme Nord",
                ifelse(GEO1_CM2005 == 5, "Littoral",
                ifelse(GEO1_CM2005 == 6, "Nord",
                ifelse(GEO1_CM2005 == 7, "Nord Ouest",
                ifelse(GEO1_CM2005 == 8, "Ouest",
                ifelse(GEO1_CM2005 == 9, "Sud", 
                ifelse(GEO1_CM2005 == 10, "Sud Ouest", NA)))))))))))

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
  group_by(agegroup, GEO1_CM2005) %>%  
  dplyr::summarize(sum_age = sum(PERWT, na.rm= T)) 

female.adm1.census5$agegroup <- factor(female.adm1.census5$agegroup, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                                                                "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                                                                "60-64", "65-69", "70-74", "75-79", "80+"))

female.adm1.census5 <-
  mutate(female.adm1.census5,
         name = ifelse(GEO1_CM2005 == 1, "Adamoua", 
                       ifelse(GEO1_CM2005 == 2, "Centre",
                              ifelse(GEO1_CM2005 == 3, "Est",
                                     ifelse(GEO1_CM2005 == 4, "Extreme Nord",
                                            ifelse(GEO1_CM2005 == 5, "Littoral",
                                                   ifelse(GEO1_CM2005 == 6, "Nord",
                                                          ifelse(GEO1_CM2005 == 7, "Nord Ouest",
                                                                 ifelse(GEO1_CM2005 == 8, "Ouest",
                                                                        ifelse(GEO1_CM2005 == 9, "Sud", 
                                                                               ifelse(GEO1_CM2005 == 10, "Sud Ouest", NA)))))))))))

# Add p-codes -------------------------------------------------------------

female.adm1.census5 <- 
  mutate(female.adm1.census5,
         reg_code = ifelse(GEO1_CM2005 == 1, 101, 
                           ifelse(GEO1_CM2005 == 2, 102,
                                  ifelse(GEO1_CM2005 == 3, 103,
                                         ifelse(GEO1_CM2005 == 4, 104,
                                                ifelse(GEO1_CM2005 == 5, 105,
                                                       ifelse(GEO1_CM2005 == 6, 106,
                                                              ifelse(GEO1_CM2005 == 7, 107,
                                                                     ifelse(GEO1_CM2005 == 8, 108,
                                                                            ifelse(GEO1_CM2005 == 9, 109, 
                                                                                   ifelse(GEO1_CM2005 == 10, 110, NA)))))))))))

male.adm1.census5 <- 
  mutate(male.adm1.census5,
         reg_code = ifelse(GEO1_CM2005 == 1, 101, 
                           ifelse(GEO1_CM2005 == 2, 102,
                                  ifelse(GEO1_CM2005 == 3, 103,
                                         ifelse(GEO1_CM2005 == 4, 104,
                                                ifelse(GEO1_CM2005 == 5, 105,
                                                       ifelse(GEO1_CM2005 == 6, 106,
                                                              ifelse(GEO1_CM2005 == 7, 107,
                                                                     ifelse(GEO1_CM2005 == 8, 108,
                                                                            ifelse(GEO1_CM2005 == 9, 109, 
                                                                                   ifelse(GEO1_CM2005 == 10, 110, NA)))))))))))

# Extract data -------------------------------------------------------------

female.pop.2005 <- female.adm1.census5%>%
  filter(!is.na(agegroup))

male.pop.2005   <- male.adm1.census5 %>%
  filter(!is.na(agegroup))

CMRpopF <- female.pop.2005[,c(5,4,1,3)]
CMRpopM <-   male.pop.2005[,c(5,4,1,3)]

# Undercount adjustment ---------------------------------------------------

undercount <- read.csv.sql("undercount.csv", 
                           sql = "select * from file where country == 'CMR'")

CMRpopF    <- merge(CMRpopF, undercount[, c("name", "undercount")], by = "name")
CMRpopF$sum_age <- CMRpopF$sum_age*(1+CMRpopF$undercount)

CMRpopM    <- merge(CMRpopM, undercount[, c("name", "undercount")], by = "name")
CMRpopM$sum_age <- CMRpopM$sum_age*(1+CMRpopM$undercount)

# Project to 2015 ---------------------------------------------------------

# use growth rate for 2005-2010
CMRpopF$'2006' <- CMRpopF$'sum_age'*(as.numeric(growth[1,2])/100 +1)
CMRpopF$'2007' <- CMRpopF$'2006'*(as.numeric(growth[1,2])/100 +1)
CMRpopF$'2008' <- CMRpopF$'2007'*(as.numeric(growth[1,2])/100 +1)
CMRpopF$'2009' <- CMRpopF$'2008'*(as.numeric(growth[1,2])/100 +1)
CMRpopF$'2010' <- CMRpopF$'2009'*(as.numeric(growth[1,2])/100 +1)

# use growth rate for 2010-2015
CMRpopF$'2011' <- CMRpopF$'2010'*(as.numeric(growth[1,3])/100 +1)
CMRpopF$'2012' <- CMRpopF$'2011'*(as.numeric(growth[1,3])/100 +1)
CMRpopF$'2013' <- CMRpopF$'2012'*(as.numeric(growth[1,3])/100 +1)
CMRpopF$'2014' <- CMRpopF$'2013'*(as.numeric(growth[1,3])/100 +1)
CMRpopF$'2015' <- CMRpopF$'2014'*(as.numeric(growth[1,3])/100 +1)

# use growth rate Mor 2005-2010
CMRpopM$'2006' <- CMRpopM$'sum_age'*(as.numeric(growth[1,2])/100 +1)
CMRpopM$'2007' <- CMRpopM$'2006'*(as.numeric(growth[1,2])/100 +1)
CMRpopM$'2008' <- CMRpopM$'2007'*(as.numeric(growth[1,2])/100 +1)
CMRpopM$'2009' <- CMRpopM$'2008'*(as.numeric(growth[1,2])/100 +1)
CMRpopM$'2010' <- CMRpopM$'2009'*(as.numeric(growth[1,2])/100 +1)

# use growth rate Mor 2010-2015
CMRpopM$'2011' <- CMRpopM$'2010'*(as.numeric(growth[1,3])/100 +1)
CMRpopM$'2012' <- CMRpopM$'2011'*(as.numeric(growth[1,3])/100 +1)
CMRpopM$'2013' <- CMRpopM$'2012'*(as.numeric(growth[1,3])/100 +1)
CMRpopM$'2014' <- CMRpopM$'2013'*(as.numeric(growth[1,3])/100 +1)
CMRpopM$'2015' <- CMRpopM$'2014'*(as.numeric(growth[1,3])/100 +1)

CMRpopF <- CMRpopF[,c(1,2,3,14)]
CMRpopM <- CMRpopM[,c(1,2,3,14)]

# Export ------------------------------------------------------------------

colnames(CMRpopF) <- c("reg_code","name","age","2015") # Why 2015?
colnames(CMRpopM) <- c("reg_code","name","age","2015") # Why 2015?

ZWEpopF <- ZWEpopF[order(ZWEpopF$reg_code, ZWEpopF$age),]
ZWEpopM <- ZWEpopM[order(ZWEpopM$reg_code, ZWEpopM$age),]

write.table(CMRpopF, paste0(output, "regdata/CMRpopF.txt"), sep = "\t", row.names = FALSE)
write.table(CMRpopM, paste0(output, "regdata/CMRpopM.txt"), sep = "\t", row.names = FALSE)

setwd(code)




