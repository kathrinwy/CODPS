##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: script to load census, DHS, MICS input data files
##
## Date created: 14 August 2019
## Last updated: 9 January 2020
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny

# Last census in Niger was in 2012, the same year a DHS took place. 
# No micro data are available for the 2012 census, however, INS has published single year 
# population estimates. The latest DHS was conducted in 2017, but deemed to low
# quality to be published (https://dhsprogram.com/pubs/pdf/OD76/OD76.pdf). 

# Read in data: INS projections -------------------------------------------

setwd(input)

# Adm0 population growth from WPP -----------------------------------------
url1                    <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F02_POPULATION_GROWTH_RATE.xlsx"
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
growth                  <- read_excel(tf, 1L, skip = 16)

growth                  <- growth %>%
  dplyr::select(c("Region, subregion, country or area *", "2010-2015"))%>%
  filter(`Region, subregion, country or area *` == country)

# Adm1
adm1.pop      <- read_excel("ins-projections-adm1.xlsx")
adm1.pop      <- gather(adm1.pop, key = "age1", value = "2012", -c("name", "reg_code", "sex"))
adm1.pop$age1 <- as.numeric(adm1.pop$age1)

asd.adm1 <- data.frame(adm1.pop[rep(seq_len(dim(adm1.pop)[1]), adm1.pop$`2012`), 1:4, drop = FALSE], row.names=NULL)

test <- asd.adm1 %>%
  filter(name == "Diffa")%>%
  filter(sex == "male") %>%
  filter(age1 == 0)

table(census.data$AGE)
table(asd.adm1$age1)

table(census.data$SEX)
table(asd.adm1$sex)

table(census.data$DHS_IPUMSI_ZW)
table(asd.adm1$name)

## Age-Sex distribution, ADM1
asd.adm1 <- table(asd.adm1$age1,
                     asd.adm1$sex,
                     asd.adm1$name)



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
  female.Noumbissi0[j] <- Noumbissi(asd.adm1[,2,j],0:80,ageMin = 20, ageMax = 80, digit=0) 
  male.Noumbissi0[j] <- Noumbissi(asd.adm1[,1,j],0:80,ageMin = 20, ageMax = 80, digit=0)   
}  

female.Noumbissi5 <- rep(0,length(asd.adm1[1,2,]))
male.Noumbissi5 <- rep(0,length(asd.adm1[1,1,]))

for (j in 1:length(asd.adm1[1,1,])){
  female.Noumbissi5[j] <- Noumbissi(asd.adm1[,2,j],0:80, ageMin = 20, ageMax = 80,digit=5) 
  male.Noumbissi5[j] <- Noumbissi(asd.adm1[,1,j],0:80, ageMin = 20, ageMax = 80,digit=5)  
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

for(i in 1:length(asd.adm1[1,2,])){
  
  ifelse(mean(rel.diff.female.05) > 15, 
         f.pop <- spencer(asd.adm1[,2,i],0:99),
         f.pop <- asd.adm1[,2,i]
  )
  
  # Replace 0-9 and ages above 90 with asd.adm1 by default
  f.pop[1:10]    <- asd.adm1[1:10,2,i]
 # f.pop[90:98]  <- c(asd.adm1[91:99,2,i])
  
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
 # m.pop[90:98]  <- c(asd.adm1[91:99,1,i])
  
  m.pop.all[[i]] <- m.pop
}

###########################################################################################


# Flat dataset
male.adm1.census1 <- data.frame(matrix(, nrow=0, ncol=3))
names(male.adm1.census1) <- c("AGE2", "PERWT", "name")

for(i in 1:length(asd.adm1[1,1,])){
  data <- as.data.frame(m.pop.all[i]) %>%
    tibble::rownames_to_column("Age")%>%
    mutate(name = i) 
  
  names(data) <- c("AGE2", "PERWT", "name")
  
  male.adm1.census1 <- rbind(male.adm1.census1, data)
  
}

female.adm1.census1 <- data.frame(matrix(, nrow=0, ncol=3))
names(female.adm1.census1) <- c("AGE2", "PERWT", "name")

for(i in 1:length(asd.adm1[1,1,])){
  data <- as.data.frame(f.pop.all[i]) %>%
    tibble::rownames_to_column("Age")%>%
    mutate(name = i) 
  
  names(data) <- c("AGE2", "PERWT", "name")
  
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
  dplyr::select(c("PERWT", "name", "agegroup")) %>%
  group_by(agegroup, name) %>%  
  dplyr::summarize(sum_age = sum(PERWT, na.rm= T)) 

male.adm1.census5$agegroup <- factor(male.adm1.census5$agegroup, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                                                            "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                                                            "60-64", "65-69", "70-74", "75-79", "80+"))

male.adm1.census5 <-
  mutate(male.adm1.census5,
         name = ifelse(name == 1, "Agadez", 
                       ifelse(name == 2, "Diffa",
                              ifelse(name == 3, "Dosso",
                                     ifelse(name == 4, "Maradi",
                                            ifelse(name == 5, "Niamey",
                                                   ifelse(name == 6, "Tahoua",
                                                          ifelse(name == 7, "Tillaberi",
                                                                 ifelse(name == 8, "Zinder", NA)))))))))

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
  group_by(agegroup, name) %>%  
  dplyr::summarize(sum_age = sum(PERWT, na.rm= T)) 

female.adm1.census5$agegroup <- factor(female.adm1.census5$agegroup, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                                                                "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                                                                "60-64", "65-69", "70-74", "75-79", "80+"))

female.adm1.census5 <-
  mutate(female.adm1.census5,
         name = ifelse(name == 1, "Agadez", 
                       ifelse(name == 2, "Diffa",
                              ifelse(name == 3, "Dosso",
                                     ifelse(name == 4, "Maradi",
                                            ifelse(name == 5, "Niamey",
                                                   ifelse(name == 6, "Tahoua",
                                                          ifelse(name == 7, "Tillaberi",
                                                                 ifelse(name == 8, "Zinder", NA)))))))))

# Add p-codes -------------------------------------------------------------

female.adm1.census5 <- 
  mutate(female.adm1.census5,
         reg_code = ifelse(name == "Agadez", 001, 
                           ifelse(name == "Diffa", 002,
                                  ifelse(name == "Dosso", 003,
                                         ifelse(name == "Maradi", 004,
                                                ifelse(name == "Niamey", 008,
                                                       ifelse(name == "Tahoua", 005,
                                                              ifelse(name == "Tillaberi", 006,
                                                                     ifelse(name == "Zinder", 007, NA)))))))))

male.adm1.census5 <- 
  mutate(male.adm1.census5,
         reg_code = ifelse(name == "Agadez", 001, 
                           ifelse(name == "Diffa", 002,
                                  ifelse(name == "Dosso", 003,
                                         ifelse(name == "Maradi", 004,
                                                ifelse(name == "Niamey", 008,
                                                       ifelse(name == "Tahoua", 005,
                                                              ifelse(name == "Tillaberi", 006,
                                                                     ifelse(name == "Zinder", 007, NA)))))))))


# Verificaton of boundary changes beween fertility assumptions and --------
# As the Niger DHS and Census both took place in 2012, boundaries do note change in between them

# Extract data -------------------------------------------------------------

female.pop.2012 <- female.adm1.census5%>%
  filter(!is.na(agegroup))

male.pop.2012   <- male.adm1.census5 %>%
  filter(!is.na(agegroup))

NERpopF <- female.pop.2012[,c(4,2,1,3)]
NERpopM <-   male.pop.2012[,c(4,2,1,3)]

# Undercount adjustment ---------------------------------------------------

undercount <- read.csv.sql("undercount.csv", 
                           sql = "select * from file where country == 'NER'")

NERpopF    <- merge(NERpopF, undercount[, c("name", "undercount")], by = "name")
NERpopF$sum_age <- NERpopF$sum_age*(1+NERpopF$undercount)

NERpopM    <- merge(NERpopM, undercount[, c("name", "undercount")], by = "name")
NERpopM$sum_age <- NERpopM$sum_age*(1+NERpopM$undercount)

# Project to 2015 ---------------------------------------------------------

# use growth rate for 2010-2015
NERpopF$'2013' <- NERpopF$'sum_age'*(as.numeric(growth[1,2])/100 +1)
NERpopF$'2014' <- NERpopF$'2013'*(as.numeric(growth[1,2])/100 +1)
NERpopF$'2015' <- NERpopF$'2014'*(as.numeric(growth[1,2])/100 +1)

# use growth rate for 2010-2015
NERpopM$'2013' <- NERpopM$'sum_age'*(as.numeric(growth[1,2])/100 +1)
NERpopM$'2014' <- NERpopM$'2013'*(as.numeric(growth[1,2])/100 +1)
NERpopM$'2015' <- NERpopM$'2014'*(as.numeric(growth[1,2])/100 +1)

NERpopF <- NERpopF[,c(1,2,3,ncol(NERpopF))]
NERpopM <- NERpopM[,c(1,2,3,ncol(NERpopM))]

# Export ------------------------------------------------------------------

colnames(NERpopF) <- c("reg_code","name","age","2015") 
colnames(NERpopM) <- c("reg_code","name","age","2015") 

NERpopF <- NERpopF[order(NERpopF$reg_code, NERpopF$age),]
NERpopM <- NERpopM[order(NERpopM$reg_code, NERpopM$age),]

write.table(NERpopF, paste0(output, "regdata/NERpopF.txt"), sep = "\t", row.names = FALSE)
write.table(NERpopM, paste0(output, "regdata/NERpopM.txt"), sep = "\t", row.names = FALSE)

setwd(code)

