##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: script to load census, DHS, MICS input data files
##
## Date created: 14 August 2019
## Last updated: 16 August 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

# Last census in Niger was in 2012, the same year a DHS took place. 
# No micro data are available for the 2012 census, however, INS has published single year 
# population estimates. The latest DHS was conducted in 2017, but deemed to low
# quality to be published (https://dhsprogram.com/pubs/pdf/OD76/OD76.pdf). 

# Read in data: INS projections -------------------------------------------

setwd(NER.input)

# Adm0 population growth from WPP -----------------------------------------

url1       <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F02_POPULATION_GROWTH_RATE.xlsx"
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
growth     <- read_excel(tf, 1L, skip = 16)

growth.NER <- growth %>%
  select(c("Region, subregion, country or area *", "2010-2015"))%>%
  filter(`Region, subregion, country or area *` == "Niger")

# Adm0

adm0.pop     <- read_excel("ins-projections-adm0.xlsx")
adm0.pop     <- gather(adm0.pop, key = "sex", value = "2012", -"age1")

# Convert into 5-year age11 groups
adm0.pop <- adm0.pop %>%
  mutate(age = ifelse(0  <= age1 & age1 < 5, "0-4",
                    ifelse(5  <= age1 & age1 < 10, "5-9",
                    ifelse(10 <= age1 & age1 < 15, "10-14",
                    ifelse(15 <= age1 & age1 < 20, "15-19",
                    ifelse(20 <= age1 & age1 < 25, "20-24",
                    ifelse(25 <= age1 & age1 < 30, "25-29",
                    ifelse(30 <= age1 & age1 < 35, "30-34",
                    ifelse(35 <= age1 & age1 < 40, "35-39",
                    ifelse(40 <= age1 & age1 < 45, "40-44",
                    ifelse(45 <= age1 & age1 < 50, "45-49",
                    ifelse(50 <= age1 & age1 < 55, "50-54",
                    ifelse(55 <= age1 & age1 < 60, "55-59",
                    ifelse(60 <= age1 & age1 < 65, "60-64",
                    ifelse(65 <= age1 & age1 < 70, "65-69",
                    ifelse(70 <= age1 & age1 < 78, "70-74",
                    ifelse(75 <= age1 & age1 < 80, "75-79",
                    ifelse(80 <= age1            , "80+", NA))))))))))))))))))

adm0.pop$'2013' <- adm0.pop$'2012'*(as.numeric(growth.NER[1,2])/100 +1)
adm0.pop$'2014' <- adm0.pop$'2013'*(as.numeric(growth.NER[1,2])/100 +1)
adm0.pop$'2015' <- adm0.pop$'2014'*(as.numeric(growth.NER[1,2])/100 +1)

NERpopF.adm0  <- filter(adm0.pop, sex == "female")
NERpopM.adm0  <- filter(adm0.pop, sex == "male")

# Adm1

adm1.pop      <- read_excel("ins-projections-adm1.xlsx")
adm1.pop      <- gather(adm1.pop, key = "age1", value = "2012", -c("name", "reg_code", "sex"))
adm1.pop$age1 <- as.numeric(adm1.pop$age1)

# Convert into 5-year age11 groups
adm1.pop <- adm1.pop %>%
  mutate(age = ifelse(0  <= age1 & age1 < 5, "0-4",
                    ifelse(5  <= age1 & age1 < 10, "5-9",
                    ifelse(10 <= age1 & age1 < 15, "10-14",
                    ifelse(15 <= age1 & age1 < 20, "15-19",
                    ifelse(20 <= age1 & age1 < 25, "20-24",
                    ifelse(25 <= age1 & age1 < 30, "25-29",
                    ifelse(30 <= age1 & age1 < 35, "30-34",
                    ifelse(35 <= age1 & age1 < 40, "35-39",
                    ifelse(40 <= age1 & age1 < 45, "40-44",
                    ifelse(45 <= age1 & age1 < 50, "45-49",
                    ifelse(50 <= age1 & age1 < 55, "50-54",
                    ifelse(55 <= age1 & age1 < 60, "55-59",
                    ifelse(60 <= age1 & age1 < 65, "60-64",
                    ifelse(65 <= age1 & age1 < 70, "65-69",
                    ifelse(70 <= age1 & age1 < 75, "70-74",
                    ifelse(75 <= age1 & age1 < 80, "75-79",
                    ifelse(80 <= age1            , "80+", NA))))))))))))))))))

adm1.pop$'2013' <- adm1.pop$'2012'*(as.numeric(growth.NER[1,2])/100 +1)
adm1.pop$'2014' <- adm1.pop$'2013'*(as.numeric(growth.NER[1,2])/100 +1)
adm1.pop$'2015' <- adm1.pop$'2014'*(as.numeric(growth.NER[1,2])/100 +1)

NERpopF.adm1  <- filter(adm1.pop, sex == "female")
NERpopM.adm1  <- filter(adm1.pop, sex == "male")

# Adm2

adm2.pop      <- read_excel("ins-projections-adm2.xlsx")
adm2.pop      <- gather(adm2.pop, key = "age1", value = "2012", -c("name", "reg_code", "name2", "reg_code2", "sex"))
adm2.pop$age1 <- as.numeric(adm2.pop$age1)

# Convert into 5-year age1 groups
adm2.pop <- adm2.pop %>%
  mutate(age = ifelse(0  <= age1 & age1 < 5, "0-4",
                    ifelse(5  <= age1 & age1 < 10, "5-9",
                    ifelse(10 <= age1 & age1 < 15, "10-14",
                    ifelse(15 <= age1 & age1 < 20, "15-19",
                    ifelse(20 <= age1 & age1 < 25, "20-24",
                    ifelse(25 <= age1 & age1 < 30, "25-29",
                    ifelse(30 <= age1 & age1 < 35, "30-34",
                    ifelse(35 <= age1 & age1 < 40, "35-39",
                    ifelse(40 <= age1 & age1 < 45, "40-44",
                    ifelse(45 <= age1 & age1 < 50, "45-49",
                    ifelse(50 <= age1 & age1 < 55, "50-54",
                    ifelse(55 <= age1 & age1 < 60, "55-59",
                    ifelse(60 <= age1 & age1 < 65, "60-64",
                    ifelse(65 <= age1 & age1 < 70, "65-69",
                    ifelse(70 <= age1 & age1 < 75, "70-74",
                    ifelse(75 <= age1 & age1 < 80, "75-79",
                    ifelse(80 <= age1           , "80+", NA))))))))))))))))))

adm2.pop$'2013' <- adm2.pop$'2012'*(as.numeric(growth.NER[1,2])/100 +1)
adm2.pop$'2014' <- adm2.pop$'2013'*(as.numeric(growth.NER[1,2])/100 +1)
adm2.pop$'2015' <- adm2.pop$'2014'*(as.numeric(growth.NER[1,2])/100 +1)

NERpopF.adm2  <- filter(adm2.pop, sex == "female")
NERpopM.adm2  <- filter(adm2.pop, sex == "male")

# Adm3

adm3.pop      <- read_excel("ins-projections-adm3.xlsx")
adm3.pop      <- gather(adm3.pop, key = "age1", value = "2012", -c("name", "reg_code", "name2", "reg_code2", "name3", "reg_code3", "sex"))
adm3.pop$age1 <- as.numeric(adm3.pop$age1)

adm3.pop <- adm3.pop %>%
  mutate(age = ifelse(0  <= age1 & age1 < 5, "0-4",
                    ifelse(5  <= age1 & age1 < 10, "5-9",
                    ifelse(10 <= age1 & age1 < 15, "10-14",
                    ifelse(15 <= age1 & age1 < 20, "15-19",
                    ifelse(20 <= age1 & age1 < 25, "20-24",
                    ifelse(25 <= age1 & age1 < 30, "25-29",
                    ifelse(30 <= age1 & age1 < 35, "30-34",
                    ifelse(35 <= age1 & age1 < 40, "35-39",
                    ifelse(40 <= age1 & age1 < 45, "40-44",
                    ifelse(45 <= age1 & age1 < 50, "45-49",
                    ifelse(50 <= age1 & age1 < 55, "50-54",
                    ifelse(55 <= age1 & age1 < 60, "55-59",
                    ifelse(60 <= age1 & age1 < 65, "60-64",
                    ifelse(65 <= age1 & age1 < 70, "65-69",
                    ifelse(70 <= age1 & age1 < 75, "70-74",
                    ifelse(75 <= age1 & age1 < 80, "75-79",
                    ifelse(80 <= age1           , "80+", NA))))))))))))))))))

adm3.pop$'2013' <- adm3.pop$'2012'*(as.numeric(growth.NER[1,2])/100 +1)
adm3.pop$'2014' <- adm3.pop$'2013'*(as.numeric(growth.NER[1,2])/100 +1)
adm3.pop$'2015' <- adm3.pop$'2014'*(as.numeric(growth.NER[1,2])/100 +1)

NERpopF.adm3 <- filter(adm3.pop, sex == "female")
NERpopM.adm3 <- filter(adm3.pop, sex == "male")

# Verificaton of boundary changes beween fertility assumptions and --------
# As the Niger DHS and Census both took place in 2012, boundaries do note change in between them

# Export population data --------------------------------------------------

write.csv(NERpopF.adm0, paste0(NER.output, "NERpopF.adm0.csv"), row.names = F)
write.csv(NERpopM.adm0, paste0(NER.output, "NERpopM.adm0.csv"), row.names = F)

write.csv(NERpopF.adm1, paste0(NER.output, "NERpopF.adm1.csv"), row.names = F)
write.csv(NERpopM.adm1, paste0(NER.output, "NERpopM.adm1.csv"), row.names = F)

write.csv(NERpopF.adm2, paste0(NER.output, "NERpopF.adm2.csv"), row.names = F)
write.csv(NERpopM.adm2, paste0(NER.output, "NERpopM.adm2.csv"), row.names = F)

write.csv(NERpopF.adm3, paste0(NER.output, "NERpopF.adm3.csv"), row.names = F)
write.csv(NERpopM.adm3, paste0(NER.output, "NERpopM.adm3.csv"), row.names = F)

popF0.adm0.file <- NERpopF.adm0
popM0.adm0.file <- NERpopM.adm0

popF0.adm1.file <- NERpopF.adm1
popM0.adm1.file <- NERpopM.adm1

popF0.adm2.file <- NERpopF.adm2
popM0.adm2.file <- NERpopM.adm2

popF0.adm3.file <- NERpopF.adm3
popM0.adm3.file <- NERpopM.adm3


