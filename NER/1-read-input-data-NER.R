##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: script to load census, DHS, MICS input data files
##
## Date created: 14 August 2019
## Last updated: 14 August 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

# Last census in Niger was in 2012, the same year a DHS took place. 
# No micro data are available for the 2012 census, however, INS has published single year 
# population estimates. The latest DHS was conducted in 2017, but deemed to low
# quality to be published (https://dhsprogram.com/pubs/pdf/OD76/OD76.pdf). 

# Read in data: INS projections -------------------------------------------

setwd(NER.input)

# Adm0

adm0.pop     <- read_excel("ins-projections-adm0.xlsx")
adm0.pop     <- gather(adm0.pop, key = "sex", value = "pop", -"age")

# Convert into 5-year age groups
adm0.pop <- adm0.pop %>%
  mutate(agegroup = ifelse(0  <= age & age < 5, "0-4",
                    ifelse(5  <= age & age < 10, "5-9",
                    ifelse(10 <= age & age < 15, "10-14",
                    ifelse(15 <= age & age < 20, "15-19",
                    ifelse(20 <= age & age < 25, "20-24",
                    ifelse(25 <= age & age < 30, "25-29",
                    ifelse(30 <= age & age < 35, "30-34",
                    ifelse(35 <= age & age < 40, "35-39",
                    ifelse(40 <= age & age < 45, "40-44",
                    ifelse(45 <= age & age < 50, "45-49",
                    ifelse(50 <= age & age < 55, "50-54",
                    ifelse(55 <= age & age < 60, "55-59",
                    ifelse(60 <= age & age < 65, "60-64",
                    ifelse(65 <= age & age < 70, "65-69",
                    ifelse(70 <= age & age < 78, "70-74",
                    ifelse(75 <= age & age < 80, "75-79",
                    ifelse(80 <= age           , "80+", NA))))))))))))))))))
# Adm1

adm1.pop     <- read_excel("ins-projections-adm1.xlsx")
adm1.pop     <- gather(adm1.pop, key = "age", value = "pop", -c("adm1", "code.adm1", "sex"))
adm1.pop$age <- as.numeric(adm1.pop$age)

# Convert into 5-year age groups
adm1.pop <- adm1.pop %>%
  mutate(agegroup = ifelse(0  <= age & age < 5, "0-4",
                    ifelse(5  <= age & age < 10, "5-9",
                    ifelse(10 <= age & age < 15, "10-14",
                    ifelse(15 <= age & age < 20, "15-19",
                    ifelse(20 <= age & age < 25, "20-24",
                    ifelse(25 <= age & age < 30, "25-29",
                    ifelse(30 <= age & age < 35, "30-34",
                    ifelse(35 <= age & age < 40, "35-39",
                    ifelse(40 <= age & age < 45, "40-44",
                    ifelse(45 <= age & age < 50, "45-49",
                    ifelse(50 <= age & age < 55, "50-54",
                    ifelse(55 <= age & age < 60, "55-59",
                    ifelse(60 <= age & age < 65, "60-64",
                    ifelse(65 <= age & age < 70, "65-69",
                    ifelse(70 <= age & age < 75, "70-74",
                    ifelse(75 <= age & age < 80, "75-79",
                    ifelse(80 <= age           , "80+", NA))))))))))))))))))
                                                                                                       
# Adm2

adm2.pop     <- read_excel("ins-projections-adm2.xlsx")
adm2.pop     <- gather(adm2.pop, key = "age", value = "pop", -c("adm1", "code.adm1", "adm2", "code.adm2", "sex"))
adm2.pop$age <- as.numeric(adm2.pop$age)

# Convert into 5-year age groups
adm2.pop <- adm2.pop %>%
  mutate(agegroup = ifelse(0  <= age & age < 5, "0-4",
                    ifelse(5  <= age & age < 10, "5-9",
                    ifelse(10 <= age & age < 15, "10-14",
                    ifelse(15 <= age & age < 20, "15-19",
                    ifelse(20 <= age & age < 25, "20-24",
                    ifelse(25 <= age & age < 30, "25-29",
                    ifelse(30 <= age & age < 35, "30-34",
                    ifelse(35 <= age & age < 40, "35-39",
                    ifelse(40 <= age & age < 45, "40-44",
                    ifelse(45 <= age & age < 50, "45-49",
                    ifelse(50 <= age & age < 55, "50-54",
                    ifelse(55 <= age & age < 60, "55-59",
                    ifelse(60 <= age & age < 65, "60-64",
                    ifelse(65 <= age & age < 70, "65-69",
                    ifelse(70 <= age & age < 75, "70-74",
                    ifelse(75 <= age & age < 80, "75-79",
                    ifelse(80 <= age           , "80+", NA))))))))))))))))))

# Adm3

adm3.pop     <- read_excel("ins-projections-adm3.xlsx")
adm3.pop     <- gather(adm3.pop, key = "age", value = "pop", -c("adm1", "code.adm1", "adm2", "code.adm2", "adm3", "code.adm3", "sex"))
adm3.pop$age <- as.numeric(adm3.pop$age)

adm3.pop <- adm3.pop %>%
  mutate(agegroup = ifelse(0  <= age & age < 5, "0-4",
                    ifelse(5  <= age & age < 10, "5-9",
                    ifelse(10 <= age & age < 15, "10-14",
                    ifelse(15 <= age & age < 20, "15-19",
                    ifelse(20 <= age & age < 25, "20-24",
                    ifelse(25 <= age & age < 30, "25-29",
                    ifelse(30 <= age & age < 35, "30-34",
                    ifelse(35 <= age & age < 40, "35-39",
                    ifelse(40 <= age & age < 45, "40-44",
                    ifelse(45 <= age & age < 50, "45-49",
                    ifelse(50 <= age & age < 55, "50-54",
                    ifelse(55 <= age & age < 60, "55-59",
                    ifelse(60 <= age & age < 65, "60-64",
                    ifelse(65 <= age & age < 70, "65-69",
                    ifelse(70 <= age & age < 75, "70-74",
                    ifelse(75 <= age & age < 80, "75-79",
                    ifelse(80 <= age           , "80+", NA))))))))))))))))))


# Verificaton of boundary changes beween fertility assumptions and --------
# As the Niger DHS and Census both took place in 2012, boundaries do note change in between them


# Export population data --------------------------------------------------

write.csv(CAMpopF, 
          "~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop/regdata/CAMpopF.csv", 
          row.names = TRUE)
write.csv(CAMpopM, 
          "~/Documents/UNFPA/sabbatical/Puebla-course-materials/lab-sessions/ALAPworkshop/regdata/CAMpopM.csv", 
          row.names = TRUE)
popF0.file <- CAMpopF
popM0.file <- CAMpopM



