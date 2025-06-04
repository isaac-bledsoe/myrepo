
## packages
#install.packages("tidyverse")
#install.packages("pollster")
#install.packages("survey")

library(tidyverse)
library(pollster)
library(survey)


## Load R Data File
full_data <- hsls_17_student_pets_sr_v1_0

## Create vector of selected variables
ca_data <- select(full_data,c(
  "STU_ID","SCH_ID","X1NCESID","X2NCESID","STRAT_ID",
  "psu",
  "X2UNIV1",
  "X2UNIV2A",
  "X2UNIV2B",
  "X3UNIV1",
  "X4UNIV1",
  "W1STUDENT",
  "W1PARENT",
  "W1MATHTCH",
  "W1SCITCH",
  "W2STUDENT",
  "W2W1STU",
  "W2PARENT",
  "W2W1PAR",
  "W3STUDENT",
  "W3W1STU",
  "W3W1W2STU",
  "W3W2STU",
  "W3HSTRANS",
  "W3STUDENTTR",
  "W3W1STUTR",
  "W3W1W2STUTR",
  "W3W2STUTR",
  "W4STUDENT",
  "W4W1STU",
  "W4W1STUP1",
  "W4W1STUP1P2",
  "W4W1W2W3STU",
  "W5PSTRANS",
  "W5PSRECORDS",
  "W5W1W2W3W4PSTRANS",
  "W5W1W2W3W4PSRECORDS",
  "W3W1MATHTCH",
  "W3W1SCITCH",
  "X1SEX",
  "X1STU30OCC2",
  "X3NCESID",
  "X4NCESID",
  "X4RFDGMJ12",
  "S1OCC30THINK",
  "S4BIRTHSEX"
))

# Save dataset
save(ca_data, file="HSLS_caspirations.rdata")

# Checking crosstabs for extent of thought question -----------------------
#select needed variables for extent of thought
xtabdata1 <- ca_data %>% 
  select(S1OCC30THINK, X1SEX, W1STUDENT)

# Clean data (recode Na's to NA)
xtabdata1_clean <- xtabdata1 %>% 
  replace(xtabdata1 == "Item legitimate skip/NA", NA) %>%
  replace(xtabdata1 == "Unit non-response", NA) %>%
  replace(xtabdata1 == "Missing", NA)

#check all non-response is coded as NA
summary(xtabdata1_clean$S1OCC30THINK)

xtabdata1_clean <- na.omit(xtabdata1_clean[, c("S1OCC30THINK", "X1SEX", "W1STUDENT")])

## Crosstabs
crosstab(xtabdata1_clean,X1SEX,S1OCC30THINK,W1STUDENT, n=FALSE,)

# crosstabs match crosstabs from Power Query


# checking crosstabs for stickiness  --------------------------------------
#select needed variables: sex, job expectation, major choice
xtabdata2 <- ca_data %>% 
  select(X4RFDGMJ12, S4BIRTHSEX, X1STU30OCC2, W4STUDENT)

xtabdata2 <- xtabdata2 %>%
  mutate(X1STU30OCC2 = fct_collapse(X1STU30OCC2,
                                    STEM = c("Computer and Mathematical Occupations",
                                             "Architecture and Engineering Occupations",
                                             "Life, Physical, and Social Science Occupations"),
                                    HEAL = c("Education, Training, and Library Occupations",
                                             "Healthcare Practitioners and Technical Occupations",
                                             "Healthcare Support Occupations",
                                             "Community and Social Services Occupations"),
                                    Business = c("Management Occupations",
                                                 "Business and Financial Operations Occupations",
                                                 "Sales and Related Occupations"),
                                    Other = c("Building and Grounds Cleaning and Maintenance Occupations",
                                              "Farming, Fishing, and Forestry Occupations",
                                              "Construction and Extraction Occupations",
                                              "Installation, Maintenance, and Repair Occupations",
                                              "Production Occupations",
                                              "Transportation and Material Moving Occupations",
                                              "Arts, Design, Entertainment, Sports, and Media Occupations",
                                              "Protective Service Occupations",
                                              "Military Specific Occupations",
                                              "Legal Occupations",
                                              "Food Preparation and Serving Related Occupations",
                                              "Office and Administrative Support Occupations",
                                              "Personal Care and Service Occupations",
                                              "Uncodeable")
  ))

xtabdata2 <- xtabdata2 %>%
  mutate(X4RFDGMJ12 = fct_collapse(X4RFDGMJ12,
                                   STEM = c("Architecture and Related Services",
                                            "Communications Technologies/Technicians and Support Services", 
                                            "Computer and Information Sciences and Support Services",
                                            "Engineering", 
                                            "Engineering Technologies/Technicians", 
                                            "Mathematics and Statistics",
                                            "Physical Sciences", 
                                            "Science Technologies/Technicians"),
                                   HEAL = c("Education",
                                            "Family and Consumer Sciences/Human Sciences", 
                                            "Biological and Biomedical Sciences",
                                            "Psychology",
                                            "Health Professions and Related Clinical Sciences",
                                            "Theology and Religious Vocations", 
                                            "Social Sciences"),
                                   Business = c("Business, Management, Marketing, and Related Support Services"),
                                   Other = c("Agriculture, Agriculture Operations, and Related Sciences", 
                                             "Construction Trades", 
                                             "Mechanic and Repair Technologies/Technicians",
                                             "Precision Production", 
                                             "Transportation and Materials Moving",
                                             "Visual and Performing Arts",
                                             "Military Technologies and Applied Sciences",
                                             "Security and Protective Services",
                                             "Natural Resources and Conservation", 
                                             "Area, Ethnic, Cultural, Gender, and Group Studies",
                                             "Communication, Journalism, and Related Programs",
                                             "Personal and Culinary Services", 
                                             "Foreign languages, literatures, and Linguistics", 
                                             "Legal Professions and Studies",
                                             "English Language and Literature/Letters", 
                                             "Liberal Arts and Sciences, General Studies and Humanities", 
                                             "Multi/Interdisciplinary Studies", 
                                             "Parks, Recreation, Leisure, and Fitness Studies",
                                             "Public Administration and Social Service Professions",
                                             "History", 
                                             "Residency Programs",
                                             "Philosophy and Religious Studies", 
                                             "Library Science")))

