#Rversion: 3.4.1
#Note: Latest data groups as of 06/18/19 version for SG_SampleGroups.R
#Purpose: Categorize patient data into groups.
#Result: Categorize and display different sample groups

#helper function filters both blood(for Plasma) and Stool.

Plasma_Data <- function(blood) {
  db <- blood %>%
    filter(Sample_Type == "Plasma") %>%
    count(Sample_ID) %>%
    filter(n > 1) %>%
    select(Sample_ID)
  
  return(db)
}

Stool_Data <- function(stool) {
  db <- stool %>%
    count(Sample_ID) %>%
    filter(n > 1) %>%
    select(Sample_ID)
  
  return(db)
}

RBC_Data <- function(blood) {
  db <- blood %>%
    filter(Sample_Type == "RBC") %>%
    count(Sample_ID) %>%
    filter(n > 1) %>%
    select(Sample_ID)
  
  return(db)
}

Plasma_Stool_Data <- function(blood, stool) {
  plasma <- filter(blood, Sample_Type == "Plasma")
  
  db <- sqldf("SELECT a.Sample_ID
              FROM plasma AS a, stool AS b
              Where a.Sample_ID = b.Sample_ID")
  
  return(db)
}

#helper function filters both blood(for RBC) and Stool.
RBC_Stool_Data <- function(blood, stool) {
  RBC <- filter(blood, Sample_Type == "RBC")
  
  db <- sqldf("SELECT a.Sample_ID
              FROM RBC AS a, stool AS b
              Where a.Sample_ID = b.Sample_ID")
  
  return(db)
}

#helper function that filters Plasma and RBC independently.
Plasma_RBC_Data <- function(blood) {
  plasma <- filter(blood, Sample_Type == "Plasma")
  RBC <- filter(blood, Sample_Type == "RBC")
  
  db <- sqldf("SELECT a.Sample_ID
              FROM plasma AS a, RBC AS b
              Where a.Sample_ID = b.Sample_ID")
  
  return(db)
}


#main user interactive function.
main <- function() {
  
  if (!require("readxl")) {
    install.packages("readxl")
  }
  if (!require("dplyr")) {
    install.packages("dplyr")
  }
  if (!require("sqldf")) {
    install.packages("sqldf")
  }
  library(dplyr)
  library(readxl)
  library(sqldf)
  
  # Location where we can find main databases
  dataDirectory <- "OMITTED"
  setwd(dataDirectory)
  
  # Databases collected here
  blood_S_all <- read_excel("GCRC Blood Analysis Patient Records.xlsm",sheet = "OMITTED", col_types=c("text","text","text","date","numeric","text","text","date","text","date","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"))
  stool_S_all <- read_excel("GCRC Stool Analysis Patient Records.xlsm",sheet = "OMITTED", col_types=c("text","text","text","date","numeric","text","text","date","text","date","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"))
  
  cat("\nWelcome to repeat finder program.\n")
  
  # Create new data tables for program
  cat("\nProcessing data tables...\n")
  Plasma <- Plasma_Data(blood_S_all)
  Stool <- Stool_Data(stool_S_all)
  RBC <- RBC_Data(blood_S_all)
  Plasma_Stool <- Plasma_Stool_Data(blood_S_all, stool_S_all)
  RBC_Stool <- RBC_Stool_Data(blood_S_all, stool_S_all)
  Plasma_RBC <- Plasma_RBC_Data(blood_S_all)
  cat("\nData tables created.\n")
  
  #Find repeating values in output of View
  View(Plasma)
  View(Stool)
  View(RBC)
  View(Plasma_RBC)
  View(RBC_Stool)
  View(Plasma_Stool)
}

main()