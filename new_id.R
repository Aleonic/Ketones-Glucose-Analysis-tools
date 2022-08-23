

main <- function() {
  
  if (!require("readxl")) {
    install.packages("readxl")
  }
  if (!require("xlsx")) {
    install.packages("xlsx")
  }
  if (!require("dplyr")) {
    install.packages("dplyr")
  }
  library(dplyr)
  library(readxl)
  library(xlsx)
  
  dataDirectory <- "OMITTED"
  setwd(dataDirectory)
  
  blood_S_all <- read_excel("GCRC Blood Analysis Patient Records.xlsm",sheet = "OMITTED", col_types=c("text","text","text","date","numeric","text","text","date","text","date","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"))
  
  stool_S <- read_excel("GCRC Stool Analysis Patient Records.xlsm",sheet = "OMITTED", col_types=c("text","text","text","date","numeric","text","text","date","text","date","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"))
  
  blood <- blood_S_all %>% distinct(Patient_Name) %>% filter(!is.na(Patient_Name))
  stool <- stool_S %>% distinct(Patient_Name) %>% filter(!is.na(Patient_Name))
  
  all <- full_join(blood, stool)
  
  
  PatientID <- c()
  for(i in 1:nrow(all)) {
    PatientID <- c(PatientID, paste("MM", i, sep=""))
  }
  
  PatientID <- data.frame(PatientID)
  all <- cbind(all,PatientID)
  
  cat("\nIn what directory would you like to put this file?\n")
  
  input <- readline(prompt="Enter here: ")
  setwd(input)
  
  
  filename <- "Patientkey.xlsx"
  if(file.exists(filename)) {
    file.remove(filename)
  }
  
  write.xlsx2(all, filename, sheetName = "OMITTED",col.names = T,row.names = F,append = T)
  
}

main()