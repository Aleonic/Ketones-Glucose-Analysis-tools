#Rversion: 3.4.1
#Note: version 2 of SG_SampleGroups.R
#Purpose: Categorize patient data into groups.
#Result: Categorize and display different sample groups.


writeToExcel <- function(df, group_ID) {
  df <- as.data.frame(df)
  write.xlsx2(df, "SampleGroups2.xlsx", sheetName = group_ID,col.names = T,row.names = F,append = T)
}

plotData <- function(stool, group_ID) {
  
  edges <- stool %>% group_by(Patient_Name) %>% summarize(min =  min(Day_on_PKT), max = max(Day_on_PKT))
  
  plot <- ggplot() +
    geom_segment(data=edges, mapping=aes(x=min, y=Patient_Name, xend=max, yend=Patient_Name), color="blue") +
    geom_point(data=stool, mapping=aes(x=Day_on_PKT, y=Patient_Name)) +
    ggtitle(paste(group_ID,"_Stool")) +
    xlab("Day on PKT") +
    ylab("Patient Name")
  
  filename <- paste(group_ID,"graph.pdf", sep= "")
  pdf(file = filename)
  
  plot(plot)
  dev.off()
}

SG1_f <- function(stool) {
  
  S_base <- filter(stool,as.numeric(Day_on_PKT) == 0)
  
  S_patients <- S_base %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  cat("SG1:\n\tMicro:",nrow(S_patients),"\n")
  
  plotData(S_base, "SG1")
  
  writeToExcel(S_base, "SG1")
}

SG2_f <- function(stool) {
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 120)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) != 0)
  
  S_patients <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)

  cat("SG2:\n\tMicro:",nrow(S_patients),"\n")
  
  plotData(S_PKT, "SG2")
  
  writeToExcel(S_PKT, "SG2")
}

SG3_f <- function(stool) {
  
  S_base <- filter(stool,as.numeric(Day_on_PKT) == 0)
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 120)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) != 0)
  
  S_base <- S_base %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  S_PKT <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)

  S_patients <- semi_join(S_base,S_PKT, by=c("Patient_Name" = "Patient_Name"))
  S_patients <- S_patients %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  S_data <- left_join(S_patients, stool, by=c("Patient_Name" = "Patient_Name"))
  S_data <- filter(S_data,as.numeric(Day_on_PKT) == 0)
  
  
  cat("SG3:\n\tMicro:",nrow(S_patients),"\n")
  
  
  plotData(S_data, "SG3")
  
  writeToExcel(S_data, "SG3")
}

SG4_f <- function(stool) {
  
  S_base <- filter(stool,as.numeric(Day_on_PKT) == 0)
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 120)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) != 0)
  
  S_base <- S_base %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  S_PKT <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  S_patients <- semi_join(S_base,S_PKT, by=c("Patient_Name" = "Patient_Name"))
  S_patients <- S_patients %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  S_data <- left_join(S_patients, stool, by=c("Patient_Name" = "Patient_Name"))
  S_data <- filter(S_data, as.numeric(Day_on_PKT) <= 120)
  S_data <- filter(S_data,as.numeric(Day_on_PKT) != 0)
  
  
  cat("SG4:\n\tMicro:",nrow(S_patients),"\n")
  
  plotData(S_data, "SG4")
  
  writeToExcel(S_data, "SG5")
}

SG5_f <- function(stool) {
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 1095)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) > 0)
  
  S_patients <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  cat("SG5:\n\tMicro:",nrow(S_patients),"\n")
  
  plotData(S_PKT, "SG5")
}

SG6_f <- function(stool) {
  S_PKT <- filter(stool, Stool_Group == 6)
  
  S_patients <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  cat("SG6:\n\tMicro:",nrow(S_patients),"\n")
  
  plotData(S_PKT, "SG6")
  
  writeToExcel(S_PKT, "SG6")
}

SG7_f <- function(stool) {
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 3285)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) > 1095)
  
  S_patients <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  cat("SG7:\n\tMicro:",nrow(S_patients),"\n")
  
  plotData(S_PKT, "SG7")
  
  writeToExcel(S_PKT, "SG7")
}

SG8_f <- function(stool) {
  
  S_PKT <- filter(stool, Stool_Group == 8)
  
  S_patients <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  cat("SG8:\n\tMicro:",nrow(S_patients),"\n")
  
  plotData(S_PKT, "SG8")
  
  writeToExcel(S_PKT, "SG8")
}

SG9_f <- function(stool) {
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 7300)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) > 3285)
  
  S_patients <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  cat("SG9:\n\tMicro:",nrow(S_patients),"\n")
  
  plotData(S_PKT, "SG9")
  
  writeToExcel(S_PKT, "SG9")
}

SG10_f <- function(stool) {
  
  S_PKT <- filter(stool, Stool_Group == 10)
  
  S_patients <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  cat("SG10:\n\tMicro:",nrow(S_patients),"\n")
  
  plotData(S_PKT, "SG10")
  
  writeToExcel(S_PKT, "SG10")
}

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
  if (!require("ggplot2")) {
    install.packages("ggplot2")
  }
  library(ggplot2)
  library(dplyr)
  library(readxl)
  library(xlsx)
  
  dataDirectory <- "OMITTED"
  setwd(dataDirectory)
  
  #if the data in the file in changed, then the column types need to be changed
  stool_S_all <- read_excel("GCRC Stool Analysis Patient Records.xlsm",sheet = "OMITTED", col_types=c("text","text","text","date","numeric","text","text","date","text","date","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"))

  
  preDirectory <- "OMITTED"
  setwd(preDirectory)
  
  predata <- read_excel("SG_6_8_10_data.xlsx", sheet="OMITTED", col_types = c("text","text","numeric","text","text","text","numeric"))
  
  print("Please enter the directory you would like the data to be placed.")
  directory <- readline(prompt="Enter here: ")
  setwd(directory)

  if(file.exists("SampleGroups2.xlsx")) {
    file.remove("SampleGroups2.xlsx")
  }
  
  SG1_f(stool_S_all)
  SG2_f(stool_S_all)
  SG3_f(stool_S_all)
  SG4_f(stool_S_all)
  SG5_f(stool_S_all)
  SG6_f(predata)
  SG7_f(stool_S_all)
  SG8_f(predata)
  SG9_f(stool_S_all)
  SG10_f(predata)
  
  cat("Samples groups have been stored.\nData can be found in:\n", directory)
}

main()