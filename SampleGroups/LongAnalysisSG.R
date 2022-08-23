#Rversion: 3.4.1
#Note: Latest data groups as of 06/18/19 version for SG_SampleGroups.R
#Purpose: Categorize patient data into groups.
#Result: Categorize and display different sample groups

#Name: Create Groups
#Goal: Filter the data frame for groups G1-G6
#Summary: 
#   Use the previously filtered data frame and filter them depending on the
#   specific group. Then use other functions to graph and store the data for
#   these groups.
#Parameters:
#   db = Prefiltered data that will be filtered again for the groups.
#   name = the name that would use to recognize the data frame used.
#   input = Decides what file to write to.
#Return value: Plots and excel file in selected folder.
createGroups <- function(db, name, input) {
  cat("\n\nCreating Groups...\n\n")
  
  db <- filter(db, !is.na(Day_on_PKT))
  
  F_base <- filter(db ,as.numeric(Day_on_PKT) == 0)
  
  B_names <- F_base %>% select(Patient_Name) %>% distinct(Patient_Name)
  
  F_PKT <- filter(db, as.numeric(Day_on_PKT)  <= as.numeric(120))
  F_PKT <- filter(F_PKT, as.numeric(Day_on_PKT) >= as.numeric(1))
  
  P_names <- F_PKT %>% select(Patient_Name) %>% distinct(Patient_Name)
  
  all_names <- semi_join(P_names,B_names, by=c("Patient_Name" = "Patient_Name"))
  
  G1 <- left_join(all_names, db, by=c("Patient_Name" = "Patient_Name"))
  G1 <- filter(G1,as.numeric(Day_on_PKT) == 0)
  
  G2 <- left_join(all_names, db, by=c("Patient_Name" = "Patient_Name"))
  G2 <- filter(G2, as.numeric(Day_on_PKT) <= as.numeric(120))
  G2 <- filter(G2, as.numeric(Day_on_PKT) >= as.numeric(1))
  
  G3_samples <- filter(db, as.numeric(Day_on_PKT)  <= as.numeric(1096))
  G3_samples <- filter(G3_samples, as.numeric(Day_on_PKT) >= as.numeric(1))
  
  G3_patient <- G3_samples %>% count(Patient_Name) %>% filter(n > 1) %>% select(Patient_Name)
  G3_patient <- left_join(G3_patient, db, by=c("Patient_Name" = "Patient_Name"))
  
  G4_samples <- filter(db, as.numeric(Day_on_PKT)  <= as.numeric(3287))
  G4_samples <- filter(G4_samples, as.numeric(Day_on_PKT) >= as.numeric(1097))
  
  G4_patient <- G4_samples %>% count(Patient_Name) %>% filter(n > 1) %>% select(Patient_Name)
  G4_patient <- left_join(G4_patient, db, by=c("Patient_Name" = "Patient_Name"))
  
  G5_samples <- filter(db, as.numeric(Day_on_PKT)  <= as.numeric(7305))
  G5_samples <- filter(G5_samples, as.numeric(Day_on_PKT) >= as.numeric(3288))
  
  G5_patient <- G5_samples %>% count(Patient_Name) %>% filter(n > 1) %>% select(Patient_Name)
  G5_patient <- left_join(G5_patient, db, by=c("Patient_Name" = "Patient_Name"))
  
  G6_samples <- db %>% 
    filter(!is.na(Patient_Name)) %>%
    filter(as.numeric(Day_on_PKT) >= as.numeric(0))
  
  G6_patient <- G6_samples %>% count(Patient_Name) %>% filter(n > 1) %>% select(Patient_Name)
  G6_patient <- left_join(G6_patient, db, by=c("Patient_Name" = "Patient_Name"))
  
  
  cat("\n\nData Collected!\n\n")
  
  end <- F
  while(!end) {
    cat("\nWhich directory would you like to put these files?\n")
    dir <- readline(prompt="Enter here: ")
    cat("\nYou entered:\n", dir, "\nIs this correct?")
    res <- readline(prompt="(Y or N): ")
    
    if(res == "Y"||res == "y") {
      end <- T
    }
    else if(res == "N"||res == "n") {
      cat("\n\nPlease try again...\n\n")
    }
    else {
      cat("\n\nInvalid input...Please try again...\n\n")
    }
  }
  setwd(dir)
  
  if(file.exists("Plasma_Groups.xlsx")) {
    file.remove("Plasma_Groups.xlsx")
    file.remove("Plasma_Anon_Groups.xlsx")
  }
  else if(file.exists("RBC_Groups.xlsx")) {
    file.remove("RBC_Groups.xlsx")
    file.remove("RBC_Anon_Groups.xlsx")
  }
  else if(file.exists("Stool_Groups.xlsx")) {
    file.remove("Stool_Groups.xlsx")
    file.remove("Stool_Anon_Groups.xlsx")
  }
  else if(file.exists("Plasma_RBC_Groups.xlsx")) {
    file.remove("Plasma_RBC_Groups.xlsx")
    file.remove("Plasma_RBC_Anon_Groups.xlsx")
  }
  else if(file.exists("Plasma_Stool_Groups.xlsx")) {
    file.remove("Plasma_Stool_Groups.xlsx")
    file.remove("Plasma_Stool_Anon_Groups.xlsx")
  }
  else if(file.exists("RBC_Stool_Groups.xlsx")) {
    file.remove("RBC_Stool_Groups.xlsx")
    file.remove("RBC_Stool_Anon_Groups.xlsx")
  }
  
  cat("\n\nStoring Files...\n\n")
  
  plotData(G1, name, "G1", "Baseline",  0, 0)
  writeToExcel(G1, "G1", "Baseline", input)
  plotData(G2, name, "G2", "PKT", 1, 120)
  writeToExcel(G2, "G2", "PKT", input)
  plotData(G3_samples, name, "G3", "Samples",  1, 1096)
  writeToExcel(G3_samples, "G3", "Samples", input)
  plotData(G3_patient, name, "G3", "Patients",  1, 1096)
  writeToExcel(G3_patient, "G3", "Patients", input)
  plotData(G4_samples, name, "G4", "Samples",  1097, 3287)
  writeToExcel(G4_samples, "G4", "Samples", input)
  plotData(G4_patient, name, "G4", "Patients",  1097, 3287)
  writeToExcel(G4_patient, "G4", "Patients", input)
  plotData(G5_samples, name, "G5", "Samples",  3288, 7305)
  writeToExcel(G5_samples, "G5", "Samples", input)
  plotData(G5_patient, name, "G5", "Patients",  3288, 7305)
  writeToExcel(G5_patient, "G5", "Patients", input)
  plotData(G6_samples, name, "G6", "Samples",  0, max(G6_samples$Day_on_PKT))
  writeToExcel(G6_samples, "G6", "Samples", input)
  plotData(G6_patient, name, "G6", "Patients",  0, max(G6_patient$Day_on_PKT))
  writeToExcel(G6_patient, "G6", "Patients", input)
  
  cat("\n\nFiles Stored!\n\n")
}


#Name: Plot Data
#Goal: To plot data using the given data.
#Summary: 
#   The function will take in a dataframe and a few other variable in order to add
#   more detail and understanding to the plots that will be placed in folders.
#Parameters:
#   df = Prefiltered data that will be used for the data points.
#   group = provides the name for the source of data. (Ex. Stool or Plasma)
#   category = Decides which sample group that this function will be representing.
#   type = Represents whether we are looking at sample data or patient data.
#   low = the low value for the plot range.
#   high = the high value for the plot range.
#Return value: Plots in selected folder.
plotData <- function(df, group, category, type, low, high) {
  edges <- df %>% group_by(Patient_Name) %>% summarize(min =  min(Day_on_PKT), max = max(Day_on_PKT))
  
  plot <- ggplot() +
    geom_segment(data=edges, mapping=aes(x=min, y=Patient_Name, xend=max, yend=Patient_Name), color="blue") +
    geom_point(data=df, mapping=aes(x=Day_on_PKT, y=Patient_Name)) +
    ggtitle(paste(group, "_", category,"_", type, sep="")) +
    xlab("Day on PKT") +
    ylab("Patient Name")
  
  if(category == "G6") {
    plot <- plot + theme(axis.text.y=element_text(size=4))
  }
  
  filename <- paste(group, "_",category,"_", type, ".pdf", sep="")
  txt1 <- paste("Plot ranges from: ", low, "-", high, " Days on PKT", sep="")
  txt2 <- paste("Number of samples:", nrow(df))
  txt3 <- paste("Number of Patients:", nrow(edges))
  
  pdf(filename)
  plot(plot)
  plot(NA, xlim=c(0,4), ylim=c(0,4), bty='n',
       xaxt='n', yaxt='n', xlab='', ylab='')
  text(1,3,txt1, pos=4)
  text(1,2,txt2, pos=4)
  text(1,1,txt3, pos=4)
  dev.off()
  
  plotDataAnon(df, group, category, type, low, high)
}

#Name: Plot Data Anonymous
#Goal: To plot data using the given data.
#Summary: 
#   The function will take in a dataframe and a few other variable in order to add
#   more detail and understanding to the plots that will be placed in folders. In addition, 
#   these plots will be UNIDENTIFIED unlike the other function above.
#Parameters:
#   df = Prefiltered data that will be used for the data points.
#   group = provides the name for the source of data. (Ex. Stool or Plasma)
#   category = Decides which sample group that this function will be representing.
#   type = Represents whether we are looking at sample data or patient data.
#   low = the low value for the plot range.
#   high = the high value for the plot range.
#Return value: Plots in selected folder.
plotDataAnon <- function(df, group, category, type, low, high) {
  edges <- df %>% group_by(PatientID) %>% summarize(min =  min(Day_on_PKT), max = max(Day_on_PKT))
  
  plot <- ggplot() +
    geom_segment(data=edges, mapping=aes(x=min, y=PatientID, xend=max, yend=PatientID), color="blue") +
    geom_point(data=df, mapping=aes(x=Day_on_PKT, y=PatientID)) +
    ggtitle(paste(group, "_", category,"_", type, sep="")) +
    xlab("Day on PKT") +
    ylab("Patient ID")
  
  if(category == "G6") {
    plot <- plot + theme(axis.text.y=element_text(size=4))
  }
  
  filename <- paste(group, "_Anon_",category,"_", type, ".pdf", sep="")
  txt1 <- paste("Plot ranges from: ", low, "-", high, " Days on PKT", sep="")
  txt2 <- paste("Number of samples:", nrow(df))
  txt3 <- paste("Number of Patients:", nrow(edges))
  
  pdf(filename)
  plot(plot)
  plot(NA, xlim=c(0,4), ylim=c(0,4), bty='n',
       xaxt='n', yaxt='n', xlab='', ylab='')
  text(1,3,txt1, pos=4)
  text(1,2,txt2, pos=4)
  text(1,1,txt3, pos=4)
  dev.off()
}

#Name: Write To Excel
#Goal: Write all the data that the plots represent
#Summary: 
#   This function will take in a dataframes for each plot and store them so they
#   might be able to be used at a later time.
#Parameters:
#   df = Prefiltered data that will be used for the data points.
#   group = Decides which sample group that this function will be representing. (Not the same group as plotData)
#   dataType = Represents whether we are looking at sample data or patient data.
#   output = Chooses which file name the data will be written to.
#Return value: Excel file in selected folder.
writeToExcel <- function(df, group, dataType, output) {
  df <- as.data.frame(df)
  
  IDindex <- grep("PatientID", names(df))
  if(IDindex != 1) {
    if(IDindex < ncol(df)) {
      df <- subset(df, select=c(IDindex,1:IDindex-1,IDindex+1:ncol(df)))
    }
    else if(IDindex == ncol(df)) {
      df <- subset(df, select=c(IDindex,1:IDindex-1))
    }
  }
  
  filename <- NULL
  if(output == 1) {
    filename <- "Stool_Groups.xlsx" 
  }
  else if(output == 2) {
    filename <- "Plasma_Groups.xlsx"
  }
  else if(output == 3) {
    filename <- "RBC_Groups.xlsx"
  }
  else if(output == 4) {
    filename <- "Plasma_Stool_Groups.xlsx"
  }
  else if(output == 5) {
    filename <- "RBC_Stool_Groups.xlsx"
  }
  else if(output == 6) {
    filename <- "Plasma_RBC_Groups.xlsx"
  }
  else {
    cat("Something went wrong in \"writeToExcel\"")
  }
  
  write.xlsx2(df, filename, sheetName = paste(group, "_",dataType),col.names = T,row.names = F,append = T)
  
  writeToExcelAnon(df,filename, group, dataType)
}

#Name: Write To Excel Anonymous
#Goal: Write all the data that the plots represent
#Summary: 
#   This function will take in a dataframes for each plot and store them so they
#   might be able to be used at a later time. However, this data will be UNIDENTIFIED.
#Parameters:
#   df = Prefiltered data that will be used for the data points.
#   group = Decides which sample group that this function will be representing. (Not the same group as plotData)
#   dataType = Represents whether we are looking at sample data or patient data.
#   output = Chooses which file name the data will be written to.
#Return value: Excel file in selected folder.
writeToExcelAnon <- function(df, filename, group, dataType) {
  filename <- gsub("_Groups","_Anon_Groups",filename)
  df$Patient_Name <- NULL
  df$KGID <- NULL
  
  write.xlsx2(df, filename, sheetName = paste(group, "_",dataType),col.names = T,row.names = F,append = T)
}

#helper function for stool.
Stool_Data <- function(stool) {
  createGroups(stool, "Stool", 1)
}

#filters blood samples for stool.
Plasma_Data <- function(blood) {
  Plasma <- filter(blood, Sample_Type == "Plasma")
  createGroups(Plasma, "Plasma", 2)
}

#filters blood samples for RBC.
RBC_Data <- function(blood) {
  RBC <- filter(blood, Sample_Type == "RBC")
  createGroups(RBC, "RBC", 3)
}

#helper function filters both blood(for Plasma) and Stool.
Plasma_Stool_Data <- function(blood, stool) {
  plasma <- filter(blood, Sample_Type == "Plasma")
  
  db <- sqldf("SELECT a.Sample_ID AS Plasma_ID, b.Sample_ID AS Stool_ID, a.Day_on_PKT AS Plasma_DOP, b.Day_on_PKT AS Stool_DOP, a.Patient_Name, a.Day_on_PKT, a.PatientID
              FROM plasma AS a, stool AS b
              Where a.Patient_Name = b.Patient_Name AND a.Day_on_PKT + 7 >= b.Day_on_PKT AND a.Day_on_PKT - 7 <= b.Day_on_PKT")
  
  createGroups(db, "Plasma_Stool", 4)
}

#helper function filters both blood(for RBC) and Stool.
RBC_Stool_Data <- function(blood, stool) {
  RBC <- filter(blood, Sample_Type == "RBC")
  
  db <- sqldf("SELECT a.Sample_ID AS RBC_ID, b.Sample_ID AS Stool_ID, a.Day_on_PKT AS RBC_DOP, b.Day_on_PKT AS Stool_DOP, a.Patient_Name, a.Day_on_PKT, a.PatientID
              FROM RBC AS a, stool AS b
              Where a.Patient_Name = b.Patient_Name AND a.Day_on_PKT + 7 >= b.Day_on_PKT AND a.Day_on_PKT - 7 <= b.Day_on_PKT")
  
  createGroups(db, "RBC_Stool", 5)
}

#helper function that filters Plasma and RBC independently.
Plasma_RBC_Data <- function(blood) {
  plasma <- filter(blood, Sample_Type == "Plasma")
  RBC <- filter(blood, Sample_Type == "RBC")
  
  db <- sqldf("SELECT a.Sample_ID AS Plasma_ID, b.Sample_ID AS RBC_ID, a.Day_on_PKT AS Plasma_DOP, b.Day_on_PKT AS RBC_DOP, a.Patient_Name, a.Day_on_PKT, a.PatientID
              FROM plasma AS a, RBC AS b
              Where a.Patient_Name = b.Patient_Name AND a.Day_on_PKT + 7 >= b.Day_on_PKT AND a.Day_on_PKT - 7 <= b.Day_on_PKT")
  
  createGroups(db, "Plasma_RBC", 6)
}

#main user interactive function.
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
  if (!require("sqldf")) {
    install.packages("sqldf")
  }
  library(dplyr)
  library(readxl)
  library(xlsx)
  library(sqldf)
  library(ggplot2)
  
  dataDirectory <- "OMITTED"
  setwd(dataDirectory)
  
  blood_S_all <- read_excel("GCRC Blood Analysis Patient Records.xlsm",sheet = "OMITTED", col_types=c("text","text","text","date","numeric","text","text","date","text","date","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"))
  stool_S <- read_excel("GCRC Stool Analysis Patient Records.xlsm",sheet = "OMITTED", col_types=c("text","text","text","date","numeric","text","text","date","text","date","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"))
  
  cat("\nWelcome to the patient grouping program.\n")
  
  cat("\nPlease enter where the Patient Key database is located.\n")
  input <- readline(prompt="Enter here: ")
  setwd(input)
  
  key <- read_excel("Patientkey.xlsx", sheet = "Key", col_types=c("text", "text"))
  
  blood_S_all <- left_join(blood_S_all, key, by=c("Patient_Name" = "Patient_Name"))
  stool_S <- left_join(stool_S, key, by=c("Patient_Name" = "Patient_Name"))
  
  end <- F
  while(!end) {
    #menu
    cat("\nWhat data would you like?\n[1] Stool\n[2] Plasma\n[3] RBC\n[4] Plasma and Stool\n[5] RBC and Stool\n[6] Plasma and RBC\n")

    input <- readline(prompt="Enter the Menu Number here: ")

    if(input == 1) {
      Stool_Data(stool_S)
      end <- T
    }
    else if(input == 2) {
      Plasma_Data(blood_S_all)
      end <- T
    }
    else if(input == 3) {
      RBC_Data(blood_S_all)
      end <- T
    }
    else if(input == 4) {
      Plasma_Stool_Data(blood_S_all, stool_S)
      end <- T
    }
    else if(input == 5) {
      RBC_Stool_Data(blood_S_all, stool_S)
      end <- T
    }
    else if(input == 6) {
      Plasma_RBC_Data(blood_S_all)
      end <- T
    }
    else {
      cat("\n\nInvalid input... Please Try again.\n\n")
    }
  }
}

main()