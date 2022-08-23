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
#   baseID = represents what id each group will be given
#Return value: Plots and excel file in selected folder.
createGroups <- function(db, name, input, baseID) {
  
  # Groups created can be found on excel document
  cat("\n\nCreating Groups...\n\n")
  
  G1 <- filter(db ,as.numeric(Day_on_PKT) == 0)
  
  G2 <- db %>%
    filter(as.numeric(Day_on_PKT) <= as.numeric(120)) %>%
    filter(as.numeric(Day_on_PKT) >= as.numeric(1))
  
  G3 <- db %>% 
    filter(as.numeric(Day_on_PKT)  <= as.numeric(1096)) %>%
    filter(as.numeric(Day_on_PKT) >= as.numeric(1))
  
  G4 <- db %>% 
    filter(as.numeric(Day_on_PKT)  <= as.numeric(3287)) %>%
    filter(as.numeric(Day_on_PKT) >= as.numeric(1097))
  
  G5 <- db %>% 
    filter(as.numeric(Day_on_PKT)  <= as.numeric(8395)) %>%
    filter(as.numeric(Day_on_PKT) >= as.numeric(3288))
  
  F_base <- filter(db ,as.numeric(Day_on_PKT) == 0)
  B_names <- F_base %>% select(Patient_Name) %>% distinct(Patient_Name)
  F_PKT <- filter(db, as.numeric(Day_on_PKT)  <= as.numeric(120))
  F_PKT <- filter(F_PKT, as.numeric(Day_on_PKT) >= as.numeric(1))
  P_names <- F_PKT %>% select(Patient_Name) %>% distinct(Patient_Name)
  all_names <- semi_join(P_names,B_names, by=c("Patient_Name" = "Patient_Name"))
  
  comparisonData <- left_join(all_names, db, by=c("Patient_Name" = "Patient_Name"))
  
  G6 <- filter(comparisonData, as.numeric(Day_on_PKT) == 0)
  
  G7 <- comparisonData %>% 
    filter(as.numeric(Day_on_PKT) <= as.numeric(120)) %>%
    filter(as.numeric(Day_on_PKT) >= as.numeric(1))
  
  G8 <- G3 %>% count(Patient_Name) %>% filter(n > 1) %>% select(Patient_Name)
  G8 <- left_join(G8, db, by=c("Patient_Name" = "Patient_Name"))
  G8 <- G8 %>%
    filter(as.numeric(Day_on_PKT)  <= as.numeric(1096)) %>%
    filter(as.numeric(Day_on_PKT) >= as.numeric(1))
  
  G9 <- G4 %>% count(Patient_Name) %>% filter(n > 1) %>% select(Patient_Name)
  G9 <- left_join(G9, db, by=c("Patient_Name" = "Patient_Name"))
  G9 <- G9 %>%
    filter(as.numeric(Day_on_PKT)  <= as.numeric(3287)) %>%
    filter(as.numeric(Day_on_PKT) >= as.numeric(1097))
  
  G10 <- G5 %>% count(Patient_Name) %>% filter(n > 1) %>% select(Patient_Name)
  G10 <- left_join(G10, db, by=c("Patient_Name" = "Patient_Name"))
  G10 <- G10 %>%
    filter(as.numeric(Day_on_PKT)  <= as.numeric(8395)) %>%
    filter(as.numeric(Day_on_PKT) >= as.numeric(3288))
  
  G11 <- comparisonData
  
  G12 <- G3 %>% count(Patient_Name) %>% filter(n > 1) %>% select(Patient_Name)
  G12 <- left_join(G12, db, by=c("Patient_Name" = "Patient_Name"))
  
  G13 <- G4 %>% count(Patient_Name) %>% filter(n > 1) %>% select(Patient_Name)
  G13 <- left_join(G13, db, by=c("Patient_Name" = "Patient_Name"))
  
  G14 <- G5 %>% count(Patient_Name) %>% filter(n > 1) %>% select(Patient_Name)
  G14 <- left_join(G14, db, by=c("Patient_Name" = "Patient_Name"))
  
  G15 <- db
  
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
    file.remove("Anon_Plasma_Groups.xlsx")
  }
  else if(file.exists("RBC_Groups.xlsx")) {
    file.remove("RBC_Groups.xlsx")
    file.remove("Anon_RBC_Groups.xlsx")
  }
  else if(file.exists("Stool_Groups.xlsx")) {
    file.remove("Stool_Groups.xlsx")
    file.remove("Anon_Stool_Groups.xlsx")
  }
  else if(file.exists("Plasma_RBC_Groups.xlsx")) {
    file.remove("Plasma_RBC_Groups.xlsx")
    file.remove("Anon_Plasma_RBC_Groups.xlsx")
  }
  else if(file.exists("Plasma_Stool_Groups.xlsx")) {
    file.remove("Plasma_Stool_Groups.xlsx")
    file.remove("Anon_Plasma_Stool_Groups.xlsx")
  }
  else if(file.exists("RBC_Stool_Groups.xlsx")) {
    file.remove("RBC_Stool_Groups.xlsx")
    file.remove("Anon_RBC_Stool_Groups.xlsx")
  }

  cat("\n\nStoring Files...\n\n")
  plotData(G1, name, baseID + 1, 0, 0)
  writeToExcel(G1, baseID + 1, input)
  plotData(G2, name, baseID + 2, 1, 120)
  writeToExcel(G2, baseID + 2, input)
  plotData(G3, name, baseID + 3, 1, 1096)
  writeToExcel(G3, baseID + 3, input)
  plotData(G4, name, baseID + 4, 1097, 3287)
  writeToExcel(G4, baseID + 4, input)
  plotData(G5, name, baseID + 5, 3288, 8395)
  writeToExcel(G5, baseID + 5, input)
  plotData(G6, name, baseID + 6, 0, 0)
  writeToExcel(G6, baseID + 6, input)
  plotData(G7, name, baseID + 7, 1, 120)
  writeToExcel(G7, baseID + 7, input)
  plotData(G8, name, baseID + 8, 1, 1096)
  writeToExcel(G8, baseID + 8, input)
  plotData(G9, name, baseID + 9, 1097, 3287)
  writeToExcel(G9, baseID + 9, input)
  plotData(G10, name, baseID + 10, 3288, 8395)
  writeToExcel(G10, baseID + 10, input)
  plotData(G11, name, baseID + 11, 0, 8395)
  writeToExcel(G11, baseID + 11, input)
  plotData(G12, name, baseID + 12, 0, 8395)
  writeToExcel(G12, baseID + 12, input)
  plotData(G13, name, baseID + 13, 0, 8395)
  writeToExcel(G13, baseID + 13, input)
  plotData(G14, name, baseID + 14, 0, 8395)
  writeToExcel(G14, baseID + 14, input)
  plotData(G15, name, baseID + 15, 0, 8395)
  writeToExcel(G15, baseID + 15, input)
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
#   ID = Decides which sample group that this function will be representing.
#   low = the low value for the plot range.
#   high = the high value for the plot range.
#Return value: Plots in selected folder.
plotData <- function(df, group, ID, low, high) {
  edges <- df %>% group_by(Patient_Name) %>% summarize(min =  min(Day_on_PKT), max = max(Day_on_PKT))
  
  plot <- ggplot() +
    geom_segment(data=edges, mapping=aes(x=min, y=Patient_Name, xend=max, yend=Patient_Name), color="blue") +
    geom_point(data=df, mapping=aes(x=Day_on_PKT, y=Patient_Name)) +
    ggtitle(paste(group, "_", ID, sep="")) +
    xlab("Day on PKT") +
    ylab("Patient Name")
  
  if(ID%%100 == 15) {
    plot <- plot + theme(axis.text.y=element_text(size=4))
  }
  
  filename <- paste(group, "_",ID, ".pdf", sep="")
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
  
  plotDataAnon(df, group, ID, low, high) # Create anonymous version
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
#   ID = Decides which sample group that this function will be representing.
#   low = the low value for the plot range.
#   high = the high value for the plot range.
#Return value: Plots in selected folder.
plotDataAnon <- function(df, group, ID, low, high) {
  edges <- df %>% group_by(PatientID) %>% summarize(min =  min(Day_on_PKT), max = max(Day_on_PKT))
  
  plot <- ggplot() +
    geom_segment(data=edges, mapping=aes(x=min, y=PatientID, xend=max, yend=PatientID), color="blue") +
    geom_point(data=df, mapping=aes(x=Day_on_PKT, y=PatientID)) +
    ggtitle(paste(group, "_", ID, sep="")) +
    xlab("Day on PKT") +
    ylab("Patient ID")
  
  if(ID%%100 == 15) {
    plot <- plot + theme(axis.text.y=element_text(size=4))
  }
  
  filename <- paste("Anon", "_", group, "_",ID, ".pdf", sep="")
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
writeToExcel <- function(df, group, output) {
  df <- as.data.frame(df)
  
  # Moves patientID to the front of the datatabel
  IDindex <- grep("PatientID", names(df))
  if(IDindex != 1) {
    if(IDindex < ncol(df)) {
      df <- subset(df, select=c(IDindex,1:IDindex-1,IDindex+1:ncol(df)))
    }
    else if(IDindex == ncol(df)) {
      df <- subset(df, select=c(IDindex,1:IDindex-1))
    }
  }
  
  # Decides what filename to give to the excel file
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
  
  write.xlsx2(df, filename, sheetName = paste("G", group), col.names = T, row.names = F, append = T)
  
  writeToExcelAnon(df,filename, group) #create anonymous version
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
writeToExcelAnon <- function(df, filename, group) {
  
  # Removes identifiable patient data from data table
  filename <- paste("Anon_", filename, sep= "")
  df$Patient_Name <- NULL
  df$KGID <- NULL
  
  write.xlsx2(df, filename, sheetName = paste("G", group), col.names = T, row.names = F, append = T)
}

#helper function for stool.
Stool_Data <- function(stool) {
  stool <- filter(stool, !is.na(Day_on_PKT))
  return(stool)
}

#filters blood samples for stool.
Plasma_Data <- function(blood) {
  Plasma <- filter(blood, Sample_Type == "Plasma")
  Plasma <- filter(Plasma, !is.na(Day_on_PKT))
  return(Plasma)
}

#filters blood samples for RBC.
RBC_Data <- function(blood) {
  RBC <- filter(blood, Sample_Type == "RBC")
  RBC <- filter(RBC, !is.na(Day_on_PKT))
  return(RBC)
}

#helper function filters both blood(for Plasma) and Stool.
Plasma_Stool_Data <- function(blood, stool) {
  plasma <- filter(blood, Sample_Type == "Plasma")
  
  db <- sqldf("SELECT a.Sample_ID AS Plasma_ID, b.Sample_ID AS Stool_ID, a.Day_on_PKT AS Plasma_DOP, b.Day_on_PKT AS Stool_DOP, a.Patient_Name, a.Day_on_PKT, a.PatientID
              FROM plasma AS a, stool AS b
              Where a.Patient_Name = b.Patient_Name AND a.Day_on_PKT + 7 >= b.Day_on_PKT AND a.Day_on_PKT - 7 <= b.Day_on_PKT")
  
  db <- filter(db, !is.na(Day_on_PKT))
  return(db)
}

#helper function filters both blood(for RBC) and Stool.
RBC_Stool_Data <- function(blood, stool) {
  RBC <- filter(blood, Sample_Type == "RBC")
  
  db <- sqldf("SELECT a.Sample_ID AS RBC_ID, b.Sample_ID AS Stool_ID, a.Day_on_PKT AS RBC_DOP, b.Day_on_PKT AS Stool_DOP, a.Patient_Name, a.Day_on_PKT, a.PatientID
              FROM RBC AS a, stool AS b
              Where a.Patient_Name = b.Patient_Name AND a.Day_on_PKT + 7 >= b.Day_on_PKT AND a.Day_on_PKT - 7 <= b.Day_on_PKT")
  
  db <- filter(db, !is.na(Day_on_PKT))
  return(db)
}

#helper function that filters Plasma and RBC independently.
Plasma_RBC_Data <- function(blood) {
  plasma <- filter(blood, Sample_Type == "Plasma")
  RBC <- filter(blood, Sample_Type == "RBC")
  
  db <- sqldf("SELECT a.Sample_ID AS Plasma_ID, b.Sample_ID AS RBC_ID, a.Day_on_PKT AS Plasma_DOP, b.Day_on_PKT AS RBC_DOP, a.Patient_Name, a.Day_on_PKT, a.PatientID
              FROM plasma AS a, RBC AS b
              Where a.Patient_Name = b.Patient_Name AND a.Day_on_PKT + 7 >= b.Day_on_PKT AND a.Day_on_PKT - 7 <= b.Day_on_PKT")
  
  db <- filter(db, !is.na(Day_on_PKT))
  return(db)
}

# finds the highest day on pkt for every database and return a hash table
# NOT CURRENTLY USED
hashCreation <- function(stool, plasma, rbc, plasma_stool, rbc_stool, plasma_rbc) {
  t <- new.env()
  t[["stool"]] <- max(stool$Day_on_PKT)
  t[["plasma"]] <- max(plasma$Day_on_PKT)
  t[["rbc"]] <- max(rbc$Day_on_PKT)
  t[["plasma_stool"]] <- max(plasma_stool$Day_on_PKT)
  t[["plasma_rbc"]] <- max(plasma_rbc$Day_on_PKT)
  t[["rbc_stool"]] <- max(rbc_stool$Day_on_PKT)

  return(t)
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
  
  # File location where we can find main databases
  dataDirectory <- "OMITTED"
  setwd(dataDirectory)
  
  # Databases collected here (Where Eating Status is T-1)
  blood_S_all <- read_excel("GCRC Blood Analysis Patient Records.xlsm",sheet = "OMITTED", col_types=c("text","text","text","date","numeric","text","text","date","text","date","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"))
  blood_S_all <- filter(blood_S_all, Eating_Status == "T-1")
  stool_S_all <- read_excel("GCRC Stool Analysis Patient Records.xlsm",sheet = "OMITTED", col_types=c("text","text","text","date","numeric","text","text","date","text","date","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"))
  stool_S_all <- filter(stool_S_all, Eating_Status == "T-1")
  
  
  cat("\nWelcome to the patient grouping program.\n")
  cat("\nPlease enter where the Patient Key database is located.\n")
  input <- readline(prompt="Enter here: ")
  setwd(input)
  
  # Takes in key with a new ID for every patient(not KDID)
  key <- read_excel("Patientkey.xlsx", sheet = "OMITTED", col_types=c("text", "text"))
  
  # Matches data with the key for every patient
  blood_S_all <- left_join(blood_S_all, key, by=c("Patient_Name" = "Patient_Name"))
  stool_S_all <- left_join(stool_S_all, key, by=c("Patient_Name" = "Patient_Name"))
  
  
  # Create new data tables for program
  cat("\nProcessing data tables...\n")
  Stool <- Stool_Data(stool_S_all)
  Plasma <- Plasma_Data(blood_S_all)
  RBC <- RBC_Data(blood_S_all)
  Plasma_Stool <- Plasma_Stool_Data(blood_S_all, stool_S_all)
  RBC_Stool <- RBC_Stool_Data(blood_S_all, stool_S_all)
  Plasma_RBC <- Plasma_RBC_Data(blood_S_all)
  cat("\nData tables created.\n")
  
  # Collect highest values from each database into an hash table
  # maxHash <- hashCreation(Stool, Plasma, RBC, Plasma_Stool, RBC_Stool, Plasma_RBC)
  # maxHash currently not used due to all max values being 8391 (Could be used in the future)
  
  end <- F
  while(!end) {
    #menu
    cat("\nWhat data would you like?\n[1] Stool\n[2] Plasma\n[3] RBC\n[4] Plasma and Stool\n[5] RBC and Stool\n[6] Plasma and RBC\n")
    input <- readline(prompt="Enter the Menu Number here: ")
    
    # Depending on the group chosen, the starting group id count will change.
    IDcounter = 0
    
    if(input == 1) {
      #Stool
      IDcounter = 0
      createGroups(Stool, "Stool", input, IDcounter)
      end <- T
    }
    else if(input == 2) {
      #Plasma
      IDcounter = 100
      createGroups(Plasma, "Plasma", input, IDcounter)
      end <- T
    }
    else if(input == 3) {
      #RBC
      IDcounter = 300
      createGroups(RBC, "RBC", input, IDcounter)
      end <- T
    }
    else if(input == 4) {
      #Plasma & Stool
      IDcounter = 200
      createGroups(Plasma_Stool, "Plasma_Stool", input, IDcounter)
      end <- T
    }
    else if(input == 5) {
      IDcounter = 500
      #RBC & Stool
      createGroups(RBC_Stool, "RBC_Stool", input, IDcounter)
      end <- T
    }
    else if(input == 6) {
      #Plasma & RBC
      IDcounter = 400
      createGroups(Plasma_RBC, "Plasma_RBC", input, IDcounter)
      end <- T
    }
    else {
      cat("\n\nInvalid input... Please Try again.\n\n")
    }
  }
}

main()
#NOTE (07/23/2019): All warnings in this program are currently caused by the read_excel function.