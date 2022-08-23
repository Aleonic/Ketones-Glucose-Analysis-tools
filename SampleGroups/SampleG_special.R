#Rversion: 3.4.1
#Note: Experimental version of SG_SampleGroups.R
#Purpose: Categorize patient data into groups.
#Result: Categorize and display different sample groups

writeToExcel <- function(df, group_ID, sample_type, output) {
  df <- as.data.frame(df)
  filename <- NULL
  if(output == 1) {
    filename <- "PlasmaGroups.xlsx" 
  }
  else if(output == 2) {
    filename <- "StoolGroups.xlsx"
  }
  else if(output == 3) {
    filename <- "M_AND_MGroups.xlsx"
  }
  else {
    cat("Something went wrong in \"writeToExcel\"")
  }
  write.xlsx2(df, filename, sheetName = paste(group_ID, "_", sample_type),col.names = T,row.names = F,append = T)
  
}

plotData <- function(df, group_ID, category, type, low, high) {
  
  edges <- df %>% group_by(Patient_Name) %>% summarize(min =  min(Day_on_PKT), max = max(Day_on_PKT))
  
  plot <- ggplot() +
    geom_segment(data=edges, mapping=aes(x=min, y=Patient_Name, xend=max, yend=Patient_Name), color="blue") +
    geom_point(data=df, mapping=aes(x=Day_on_PKT, y=Patient_Name)) +
    ggtitle(paste(group_ID,"_",category,"_", type, sep="")) +
    xlab("Day on PKT") +
    ylab("Patient Name")
  
  filename <- paste(group_ID, "_", category, ".pdf", sep="")
  txt1 <- paste("The plot ranges from: ", low, "-", high, " Days on PKT", sep="")
  txt2 <- paste("Number of samples:", nrow(df))
  txt3 <- paste("Number of Patients:", nrow(edges))
  pdf(filename)
  plot(plot)
  plot(NA, xlim=c(0,4), ylim=c(0,4), bty='n',
       xaxt='n', yaxt='n', xlab='', ylab='')
  text(1,3,txt1, pos=3)
  text(1,2,txt2, pos=3)
  text(1,1,txt3, pos=3)
  dev.off()
}

createGroup <- function(df, gname, type, low, high, xlsx_value) {
  
  df_base <- filter(df,as.numeric(Day_on_PKT) == 0)
  
  df_PKT <- filter(df, as.numeric(Day_on_PKT)  <= as.numeric(high))
  df_PKT <- filter(df_PKT, as.numeric(Day_on_PKT) >= as.numeric(low))
  
  df_base <- df_base %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  df_PKT <- df_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  patients_id <- semi_join(df_base,df_PKT, by=c("Patient_Name" = "Patient_Name"))
  patients_id <- patients_id %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  df_Bdata <- left_join(patients_id, df, by=c("Patient_Name" = "Patient_Name"))
  df_Bdata <- filter(df_Bdata,as.numeric(Day_on_PKT) == 0)
  
  df_Pdata <- left_join(patients_id, df, by=c("Patient_Name" = "Patient_Name"))
  df_Pdata <- filter(df_Pdata, as.numeric(Day_on_PKT) <= as.numeric(high))
  df_Pdata <- filter(df_Pdata,as.numeric(Day_on_PKT) >= as.numeric(low))
  
  cat(gname, ":\n\nNumber of Patients in group:", nrow(patients_id), "\n")
  
  
  plotData(df_Bdata, gname, "Baseline", type, low, high)
  plotData(df_Pdata, gname, "PKT", type, low, high)
  
  writeToExcel(df_Bdata, gname, "Baseline", xlsx_value)
  writeToExcel(df_Pdata, gname, "PKT", xlsx_value)
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
  
  #if the data in the file is changed, then the column types need to be changed
  blood_S_all <- read_excel("GCRC Blood Analysis Patient Records.xlsm",sheet = "OMITTED", col_types=c("text","text","text","date","numeric","text","text","date","text","date","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"))
  plasma_S <- filter(blood_S_all, Sample_Type == "Plasma")
  
  stool_S <- read_excel("GCRC Stool Analysis Patient Records.xlsm",sheet = "OMITTED", col_types=c("text","text","text","date","numeric","text","text","date","text","date","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"))
  
  cat("Welcome to the SG grouping program.\n\n")
  
  mainData <- NULL
  plasma_bool <- F
  output_value <- NULL
  S_type <- NULL
  end <- F
  while(!end) {
    cat("\nWould you like to work with stool or plasma samples?\n")
    test <- readline(prompt="(P for plasma or S for stool): ")
    
    if(test == "P" || test == "p") {
      mainData <- plasma_S
      output_value <- 1
      plasma_bool <- T
      S_type <- "Plasma"
      end <- T
    }
    else if(test == "S" || test == "s") {
      mainData <- stool_S
      output_value <- 2
      S_type <- "Stool"
      end <- T
    }
    else{
      cat("\n\nInvalid input... Please Try again.\n\n")
    }
  }
  
  end <- F
  while(!end) {
    cat("\nPlease enter the name of the group.")
    group_Name <- readline(prompt="Group name: ")
    
    end2 <- F
    while(!end2) {
      cat("\nYou have entered:", group_Name)
      test <- readline(prompt="Is this correct? (Y or N): ")
      
      if(test == "Y" || test == "y") {
        end <- T
        end2 <- T
      }
      else if(test == "N" || test == "n") {
        end2 <- T
        cat("\n\nPlease try again.\n\n")
      }
      else{
        cat("\n\nInvalid input... Please Try again.\n\n")
      }
    }
  }
  
  group_Name <- toupper(group_Name)
  
  end <- F
  while(!end) {
    cat("\nPlease enter the low and high values of the groups.")
    low_value <- readline(prompt="Low: ")
    high_value <- readline(prompt="High: ")
    
    end2 <- F
    while(!end2) {
      cat("\nYou have entered:\n\tLow:", low_value, "\n\tHigh:", high_value)
      test <- readline(prompt="Is this correct? (Y or N): ")
      
      if(test == "Y" || test == "y") {
        end <- T
        end2 <- T
      }
      else if(test == "N" || test == "n") {
        end2 <- T
        cat("\n\nPlease try again.\n\n")
      }
      else{
        cat("\n\nInvalid input... Please Try again.\n\n")
      }
    }
  }
  
  
  end <- F
  if(plasma_bool) {
    while(!end) {
      cat("\nWould you like to filter the plasma samples by stool samples?")
      test <- readline(prompt="(Y or N): ")
      if(test == "Y" || test == "y") {
        plasmaFilter(mainData)
        output_value <- 3
        end <- T
      }
      else if(test == "N" || test == "n") {
        end <- T
      }
      else{
        cat("\n\nInvalid input... Please Try again.\n\n")
      }
    }
  }
  
  
  cat("\nPlease enter the directory you would like the data to be placed.")
  directory <- readline(prompt="Enter here: ")
  setwd(directory)
  
  cat("\nRetrieving data...\n\n")
  
  createGroup(mainData, group_Name, S_type, low_value, high_value, output_value)
  
  cat("\nPlots and Data created.")
}


main()