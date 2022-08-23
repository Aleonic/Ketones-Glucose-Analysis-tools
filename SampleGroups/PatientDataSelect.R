#Rversion: 3.4.1
#Purpose: Categorize patient data into groups.
#Result: *No true current endgoal*



#Name: Get Patient Names
#Goal: Get each individual patient name from the datatable
#Summary: Function takes the dataTable and returns a list of the patient names.
#Parameters:
# dataTable = all data including all patients and samples.
#Return value: Patient names in a table
getPatientNames <- function(dataTable) {
  patientNames <- table(dataTable["Patient_Name"])
  return(names(patientNames))
}

#Name: Date Graph
#Goal: temporary graph to display Day_on_PKT vs Day_Of_Life
#Summary: 
#   Functions uses data to plot the data points for a specific color group
#   and display the number of patients in the specific color group.
#Parameters:
#   dat = data for the patients in a specific color group.
#Return value: creates plots and prints the number of patients in the color group.
dateGraph <- function(dat) {
  idList <- unique(dat$Patient_Name)
  
  for(i in idList) {
    print(i)
    plot <- dat %>%
      filter(Patient_Name == i) %>%
      ggplot(aes(x=Day_on_PKT, y=Day_Of_Life)) + 
      geom_line(linetype = "dashed") +
      geom_point() +
      labs(title=i)
    
    plot(plot)
  }
  cat("Size of color:", length(idList))
}

#Name: read Table
#Goal: Read Excel File and categorize the patients to color groups
#Summary: 
#   Function reads the data from the provided Excel file and uses sql logic to
#   categorize the data in categories.
#Parameters:
#Return value: Expected to print out graphs for each category. 
readTable <- function() {
  
  #installs the required library when necessary
  if (!require("xlsx")) {
    install.packages("xlsx")
  }
  if (!require("dplyr")) {
    install.packages("dplyr")
  }
  if (!require("ggplot2")) {
    install.packages("ggplot2")
  }
  
  #libraries to be used in program
  library(ggplot2)
  library(dplyr)
  library(xlsx)
  
  #set the directory to where the data table is.
  mdirectory <- "OMITTED"
  setwd(mdirectory)

  #read the excel file and store the data
  dat <- read.xlsx("New Combined table lists.xlsx",sheetName = "OMITTED")
  #dat <- filter(dat, Stool_Sample == 'X')
  
  
  #take only the patient names from excel data
  patients <- getPatientNames(dat)
  
  #display the number of samples vs patients
  cat("Number of all samples in the database:",nrow(dat), "\n")
  cat("Number of all patients in the database:", length(patients), "\n")
  
  #collect patients with a baseline
  pBaseline <- filter(dat,Day_on_PKT == 0)
  #View(pBaseline)
  
  #collect all values that are considered PKT
  pPKT <- filter(dat,Day_on_PKT > 0)
  #View(pPKT)
  
  #collect the patients with 2 or more PKT values
  p2OM <- pPKT %>% count(Patient_Name)
  p2OM <- filter(p2OM, n > 1)
  #View(p2OM)
  
  #collect all patients without a baseline
  nBaseline <- anti_join(dat, pBaseline, by=c("Patient_Name" = "Patient_Name"))
  nBaseline <- select(nBaseline, Patient_Name)
  nBaseline <- distinct(nBaseline)
  #View(nBaseline)
  
  #patients with no baseline and only 1 PKT
  red <- anti_join(nBaseline,p2OM, by=c("Patient_Name" = "Patient_Name"))
  red <- select(red,Patient_Name)
  #View(red)
  
  #patients with no baseline with 2 or more PKT samples
  orange <- anti_join(p2OM, pBaseline, by=c("Patient_Name" = "Patient_Name"))
  orange <- select(orange,Patient_Name)
  #View(orange)
  
  #patients with baseline but no PKT samples
  purple <- anti_join(pBaseline,pPKT, by=c("Patient_Name" = "Patient_Name"))
  purple <- select(purple,Patient_Name)
  #View(purple)
  
  #collection of patients with both baseline and PKT samples
  pBoth <- semi_join(pBaseline,pPKT, by=c("Patient_Name" = "Patient_Name"))
  pBoth <- select(pBoth, Patient_Name)
  #View(pBoth)
  
  #patients with a baseline and a single PKT sample
  yellow <- anti_join(pBoth,p2OM, by=c("Patient_Name" = "Patient_Name"))
  yellow <- select(yellow,Patient_Name)
  #View(yellow)
  
  #patients with a baseline with 2 or more PKT samples
  green <- semi_join(p2OM,pBoth, by=c("Patient_Name" = "Patient_Name"))
  green <- select(green,Patient_Name)
  #View(green)
  
  #Enter the category that you would like to plot/count here:
  test <- green

  test <- left_join(test,dat, by=c("Patient_Name" = "Patient_Name"))
  #View(test)

  dateGraph(test)
}

readTable()
