#Rversion: 3.4.1
#Purpose: plot predetermined groups by ranges on the x-axis
#Result: plot with ranged values using "gantt charts"

createPlots <- function(df) {
  groups <- distinct(df,Stool_Group)
  groups <- groups %>% pull(Stool_Group)
  edges <- df %>% group_by(Patient_Name, Stool_Group) %>% summarize(min =  min(Day_on_PKT), max = max(Day_on_PKT))
  filename <- "PatientRangePlots.pdf"
  pdf(file = filename)
  
  for(i in groups) {
    points <- filter(df, Stool_Group == i)
    corners <- filter(edges, Stool_Group == i)
    
    plot <- ggplot() +
      geom_segment(data=corners, mapping=aes(x=min, y=Patient_Name, xend=max, yend=Patient_Name), color="blue") +
      geom_point(data=points, mapping=aes(x=Day_on_PKT, y=Patient_Name)) +
      ggtitle(paste("SG", i,sep="")) +
      xlab("Day on PKT") +
      ylab("Patient Name")

    plot(plot)
  }
  
  dev.off()
}

RangePlot <- function() {
  if (!require("readxl")) {
    install.packages("readxl")
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
  
  mDirectory <- "OMITTED"
  setwd(mDirectory)
  dat <- read_excel("SampleGroups.xlsx", sheet="OMITTED", col_types = c("text","text","numeric","text","text","text","numeric"))

   
  
  createPlots(dat) 
  
  print("plot created")
  
}

RangePlot()