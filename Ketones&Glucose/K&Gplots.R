#Rversion: 3.4.1
#Purpose: Display Ketones and Glucose data for individual and population.
#Result: Plots created from data and can be placed in files.


#Name: Ketones & Glucose VS Time
#Goal: Plot both Ketones and Glucose vs time for each individual.
#Summary: Function uses data to plot Ketones and Glucose for individuals.
#Parameters:
# df = Data that contains individual's data on Ketones and Glucose.
#Return value: Ketones & Glucose VS Time plots for individuals.
KGvT_individual <- function(df) {
  
  pdf("Ketones&GlucoseVTime_individual.pdf")
  
  patients <- df %>% distinct(Patient) %>% pull(Patient) # Collect patient names
  
  for(i in patients) {
    temp <- filter(df, Patient == i)
    
    plot <-  ggplot(temp) +
      ggtitle(i) +
      geom_line(mapping=aes(x=Days_on_PKT, y=Blood_glucose_mg_per_dL,color="red")) +
      geom_point(mapping=aes(x=Days_on_PKT, y=Blood_glucose_mg_per_dL,color="red")) +
      geom_line(mapping=aes(x=Days_on_PKT, y=Blood_ketones_mmol_per_L,color="blue")) +
      geom_point(mapping=aes(x=Days_on_PKT, y=Blood_ketones_mmol_per_L,color="blue")) +
      scale_color_identity(name="Type",
                           breaks=c("red","blue"),
                           labels=c("Glucose (mg/dL)", "Ketones (mmol/L)"),
                           guide="legend") +
      xlab("Days on PKT") +
      ylab("Values")
      
   
    plot(plot) 
  }
  
  dev.off()
}

#Name: Ketones VS Glucose
#Goal: Plot Ketones vs Glucose for each individual.
#Summary: Function uses data provided to plot Ketones VS Glucose. The data is filtered
# so that each value for ketones has a glucose value as well.
#Parameters:
# df = Data that contains individual's data on Ketones and Glucose.
#Return value: Two Ketones VS Glucose plots for population.
KvG_individual <- function(df) {
  
  pdf("KetonesVGlucose_individual.pdf")
  
  df <- df %>% 
    filter(!is.na(Blood_glucose_mg_per_dL)) %>% 
    filter(!is.na(Blood_ketones_mmol_per_L))
  
  patients <- df %>%
    distinct(Patient) %>% 
    pull(Patient) # Collect patient names
  
  for(i in patients) {
    temp <- filter(df, Patient == i)
    
    plot <-  ggplot(temp) +
      ggtitle(i) +
      geom_line(mapping=aes(x=Blood_ketones_mmol_per_L, y=Blood_glucose_mg_per_dL)) +
      geom_point(mapping=aes(x=Blood_ketones_mmol_per_L, y=Blood_glucose_mg_per_dL)) +
      xlab("Ketones (mmol/L)") +
      ylab("Glucose (mg/dL)")
    plot(plot) 
  }
  
  dev.off()
}

#Name: Ketones VS Time
#Goal: Plot Ketones vs time for data population.
#Summary: Function uses data to plot Ketones data
#Parameters:
# df = Data that contains individual's data on Ketones and Glucose.
#Return value: Ketones VS Time plots for population.
KvT_pop <- function(df, split) {
  
  pdf("KetonesvTime_population.pdf")
  
  maxValueX <- max(df$Days_on_PKT, na.rm = TRUE)
  maxValueY <- max(df$Blood_ketones_mmol_per_L, na.rm = TRUE)
  
  dx <- ceiling(maxValueX/split)
  dy <- ceiling(maxValueY/split)
  
  CurrStartX <- 0
  CurrStartY <- 0
  CurrEndX <- dx
  CurrEndY <- dy
  while (CurrStartX < maxValueX) {
    
    temp <- df %>% filter(Days_on_PKT >= CurrStartX) %>% filter(Days_on_PKT <= CurrEndX)
    
    while (CurrStartY < maxValueY) {
      
      temp2 <- temp %>% filter(Blood_ketones_mmol_per_L >= CurrStartY) %>% filter(Blood_ketones_mmol_per_L <= CurrEndY)
      
      plot <-  ggplot(temp2, aes(x=Days_on_PKT, y=Blood_ketones_mmol_per_L,color=Patient)) +
        ggtitle(paste("(LINE GRAPH) Days on PKT:", CurrStartX, "-", CurrEndX," Ketones:", CurrStartY, "-", CurrEndY, sep="")) +
        geom_line() +
        geom_point() +
        xlab("Days on PKT") +
        ylab("Ketones (mmol/L)") +
        theme(legend.position="bottom") +
        guides(fill=guide_legend(nrow=5, byrow=TRUE))
      plot(plot)
      
      CurrStartY <- CurrEndY
      CurrEndY <- CurrStartY + dy
    }
    
    CurrStartY <- 0
    CurrEndY <- dy
    CurrStartX <- CurrEndX
    CurrEndX <- CurrStartX + dx
    
  }
  
  plot <-  ggplot(df) +
    ggtitle("All Days on PKT (Line):") +
    geom_line(mapping=aes(x=Days_on_PKT, y=Blood_ketones_mmol_per_L,color=Patient)) +
    geom_point(mapping=aes(x=Days_on_PKT, y=Blood_ketones_mmol_per_L,color=Patient)) +
    theme(legend.position="none") +
    xlab("Days on PKT") +
    ylab("Ketones (mmol/L)")
  plot(plot)
  
  plot2 <-  ggplot(df) +
    ggtitle("All Days on PKT (Point):") +
    geom_point(mapping=aes(x=Days_on_PKT, y=Blood_ketones_mmol_per_L,color=Patient)) +
    theme(legend.position="none") +
    xlab("Days on PKT") +
    ylab("Ketones (mmol/L)")
  plot(plot2)
  
  dev.off()
}

#Name: Glucose VS Time
#Goal: Plot Glucose vs time for data population.
#Summary: Function uses data to plot Glucose data
#Parameters:
# df = Data that contains individual's data on Ketones and Glucose.
#Return value: Glucose VS Time plots for population.
GvT_pop <- function(df, split) {
  
  pdf("GlucoseVTime_population.pdf")
  
  maxValueX <- max(df$Days_on_PKT, na.rm = TRUE)
  maxValueY <- max(df$Blood_glucose_mg_per_dL, na.rm = TRUE)
  
  dx <- ceiling(maxValueX/split)
  dy <- ceiling(maxValueY/split)
  
  CurrStartX <- 0
  CurrStartY <- 0
  CurrEndX <- dx
  CurrEndY <- dy
  while (CurrStartX < maxValueX) {
    
    temp <- df %>% filter(Days_on_PKT >= CurrStartX) %>% filter(Days_on_PKT <= CurrEndX)
  
    while(CurrStartY < maxValueY) {
      
      temp2 <- temp %>% filter(Blood_glucose_mg_per_dL >= CurrStartY) %>% filter(Blood_glucose_mg_per_dL <= CurrEndY)
      
      plot <-  ggplot(temp2) +
        ggtitle(paste("(LINE GRAPH) Days on PKT:", CurrStartX, "-", CurrEndX," Glucose:", CurrStartY, "-", CurrEndY, sep="")) +
        geom_line(mapping=aes(x=Days_on_PKT, y=Blood_glucose_mg_per_dL,color=Patient)) +
        geom_point(mapping=aes(x=Days_on_PKT, y=Blood_glucose_mg_per_dL,color=Patient)) +
        xlab("Days on PKT") +
        ylab("Glucose (mg/dL)") +
        theme(legend.position="bottom") +
        guides(fill=guide_legend(nrow=5, byrow=TRUE))
      
      plot(plot)
      
      CurrStartY <- CurrEndY
      CurrEndY <- CurrStartY + dy
    }
    
    CurrStartY <- 0
    CurrEndY <- dy
    CurrStartX <- CurrEndX
    CurrEndX <- CurrStartX + dx
  }
  
  plot <-  ggplot(df) +
    ggtitle("All Days on PKT (Line):") +
    geom_line(mapping=aes(x=Days_on_PKT, y=Blood_glucose_mg_per_dL,color=Patient)) +
    geom_point(mapping=aes(x=Days_on_PKT, y=Blood_glucose_mg_per_dL,color=Patient)) +
    theme(legend.position="none") +
    xlab("Days on PKT") +
    ylab("Glucose (mg/dL)")
  plot(plot)
  
  plot2 <-  ggplot(df) +
    ggtitle("All Days on PKT (Point):") +
    geom_point(mapping=aes(x=Days_on_PKT, y=Blood_glucose_mg_per_dL,color=Patient)) +
    theme(legend.position="none") +
    xlab("Days on PKT") +
    ylab("Glucose (mg/dL)")
  plot(plot2)
  
  dev.off()
}

#Name: Ketones VS Glucose
#Goal: Plot Ketones vs Glucose for data population.
#Summary: Function uses data to plot ketones VS Glucose. The data is filtered
# so that each value for ketones has a glucose value as well. One plot connects
# individual data points while the other just makes the points (just points, no lines).
#Parameters:
# df = Data that contains individual's data on Ketones and Glucose.
#Return value: Two Ketones VS Glucose plots for population.
KvG_pop <- function(df, split) {
  
  pdf("KetonesVGlucose_population.pdf")
  
  df <- df %>% 
    filter(!is.na(Blood_ketones_mmol_per_L)) %>%
    filter(!is.na(Blood_glucose_mg_per_dL))
  
  maxValueX <- max(df$Blood_ketones_mmol_per_L, na.rm = TRUE)
  maxValueY <- max(df$Blood_glucose_mg_per_dL, na.rm = TRUE)
  
  dx <- ceiling(maxValueX/split)
  dy <- ceiling(maxValueY/split)
  
  CurrStartX <- 0
  CurrStartY <- 0
  CurrEndX <- dx
  CurrEndY <- dy
  while (CurrStartX < maxValueX) {
    
    temp <- df %>% filter(Blood_ketones_mmol_per_L >= CurrStartX) %>% filter(Blood_ketones_mmol_per_L <= CurrEndX)
    
    while (CurrStartY < maxValueY) {

      temp2 <- temp %>% filter(Blood_glucose_mg_per_dL >= CurrStartY) %>% filter(Blood_glucose_mg_per_dL <= CurrEndY)
      
      plot <-  ggplot(temp2) +
        ggtitle(paste("(LINE GRAPH) Ketones:", CurrStartX, "-", CurrEndX," Glucose:", CurrStartY, "-", CurrEndY, sep="")) +
        geom_line(mapping=aes(x=Blood_ketones_mmol_per_L, y=Blood_glucose_mg_per_dL,color=Patient)) +
        geom_point(mapping=aes(x=Blood_ketones_mmol_per_L, y=Blood_glucose_mg_per_dL,color=Patient)) +
        xlab("Ketones (mmol/L)") +
        ylab("Glucose (mg/dL)") +
        theme(legend.position="bottom") +
        guides(fill=guide_legend(nrow=5, byrow=TRUE))
      plot(plot)
      
      # NOTE: Commented out for testing
      # plot2 <-  ggplot(temp) +
      #   ggtitle(paste("Ketones (Point):", CurrStart, "-", CurrEnd)) +
      #   geom_point(mapping=aes(x=Blood_ketones_mmol_per_L, y=Blood_glucose_mg_per_dL,color=Patient)) +
      #   theme(legend.position="none") + 
      #   xlab("Ketones (mmol/L)") +
      #   ylab("Glucose (mg/dL)")
      # plot(plot2)
      
      CurrStartY <- CurrEndY
      CurrEndY <- CurrStartY + dy
    }
    
    CurrStartY <- 0
    CurrEndY <- dy
    CurrStartX <- CurrEndX
    CurrEndX <- CurrStartX + dx
  }
  
  plot <-  ggplot(df) +
    ggtitle("All Days on PKT (Line):") +
    geom_line(mapping=aes(x=Blood_ketones_mmol_per_L, y=Blood_glucose_mg_per_dL,color=Patient)) +
    geom_point(mapping=aes(x=Blood_ketones_mmol_per_L, y=Blood_glucose_mg_per_dL,color=Patient)) +
    theme(legend.position="none") +
    xlab("Ketones (mmol/L)") +
    ylab("Glucose (mg/dL)")
  plot(plot)
  
  plot2 <-  ggplot(df) +
    ggtitle("All Days on PKT (Point):") +
    geom_point(mapping=aes(x=Blood_ketones_mmol_per_L, y=Blood_glucose_mg_per_dL,color=Patient)) +
    theme(legend.position="none") + 
    xlab("Ketones (mmol/L)") +
    ylab("Glucose (mg/dL)")
  plot(plot2)
  
  dev.off()
}

# Main function for user interaction
main <- function() {
  
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
  library(readxl)
  library(dplyr)
  
  mdirectory <- "OMITTED"
  setwd(mdirectory)

  KG <- read_excel("Glucose project patient database.xlsx", sheet="OMITTED", col_types = c("text","skip","skip","skip","numeric","skip","numeric","numeric","skip","skip","skip"))
  
  end <- F
  while(!end) {
    cat("\nWhich plots would you like?\n[1] Ketones & Glucose VS Time\n[2] Ketones VS Glucose \n")
    input <- readline(prompt="Enter the Menu Number here: ")
    
    cat("\nIn which directory would you like to place these plots?\n")
    dir <- readline(prompt="Enter the directory location here: ")
    setwd(dir)
    
    cat("\nHow would you like to split the population graphs?\n")
    split <- readline(prompt="Enter integer value here(1 to 20): ")
    split <- as.integer(split)
    
    if(split < 1 || split > 20 || !is.integer(split)) {
      cat("\n\nInvalid integer value... Please Try again.\n\n")
    }
    else {
      if(input == 1) {
        cat("\nCreating plots... Please wait...\n")
        KGvT_individual(KG)
        GvT_pop(KG, split)
        KvT_pop(KG, split)
        cat("\nPlots Complete!\n")
        end <- T
      }
      else if(input == 2) {
        cat("\nCreating plots... Please wait...\n")
        KvG_individual(KG)
        KvG_pop(KG, split)
        cat("\nPlots Complete!\n")
        end <- T
      }
      else {
        cat("\n\nInvalid Menu Number... Please Try again.\n\n")
      }
    }
  }
}

main()