#Rversion: 3.4.1
#Purpose: Display Ketones and Glucose data for individual and population.
#Result: Plots created from data and can be placed in files.

#------------------------------Plot Functions------------------------------

#Name: Ketones & Glucose VS Time
#Goal: Plot both Ketones and Glucose vs time for each individual.
#Summary: Function uses data to plot Ketones and Glucose for individuals.
#Parameters:
# df = Data that contains individual's data on Ketones and Glucose.
# ind = name or ID of the individual.
#Return value: Ketones & Glucose VS Time plots for individuals.
KGvT_individual <- function(df, ind) {
  
  df <- filter(df, Patient == ind)
  
  plot <-  ggplot(df) +
    ggtitle(ind) +
    geom_line(mapping=aes(x=Days_on_PKT, y=Blood_glucose_mg_per_dL, color="red")) +
    geom_point(mapping=aes(x=Days_on_PKT, y=Blood_glucose_mg_per_dL, color="red")) +
    geom_line(mapping=aes(x=Days_on_PKT, y=Blood_ketones_mmol_per_L, color="blue")) +
    geom_point(mapping=aes(x=Days_on_PKT, y=Blood_ketones_mmol_per_L, color="blue")) +
    scale_color_identity(name="Type",
                         breaks=c("red","blue"),
                         labels=c("Glucose (mg/dL)", "Ketones (mmol/L)"),
                         guide="legend") +
    xlab("Days on PKT") +
    ylab("Values")
  
  #TODO: The labels on the legend do not say 'Glucose' and 'Ketones'
  # as intended. However, hovering over the points will show the correct details.
  
  return(plot)
}

#Name: Ketones VS Glucose
#Goal: Plot Ketones vs Glucose for each individual.
#Summary: Function uses data provided to plot Ketones VS Glucose. The data is filtered
# so that each value for ketones has a glucose value as well.
#Parameters:
# df = Data that contains individual's data on Ketones and Glucose.
# ind = name or ID of the individual.
#Return value: Two Ketones VS Glucose plots for population.
KvG_individual <- function(df, ind) {
  
  df <- df %>% 
    filter(!is.na(Blood_glucose_mg_per_dL)) %>% 
    filter(!is.na(Blood_ketones_mmol_per_L)) %>%
    filter(Patient == ind)
    
  plot <-  ggplot(df, aes(x=Blood_ketones_mmol_per_L, y=Blood_glucose_mg_per_dL)) +
    ggtitle(ind) +
    geom_line() +
    geom_point() +
    xlab("Ketones (mmol/L)") +
    ylab("Glucose (mg/dL)")
  
  return(plot)
}

#Name: Ketones VS Time
#Goal: Plot Ketones vs time for data population.
#Summary: Function uses data to plot Ketones data
#Parameters:
# df = Data that contains individual's data on Ketones and Glucose.
#Return value: Ketones VS Time plots for population.
KvT_pop <- function(df) {
  
  plot <-  ggplot(df, aes(x=Days_on_PKT, y=Blood_ketones_mmol_per_L, color=Patient)) +
    ggtitle("Ketones V Time") +
    geom_line() +
    geom_point() +
    xlab("Days on PKT") +
    ylab("Ketones (mmol/L)")

  return(plot)
}

#Name: Glucose VS Time
#Goal: Plot Glucose vs time for data population.
#Summary: Function uses data to plot Glucose data
#Parameters:
# df = Data that contains individual's data on Ketones and Glucose.
#Return value: Glucose VS Time plots for population.
GvT_pop <- function(df) {
  plot <-  ggplot(df, aes(x=Days_on_PKT, y=Blood_glucose_mg_per_dL, color=Patient)) +
    ggtitle("Glucose V Time") +
    geom_line() +
    geom_point() +
    xlab("Days on PKT") +
    ylab("Glucose (mg/dL)")
  
  return(plot)
}

#Name: Ketones VS Glucose
#Goal: Plot Ketones vs Glucose for data population.
#Summary: Function uses data to plot ketones VS Glucose. The data is filtered
# so that each value for ketones has a glucose value as well. One plot connects
# individual data points while the other just makes the points (just points, no lines).
#Parameters:
# df = Data that contains individual's data on Ketones and Glucose.
#Return value: Two Ketones VS Glucose plots for population.
KvG_pop <- function(df) {
  
  df <- df %>% 
    filter(!is.na(Blood_ketones_mmol_per_L)) %>%
    filter(!is.na(Blood_glucose_mg_per_dL))
  
  plot <-  ggplot(df) +
    ggtitle("Ketones V Glucose") +
    geom_line(mapping=aes(x=Blood_ketones_mmol_per_L, y=Blood_glucose_mg_per_dL,color=Patient)) +
    geom_point(mapping=aes(x=Blood_ketones_mmol_per_L, y=Blood_glucose_mg_per_dL,color=Patient)) +
    xlab("Ketones (mmol/L)") +
    ylab("Glucose (mg/dL)")
  
  return(plot)
  
}

#------------------------------User Prompts------------------------------

chooseInd <- function(df) {
  
  end <- F
  while(!end) {
    cat("\nWhich individual plot from the choices below would you like to see?
        [1]Ketones AND Glucose VERSUS Time
        [2]Ketones VERSUS Glucose\n")
    type <- readline(prompt="Enter the Menu Number here: ")
    
    if(type == 1 || type == 2) {
      end <- T
    }
  }
  
  end <- F
  while(!end) {
    cat("\nWhat is the name/ID of the individual?\n")
    who <- readline(prompt="Enter here: ")
    
    cat("\nYou choose", who, "\n is this correct?")
    check <- readline(prompt="(Y or N): ")
    
    if(check == "Y" || check == "y") {
      end <- T
    }
  }
  
  #Variables used from prompts: who, type
  if(type == 1) {
    plot <- KGvT_individual(df, who)
  }
  else { # type is 2
    plot <- KvG_individual(df, who)
  }
  
  return(plot) 
}

choosePop <- function(df) {
  end <- F
  while(!end) {
    cat("\nWhich population plot from the choices below would you like to see?
        [1]Ketones VERSUS Time
        [2]Glucose VERSUS Time
        [3]Ketones VERSUS Glucose\n")
    type <- readline(prompt="Enter the Menu Number here: ")
    
    if(type == 1) {
      plot <- KvT_pop(df)
      end <- T
    }
    else if(type == 2) {
      plot <- GvT_pop(df)
      end <- T
    }
    else if(type == 3) {
      plot <- KvG_pop(df)
      end <- T
    }
  }
  
  return(plot)
}

# Main function for user interaction
createPlot <- function() {
  
  if (!require("readxl")) {
    install.packages("readxl")
  }
  if (!require("dplyr")) {
    install.packages("dplyr")
  }
  if (!require("plotly")) {
    install.packages("plotly")
  }
  if (!require("ggplot2")) {
    install.packages("ggplot2")
  }
  library(ggplot2)
  library(plotly)
  library(readxl)
  library(dplyr)
  
  mdirectory <- "OMITTED"
  setwd(mdirectory)
  
  KG <- read_excel("Glucose project patient database.xlsx", sheet="OMITTED", col_types = c("text","skip","skip","skip","numeric","skip","numeric","numeric","skip","skip","skip"))
  
  cat("\nWelcome to the Ketones and Glucose graph viewer.\n")
  
  end <- F
  while(!end) {
    
    cat("\nChoose which type of graph you would like to view:
        [1] Individual
        [2] Population\n")
    input <- readline(prompt="Enter the Menu Number here:")
    
    if(input == 1) {
      plot <- chooseInd(KG)
      end <- T
    }
    else if(input == 2) {
      plot <- choosePop(KG)
      end <- T
    }
    else {
      cat("\n\nInvalid Menu Number... Please Try again.\n\n")
    }
  }
  
  cat("Creating Plot...")
  plot <- ggplotly(plot)
  return(plot)
}


#------------------------------Main------------------------------

# Plotly only seems to work from this point so we will
# return the ggplot here from createPlot and plot the graph 
# to the Viewer panel.
# Currently only able to return a single plot due to this.

KG <- createPlot()
ggplotly(KG)
