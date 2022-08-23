#Rversion: 3.4.1
#Purpose: Display Ketones and Glucose data for individual and population.
#Result: Plots created from data and can be placed in files.


#Name: Ketones & Glucose VS Time
#Goal: Plot both Ketones and Glucose vs time for each individual.
#Summary: Function uses data to plot Ketones and Glucose for individuals.
#Parameters:
# df = Data that contains individual's data on Ketones and Glucose.
#Return value: Ketones & Glucose VS Time plots for each individual.
KGvT_individual <- function(df) {
  
  mdir <- getwd()
  dir.create(file.path(mdir, "__KGVT_individual"), showWarnings = FALSE)
  
  setwd(file.path(mdir, "__KGVT_individual"))
  
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
      
   
    plot <- ggplotly(plot) #converts ggplot to plotly
    htmlwidgets::saveWidget(as_widget(plot), paste(i, ".html", sep="")) #saves plotly in current working directory.
  }
  
  setwd(mdir)
}

#Name: Ketones VS Glucose
#Goal: Plot Ketones vs Glucose for each individual.
#Summary: Function uses data provided to plot Ketones VS Glucose. The data is filtered
# so that each value for ketones has a glucose value as well.
#Parameters:
# df = Data that contains individual's data on Ketones and Glucose.
#Return value: Ketones VS Glucose plots for each individual.
KvG_individual <- function(df) {
  
  mdir <- getwd()
  dir.create(file.path(mdir, "__KVG_individual"), showWarnings = FALSE)
  setwd(file.path(mdir, "__KVG_individual"))
  
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
    
    plot <- ggplotly(plot)
    htmlwidgets::saveWidget(as_widget(plot), paste(i, ".html", sep=""))
  }
  
  setwd(mdir)
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
  
  plot <- ggplotly(plot)
  htmlwidgets::saveWidget(as_widget(plot), paste("KVT_population.html", sep=""))
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
  
  plot <- ggplotly(plot)
  htmlwidgets::saveWidget(as_widget(plot), paste("GVT_population.html", sep=""))
}

#Name: Ketones VS Glucose
#Goal: Plot Ketones vs Glucose for data population.
#Summary: Function uses data to plot ketones VS Glucose. The data is filtered
# so that each value for ketones has a glucose value as well. One plot connects
# individual data points while the other just makes the points (just points, no lines).
#Parameters:
# df = Data that contains individual's data on Ketones and Glucose.
#Return value: Ketones VS Glucose plots for population.
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
  
  plot <- ggplotly(plot)
  htmlwidgets::saveWidget(as_widget(plot), paste("KVG_population.html", sep=""))
}

# Main function for user interaction
main <- function() {
  
  #if library is not installed... install it!
  if (!require("readxl")) {
    install.packages("readxl")
  }
  if (!require("dplyr")) {
    install.packages("dplyr")
  }
  if (!require("plotly")) {
    install.packages("plotly")
  }
  #use the library
  library(plotly)
  library(readxl)
  library(dplyr)
  
  mdirectory <- "OMITTED"
  setwd(mdirectory)
  
  # KG reads excel... col_types is not always necessary to read databases.
  KG <- read_excel("Glucose project patient database.xlsx", sheet="OMITTED", col_types = c("text","skip","skip","skip","numeric","skip","numeric","numeric","skip","skip","skip"))
  
  if("__plot"%in%dir()==FALSE)  {
    dir.create(file.path(getwd(), "__plot"), showWarnings = FALSE)
  }
  setwd(file.path(getwd(), "__plot"))
  
  end <- F
  while(!end) {
    cat("\nWhich plots would you like?\n[1] Ketones & Glucose VS Time\n[2] Ketones VS Glucose \n")
    input <- readline(prompt="Enter the Menu Number here: ")

    if(input == 1) {
      cat("\nCreating plots... Please wait...\n")
      KGvT_individual(KG)
      GvT_pop(KG)
      KvT_pop(KG)
      cat("\nPlots Complete!\n")
      end <- T
    }
    else if(input == 2) {
      cat("\nCreating plots... Please wait...\n")
      KvG_individual(KG)
      KvG_pop(KG)
      cat("\nPlots Complete!\n")
      end <- T
    }
    else {
      cat("\n\nInvalid Menu Number... Please Try again.\n\n")
    }
  }
}

main()