#Rversion: 3.4.1
#Purpose: Categorize patient data into groups.
#Result: Categorize and display different sample groups.

plotData <- function(df1,df2,names, group) {
  filename <- paste(group,"graph.pdf", sep= "")
  
  #pdf(file = filename)
  
  df1_Data <- df1 %>% count(Day_on_PKT)
  df2_Data <- df2 %>% count(Day_on_PKT)
  
  df1_filter <- left_join(names, df1, by=c("Patient_Name"="Patient_Name"))
  df2_filter <- left_join(names, df2, by=c("Patient_Name"="Patient_Name"))
  both_Data <- full_join(df1_filter,df2_filter)
  both_Data <- both_Data %>% count(Day_on_PKT)
  
  plot <- ggplot(df1_Data,aes(Day_on_PKT, n, label = Day_on_PKT)) + 
    geom_bar(stat = "identity") + 
    geom_text(size = 2, vjust = 0.4, hjust = -0.3, angle = 90, color= "red", fontface = "bold", check_overlap = T) +
    ggtitle(paste(group,"_Blood",sep="")) +
    xlab("Day on PKT") + 
    ylab("# of Samples")
  plot(plot)
  
  plot <- ggplot(df2_Data,aes(Day_on_PKT, n, label = Day_on_PKT)) + 
    geom_bar(stat = "identity") + 
    geom_text(size = 2, vjust = 0.4, hjust = -0.3, angle = 90, color= "red", fontface = "bold") +
    ggtitle(paste(group,"_Stool",sep="")) +
    xlab("Day on PKT") + 
    ylab("# of Samples")
  plot(plot)
  
  plot <- ggplot(both_Data,aes(Day_on_PKT, n, label = Day_on_PKT)) + 
    geom_bar(stat = "identity") + 
    geom_text(size = 2, vjust = 0.4, hjust = -0.3, angle = 90, color= "red", fontface = "bold") +
    ggtitle(paste(group,"_Both",sep="")) +
    xlab("Day on PKT") + 
    ylab("# of Samples")
  plot(plot)
  
  #dev.off()
}

writeToExcel <- function(df, group) {
  df <- as.data.frame(df)
  write.xlsx2(df, "SampleGroups.xlsx", sheetName = group,col.names = T,row.names = F,append = T)
}

SG1_f <- function(blood, stool) {
  
  B_base <- filter(blood,as.numeric(Day_on_PKT) == 0)
  S_base <- filter(stool,as.numeric(Day_on_PKT) == 0)
  
  S_patients <- S_base %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_base %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG1:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_base, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)

  S_combined <- select(S_base, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG1")
  
  writeToExcel(collect,"SG1")
}

SG2_f <- function(blood, stool) {

  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 120)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) != 0)
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 120)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) != 0)
  
  S_patients <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG2:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG2")
  
  #writeToExcel(collect,"SG2")
}

SG3_f <- function(blood, stool) {
  
  B_base <- filter(blood,as.numeric(Day_on_PKT) == 0)
  S_base <- filter(stool,as.numeric(Day_on_PKT) == 0)
  
  B_combined <- select(B_base, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  S_combined <- select(S_base, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 120)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) != 0)
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 120)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) != 0)
  
  S_base <- S_base %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_base <- B_base %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  S_PKT <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_PKT <- B_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- semi_join(B_base, B_PKT, by= c("Patient_Name" = "Patient_Name"))
  B_patients <- B_patients %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  S_patients <- semi_join(S_base,S_PKT, by=c("Patient_Name" = "Patient_Name"))
  S_patients <- S_patients %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG3:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG3")
  
  writeToExcel(collect,"SG3")
}

SG4_f <- function(blood, stool) {
  
  B_base <- filter(blood,as.numeric(Day_on_PKT) == 0)
  S_base <- filter(stool,as.numeric(Day_on_PKT) == 0)
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 120)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) != 0)
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 120)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) != 0)
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_base <- S_base %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_base <- B_base %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  S_PKT <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_PKT <- B_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- semi_join(B_base, B_PKT, by= c("Patient_Name" = "Patient_Name"))
  B_patients <- B_patients %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  S_patients <- semi_join(S_base,S_PKT, by=c("Patient_Name" = "Patient_Name"))
  S_patients <- S_patients %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG4:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG4")
  
  writeToExcel(collect,"SG4")
}

SG5_f <- function(blood, stool) {
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 365)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) >= 22)
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 365)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) >= 22)
  
  S_patients <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG5:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG5")
  
  writeToExcel(collect,"SG5")
}

SG6_f <- function(blood, stool) {
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 365)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) >= 22)
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 365)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) >= 22)
  
  B_2OM <- B_PKT %>% count(Patient_Name) %>% filter(n > 1)
  S_2OM <- S_PKT %>% count(Patient_Name) %>% filter(n > 1)
  
  S_patients <- S_2OM %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_2OM %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG6:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG6")
  
  writeToExcel(collect,"SG6")
}

SG7_f <- function(blood, stool) {
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 730)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) >= 22)
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 730)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) >= 22)
  
  S_patients <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG7:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG7")
  
  writeToExcel(collect,"SG7")
}

SG8_f <- function(blood, stool) {
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 730)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) >= 22)
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 730)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) >= 22)
  
  B_2OM <- B_PKT %>% count(Patient_Name) %>% filter(n > 1)
  S_2OM <- S_PKT %>% count(Patient_Name) %>% filter(n > 1)
  
  S_patients <- S_2OM %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_2OM %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG8:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG8")
  
  writeToExcel(collect,"SG8")
}

SG9_f <- function(blood, stool) {
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 1460)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) >= 22)
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 1460)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) >= 22)
  
  S_patients <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG9:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG9")
  
  writeToExcel(collect,"SG9")
}

SG10_f <- function(blood, stool) {
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 1460)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) >= 22)
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 1460)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) >= 22)
  
  B_2OM <- B_PKT %>% count(Patient_Name) %>% filter(n > 1)
  S_2OM <- S_PKT %>% count(Patient_Name) %>% filter(n > 1)
  
  S_patients <- S_2OM %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_2OM %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG10:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG10")
  
  writeToExcel(collect,"SG10")
}

SG11_f <- function(blood, stool) {
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 2190)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) >= 22)
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 2190)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) >= 22)
  
  S_patients <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG11:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG11")
  
  writeToExcel(collect,"SG11")
}

SG12_f <- function(blood, stool) {
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 2190)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) >= 22)
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 2190)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) >= 22)
  
  B_2OM <- B_PKT %>% count(Patient_Name) %>% filter(n > 1)
  S_2OM <- S_PKT %>% count(Patient_Name) %>% filter(n > 1)
  
  S_patients <- S_2OM %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_2OM %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG12:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG12")
  
  writeToExcel(collect,"SG12")
}

SG13_f <- function(blood, stool) {
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 2920)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) >= 22)
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 2920)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) >= 22)
  
  S_patients <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG13:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG13")
  
  writeToExcel(collect,"SG13")
}

SG14_f <- function(blood, stool) {
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 2920)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) >= 22)
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 2920)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) >= 22)
  
  B_2OM <- B_PKT %>% count(Patient_Name) %>% filter(n > 1)
  S_2OM <- S_PKT %>% count(Patient_Name) %>% filter(n > 1)
  
  S_patients <- S_2OM %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_2OM %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG14:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG14")
  
  writeToExcel(collect,"SG14")
}

SG15_f <- function(blood, stool) {
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 3650)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) >= 22)
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 3650)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) >= 22)
  
  S_patients <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG15:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG15")
  
  writeToExcel(collect,"SG15")
}

SG16_f <- function(blood, stool) {
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 3650)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) >= 22)
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 3650)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) >= 22)
  
  B_2OM <- B_PKT %>% count(Patient_Name) %>% filter(n > 1)
  S_2OM <- S_PKT %>% count(Patient_Name) %>% filter(n > 1)
  
  S_patients <- S_2OM %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_2OM %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG16:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG16")
  
  writeToExcel(collect,"SG16")
}

SG17_f <- function(blood, stool) {
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 5475)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) >= 22)
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 5475)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) >= 22)
  
  S_patients <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG17:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG17")
  
  writeToExcel(collect,"SG17")
}

SG18_f <- function(blood, stool) {
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 5475)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) >= 22)
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 5475)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) >= 22)
  
  B_2OM <- B_PKT %>% count(Patient_Name) %>% filter(n > 1)
  S_2OM <- S_PKT %>% count(Patient_Name) %>% filter(n > 1)
  
  S_patients <- S_2OM %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_2OM %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG18:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG18")
  
  writeToExcel(collect,"SG18")
}

SG19_f <- function(blood, stool) {
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 7300)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) >= 22)
  
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 7300)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) >= 22)
  
  S_patients <- S_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_PKT %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG19:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG19")
  
  writeToExcel(collect,"SG19")
}

SG20_f <- function(blood, stool) {
  
  B_PKT <- filter(blood, as.numeric(Day_on_PKT) <= 7300)
  B_PKT <- filter(B_PKT, as.numeric(Day_on_PKT) >= 22)
  S_PKT <- filter(stool, as.numeric(Day_on_PKT) <= 7300)
  S_PKT <- filter(S_PKT, as.numeric(Day_on_PKT) >= 22)
  
  B_2OM <- B_PKT %>% count(Patient_Name) %>% filter(n > 1)
  S_2OM <- S_PKT %>% count(Patient_Name) %>% filter(n > 1)
  
  S_patients <- S_2OM %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  B_patients <- B_2OM %>%
    select(Patient_Name) %>%
    distinct(Patient_Name)
  
  both <- semi_join(B_patients,S_patients,by=c("Patient_Name"="Patient_Name"))
  
  cat("SG20:\n\tMicro:",nrow(S_patients),"\n\tMetab:",nrow(B_patients),"\n\tM&M:", nrow(both),"\n")
  
  B_combined <- select(B_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  S_combined <- select(S_PKT, Sample_ID, Patient_Name, Day_on_PKT, Sample_Type, Location_within_Freezer, Location_within_Rack)
  
  collect <- full_join(B_combined, S_combined)
  
  plotData(B_combined,S_combined,both, "SG20")
  
  writeToExcel(collect,"SG20")
}

dataGroups <- function() {
  
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
  blood_S_all <- read_excel("GCRC Blood Analysis Patient Records.xlsm",sheet = "OMITTED", col_types=c("text","text","text","date","numeric","text","text","date","text","date","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"))
  stool_S_all <- read_excel("GCRC Stool Analysis Patient Records.xlsm",sheet = "OMITTED", col_types=c("text","text","text","date","numeric","text","text","date","text","date","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"))

  print("Please enter the directory you would like the data to be placed.")
  directory <- readline(prompt="Enter here: ")
  setwd(directory)

  if(file.exists("SampleGroups.xlsx")) {
    file.remove("SampleGroups.xlsx")
  }
  
  SG1_f(blood_S_all,stool_S_all)
  SG2_f(blood_S_all,stool_S_all)
  SG3_f(blood_S_all,stool_S_all)
  SG4_f(blood_S_all,stool_S_all)
  SG5_f(blood_S_all,stool_S_all)
  SG6_f(blood_S_all,stool_S_all)
  SG7_f(blood_S_all,stool_S_all)
  SG8_f(blood_S_all,stool_S_all)
  SG9_f(blood_S_all,stool_S_all)
  SG10_f(blood_S_all,stool_S_all)
  SG11_f(blood_S_all,stool_S_all)
  SG12_f(blood_S_all,stool_S_all)
  SG13_f(blood_S_all,stool_S_all)
  SG14_f(blood_S_all,stool_S_all)
  SG15_f(blood_S_all,stool_S_all)
  SG16_f(blood_S_all,stool_S_all)
  SG17_f(blood_S_all,stool_S_all)
  SG18_f(blood_S_all,stool_S_all)
  SG19_f(blood_S_all,stool_S_all)
  SG20_f(blood_S_all,stool_S_all)
  
  cat("Samples groups have been stored.\nData can be found in:\n", directory)
}

dataGroups()