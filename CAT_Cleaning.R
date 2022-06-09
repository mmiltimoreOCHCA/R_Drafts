# CAT Cleaning
# Matthew Miltimore
# Updated: 5/10/2022

### Start-up ###
# Must also update working directory to folder which holds current load
# Must manually update name of file with most current data
{
  # Clear workspace
  if(!is.null(dev.list())) dev.off
  cat("\014")
  rm(list = ls())
  
  # Packages
  # If these packages are not yet on your the system, must run install.packages
  library(dplyr)
  library(lubridate)
  
  # Load Data
  setwd('/Users/Matthew Miltimore/Documents/CAT') ### <- CHANGE DIRECTORY HERE ###
  
  df <- read.csv("cat_dispatch 5-1-22.csv") ### <- CHANGE DATA HERE (must be .csv) ###
  
  # Remove rows where all values are N/A
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  
}

### Cleaning ###
{
# Select only Child, Adult, and PERT teams
df <- subset(df, df$Cat_team == "Child" | 
               df$Cat_team == "Adult" |
               df$Cat_team == "PERT")

# remove NAs from Request Date
df$Request_date <- as.character(df$Request_date)  

df <- subset(df, df$Request_date != "")

# select only necessary variables
names(df)[1] <- "Id"

df <- df %>%
  select(Id,
         Case,
         Request_date, 
         Date_of_birth, 
         Gender, 
         Family_language, 
         English_proficiency, 
         Cat_team, 
         Substance_abuse_today, 
         Fever_chills_aches, 
         Cough, 
         Shortness_of_breath, 
         Weapons_yn, 
         Substance_abuse_yn, 
         Call_dispatch_end_time, 
         City, 
         Zip, 
         Ehr_found, 
         Cat_contacted_date_time,
         Cat_dispatched_date_time,
         Cat_arrival_date_time,
         Cat_assessment_start_date_time, 
         Cat_assessment_end_date_time, 
         Cat_cleared_date_time, 
         Disposition, 
         Dispatch_status, 
         Covid_19_ppl)

}

### Convert step time strings to date time variables (POSIXct) ###
{
  
  # list of step variable names to convert
  stepvar_vec <- c("Request_date", 
                   "Call_dispatch_end_time",
                   "Cat_contacted_date_time",
                   "Cat_dispatched_date_time", 
                   "Cat_arrival_date_time",
                   "Cat_assessment_start_date_time",
                   "Cat_assessment_end_date_time",
                   "Cat_cleared_date_time")
  
  stepvarnum_vec <- match(stepvar_vec, names(df))
  
  # loop
  for (i in stepvarnum_vec) {
    
    
    if (i != stepvarnum_vec[1]) {
      
      df[[i]] <- as.character(df[[i]])
      
    } 
    
    df[[i]] <- as.POSIXct(paste0(substr(df[[i]], 7, 10),
                                 "/",
                                 substr(df[[i]], 1, 5),
                                 substr(df[[i]], 11, nchar(df[[i]]))), 
                          format = "%Y/%m/%d %I:%M %p")
    
  }
  
}

### Time Step Variables ###
{
  
  # Loop
  for (i in 1:9) {
    
    if (between(i, 1, 2)) {
      
      df[[ncol(df)+1]] <- as.numeric(difftime(df[[stepvarnum_vec[i+1]]],
                                              df[[stepvarnum_vec[i]]],
                                              units = "mins"))
      
    } else if (i == 3) {
      # Request to Assign
      df[[ncol(df)+1]] <- as.numeric(difftime(df[[stepvarnum_vec[3]]],
                                              df[[stepvarnum_vec[1]]],
                                              units = "mins"))
      
    } else if (between(i, 4, 8)) {
      
      df[[ncol(df)+1]] <- as.numeric(difftime(df[[stepvarnum_vec[i]]],
                                              df[[stepvarnum_vec[i-1]]],
                                              units = "mins"))
      
    } else if (i == 9) {
      # Total Time
      df[[ncol(df)+1]] <- as.numeric(difftime(df[[stepvarnum_vec[8]]],
                                              df[[stepvarnum_vec[1]]],
                                              units = "mins"))
      
    }
    
  }
  
  # Name the variables
  ncol(df)-8
  
  names(df)[(ncol(df)-8):ncol(df)] <- c("Req_CallEnd",
                                        "CallEnd_Assign",
                                        "Req_Assign",
                                        "Assign_Disp",
                                        "Disp_Arvl",
                                        "Arvl_AsStr",
                                        "AsStr_AsEnd",
                                        "End_Clear",
                                        "TotalTime")
  
}

### Outlier
{
  
  str_num <- match("Req_CallEnd", names(df)) # first time step var
  end_num <- match("TotalTime", names(df)) # last time step var
  
  
  for (i in str_num:end_num) {
    
    # Make and name empty variable
    df[[ncol(df)+1]] <- rep(NA, nrow(df))
    
    names(df)[ncol(df)] <- paste0("OUT_",
                                  names(df)[i])
    
    # Check if outlier
    avg <- mean(df[[i]], na.rm = TRUE)
    
    std <- sd(df[[i]], na.rm = TRUE)
    
    df[[ncol(df)]] <- replace(df[[ncol(df)]],
                              !is.na(abs((df[[i]]-avg)/(std))),
                              FALSE)
    
    df[[ncol(df)]] <- replace(df[[ncol(df)]], 
                              abs((df[[i]]-avg)/(std)) > 3,
                              TRUE)
    
  }
  
}

### Greater than Total Time boolean
{
  
  str_num <- match("Req_CallEnd", names(df)) # first time step var
  end_num <- match("End_Clear", names(df)) # last time step var
  
  
  for (i in str_num:end_num) {
    
    # Make and name empty variable
    df[[ncol(df)+1]] <- rep(FALSE, nrow(df))
    
    names(df)[ncol(df)] <- paste0("TT_",
                                  names(df)[i])
    
    # Check if greater than total time
    df[[ncol(df)]] <- replace(df[[ncol(df)]], 
                              (df[[i]] > df$TotalTime) & (df$TotalTime > 0),
                              TRUE)
    
    df[[ncol(df)]] <- replace(df[[ncol(df)]], 
                              is.na(df[[i]]),
                              NA)
    
  }
  
}

### Dummy Variables ###
{
  # Child Team (1 = on Child Team, 0 = Adult/PERT)
  df$ChildTeam_Dum <- ifelse(is.na(df$Cat_team), NA,
                             ifelse(df$Cat_team == "Child", 1, 0))
  
  # Male (1 = Man, 0 = Gender besides Man)  
  df$Male_Dum <- ifelse(is.na(df$Gender) | df$Gender == "decline", NA,
                        ifelse(df$Gender == "male", 1, 0))
  
  # English is Family Langauge (1 = English, 0 = Family Language besides English)
  df$EngFL_Dum <- ifelse(is.na(df$Family_language), NA,
                         ifelse(df$Family_language == "english", 1, 0))
  
  # Substance Abuse (1 = Substance Abuse today "yes", 0 = "no")
  df$SubAbuse_Dum <- ifelse(is.na(df$Substance_abuse_yn), NA,
                            ifelse(df$Substance_abuse_yn == "Yes", 1, 0))
  
}

### Temporal Variables ###
{
  # Month
  df$Month_Req <- month(df$Request_date)
  
  # Month Name
  df$Month_ReqSTR <- months(df$Request_date)
  
  # Day of the Week
  df$DayWeek_Req <- weekdays(df$Request_date)
  
  df$DayWeek_Req <- factor(df$DayWeek_Req,
                           levels = c("Sunday", "Monday", "Tuesday", 'Wednesday',
                                      "Thursday", "Friday", "Saturday"))
  
  df$DayWeek_Req <- as.integer(df$DayWeek_Req)
  
  # Day of the week Name
  df$DayWeek_ReqSTR <- weekdays(df$Request_date)
  
  # Time of day (scale of 0 to 24)
  df$TimeDay_Req <- format(df$Request_date, format = "%Y/%m/%d") # set to date
  
  df$TimeDay_Req <- format(as.POSIXct(paste(df$TimeDay_Req, "00:00:00")), 
                           format = "%Y-%m-%d %H:%M:%S") # set to midnight on date
  
  df$TimeDay_Req <- abs(as.numeric(difftime(df$TimeDay_Req, 
                                            df$Request_date, 
                                            units = "hours"))) # Get diff from mn
  
}
