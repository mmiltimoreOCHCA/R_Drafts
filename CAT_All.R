# CAT All (Clean, Regression, and Summary Analysis)
# Matthew Miltimore
# Updated: 5/24/2022

### Start-up ###
# MUST also update working directory to folder which holds must current data
# MUST manually update name of file with most current data
{
  # Clear workspace
  if(!is.null(dev.list())) dev.off
  cat("\014")
  rm(list = ls())
  
  # Packages
  # If these packages are not yet on your system, must run install.packages
  # install.packages("packagename")
  library(dplyr)
  library(lubridate)
  library(stargazer)
  library(ggplot2)
  library(readr)
  
  # Load Data
  setwd('/Users/Matthew Miltimore/Documents/CAT') ### <- CHANGE DIRECTORY HERE ###
  
  df <- read.csv("cat_dispatch 5-1-22.csv") ### <- CHANGE DATA HERE (must be .csv) ###
  
  # Remove rows where all values are N/A
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  
}

################################################################################
# Cleaning
################################################################################

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

################################################################################
# Regression
################################################################################
{
  # Regression note
  min_my <- format(min(df$Request_date), "%B %Y")
  max_my <- format(max(df$Request_date), "%B %Y")
  
  if (min_my == max_my) {
    
    note_source <- paste0("Data pulled from ", 
                          format(max(df$Request_date), "%B %Y"),
                          ".")
    
    sheet.name <- format(max(df$Request_date), "%b%y")
    
  } else {
    
    note_source <- paste0("Data pulled from ",
                          format(min(df$Request_date), "%B %Y"),
                          " through ",
                          format(max(df$Request_date), "%B %Y"),
                          ".")
    
    sheet.name <- paste0(format(min(df$Request_date), "%b%y"),
                         "-",
                         format(max(df$Request_date), "%b%y"))
    
  }
  
  
  
}

#####
# Dummy predictors
#####

### CAT Team as Pred. of Step Time ### 
{
  # Linear Regression loop
  str_num <- match("Req_CallEnd", names(df)) # first step var
  end_num <- match("TotalTime", names(df)) # last step var
  
  mod_list <- list()
  mod_na_list <- list()
  
  for (i in str_num:end_num) {
    
    # Regression
    print(names(df)[i])
    
    assign(paste0("mod", names(df)[i]),
           mod <- lm(df[[i]] ~ ChildTeam_Dum, data = df))
    
    mod_list[[i-str_num+1]] <- mod
    
    print(summary(mod))
    
    # Non-Anomalous Regression
    print(paste(names(df)[i], "without Anomolies"))
    
    if (i == end_num) {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE)
      
    } else {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE &
                         df[[i+(length(str_num:end_num)*2)]] == FALSE)
      
    }
    
    assign(paste0("mod", names(df)[i], "w/o_anom"),
           mod.sub <- lm(df.sub[[i]] ~ ChildTeam_Dum, data = df.sub))
    
    mod_na_list[[i-str_num+1]] <- mod.sub
    
    print(summary(mod.sub))
    
  }
  
  # Stargazer 
  dv_list = c('Request to Call End',
              'Call End to Assign',
              'Request to Assign',
              'Assign to Dispatch',
              'Dispatch to Arrival',
              'Arrival to Assesment Start',
              'Assesment Time',
              'Assesment End to Clear',
              'Total Time')
  
  # CAT Team, Step Time, LINEAR
  #####
  stargazer(mod_list, 
            type = 'html',
            out = paste0('TeamStepReg_', sheet.name, '.html'),
            title = "Linear Regression of CAT Team as Predictor of Step Time in Minutes",
            dep.var.labels.include = FALSE,
            column.labels = dv_list,
            model.numbers = FALSE,
            covariate.labels = c("Child Team",
                                 "Constant"),
            notes = paste(note_source, 
                          "(*p<0.1; **p<0.05; ***p<0.01)"),
            notes.align = "r",
            notes.append = FALSE)
  #####
  
  # CAT Team, Step Time, LINEAR (NON-ANOMOLOUS)
  #####
  stargazer(mod_na_list, 
            type = 'html',
            out = paste0('TeamStepRegNA_', sheet.name, '.html'),
            title = "Linear Regression of CAT Team as Predictor of Step Time in Minutes (Non-Anomalous)",
            dep.var.labels.include = FALSE,
            column.labels = dv_list,
            model.numbers = FALSE,
            covariate.labels = c("Child Team",
                                 "Constant"),
            notes = paste(note_source,
                          "Anomalies are entries which are negative, outliers,",
                          "or greater than the total time.",
                          "(*p<0.1; **p<0.05; ***p<0.01)"),
            notes.align = "r",
            notes.append = FALSE)
  #####
}

### Gender as Pred. of Step Time ###
{
  # Linear Regression loop
  str_num <- match("Req_CallEnd", names(df)) # first step var
  end_num <- match("TotalTime", names(df)) # last step var
  
  mod_list <- list()
  mod_na_list <- list()
  
  for (i in str_num:end_num) {
    
    # Regression
    print(names(df)[i])
    
    assign(paste0("mod", names(df)[i]),
           mod <- lm(df[[i]] ~ Male_Dum, data = df))
    
    mod_list[[i-str_num+1]] <- mod
    
    print(summary(mod))
    
    # Non-Anomalous Regression
    print(paste(names(df)[i], "without Anomolies"))
    
    if (i == end_num) {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE)
      
    } else {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE &
                         df[[i+(length(str_num:end_num)*2)]] == FALSE)
      
    }
    
    assign(paste0("mod", names(df)[i], "w/o_anom"),
           mod.sub <- lm(df.sub[[i]] ~ Male_Dum, data = df.sub))
    
    mod_na_list[[i-str_num+1]] <- mod.sub
    
    print(summary(mod.sub))
    
  }
  
  # Stargazer 
  dv_list = c('Request to Call End',
              'Call End to Assign',
              'Request to Assign',
              'Assign to Dispatch',
              'Dispatch to Arrival',
              'Arrival to Assesment Start',
              'Assesment Time',
              'Assesment End to Clear',
              'Total Time')
  
  # CAT Team, Step Time, LINEAR
  #####
  stargazer(mod_list, 
            type = 'html',
            out = paste0('GenderStepReg_', sheet.name, '.html'),
            title = "Linear Regression of Gender as Predictor of Step Time in Minutes",
            dep.var.labels.include = FALSE,
            column.labels = dv_list,
            model.numbers = FALSE,
            covariate.labels = c("Male",
                                 "Constant"),
            notes = paste(note_source, 
                          "(*p<0.1; **p<0.05; ***p<0.01)"),
            notes.align = "r",
            notes.append = FALSE)
  #####
  
  # CAT Team, Step Time, LINEAR (NON-ANOMOLOUS)
  #####
  stargazer(mod_na_list, 
            type = 'html',
            out = paste0('GenderStepRegNA_', sheet.name, '.html'),
            title = "Linear Regression of Gender as Predictor of Step Time in Minutes (Non-Anomalous)",
            dep.var.labels.include = FALSE,
            column.labels = dv_list,
            model.numbers = FALSE,
            covariate.labels = c("Male",
                                 "Constant"),
            notes = paste(note_source,
                          "Anomalies are entries which are negative, outliers,",
                          "or greater than the total time.",
                          "(*p<0.1; **p<0.05; ***p<0.01)"),
            notes.align = "r",
            notes.append = FALSE)
  #####
}

### Family Language (English Dummy) as Pred. of Step Time ###
{
  # Linear Regression loop
  str_num <- match("Req_CallEnd", names(df)) # first step var
  end_num <- match("TotalTime", names(df)) # last step var
  
  mod_list <- list()
  mod_na_list <- list()
  
  for (i in str_num:end_num) {
    
    # Regression
    print(names(df)[i])
    
    assign(paste0("mod", names(df)[i]),
           mod <- lm(df[[i]] ~ EngFL_Dum, data = df))
    
    mod_list[[i-str_num+1]] <- mod
    
    print(summary(mod))
    
    # Non-Anomalous Regression
    print(paste(names(df)[i], "without Anomolies"))
    
    if (i == end_num) {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE)
      
    } else {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE &
                         df[[i+(length(str_num:end_num)*2)]] == FALSE)
      
    }
    
    assign(paste0("mod", names(df)[i], "w/o_anom"),
           mod.sub <- lm(df.sub[[i]] ~ EngFL_Dum, data = df.sub))
    
    mod_na_list[[i-str_num+1]] <- mod.sub
    
    print(summary(mod.sub))
    
  }
  
  # Stargazer 
  dv_list = c('Request to Call End',
              'Call End to Assign',
              'Request to Assign',
              'Assign to Dispatch',
              'Dispatch to Arrival',
              'Arrival to Assesment Start',
              'Assesment Time',
              'Assesment End to Clear',
              'Total Time')
  
  # CAT Team, Step Time, LINEAR
  #####
  stargazer(mod_list, 
            type = 'html',
            out = paste0('FamLangStepReg_', sheet.name, '.html'),
            title = "Linear Regression of Family Language as Predictor of Step Time in Minutes",
            dep.var.labels.include = FALSE,
            column.labels = dv_list,
            model.numbers = FALSE,
            covariate.labels = c("English Family Language",
                                 "Constant"),
            notes = paste(note_source, 
                          "(*p<0.1; **p<0.05; ***p<0.01)"),
            notes.align = "r",
            notes.append = FALSE)
  #####
  
  # CAT Team, Step Time, LINEAR (NON-ANOMOLOUS)
  #####
  stargazer(mod_na_list, 
            type = 'html',
            out = paste0('FamLangStepRegNA_', sheet.name, '.html'),
            title = "Linear Regression of Family Language as Predictor of Step Time in Minutes (Non-Anomalous)",
            dep.var.labels.include = FALSE,
            column.labels = dv_list,
            model.numbers = FALSE,
            covariate.labels = c("English Family Language",
                                 "Constant"),
            notes = paste(note_source,
                          "Anomalies are entries which are negative, outliers,",
                          "or greater than the total time.",
                          "(*p<0.1; **p<0.05; ***p<0.01)"),
            notes.align = "r",
            notes.append = FALSE)
  #####
}

### Substance Abuse as Pred. of Step Time ###
{
  # Linear Regression loop
  str_num <- match("Req_CallEnd", names(df)) # first step var
  end_num <- match("TotalTime", names(df)) # last step var
  
  mod_list <- list()
  mod_na_list <- list()
  
  for (i in str_num:end_num) {
    
    # Regression
    print(names(df)[i])
    
    assign(paste0("mod", names(df)[i]),
           mod <- lm(df[[i]] ~ SubAbuse_Dum, data = df))
    
    mod_list[[i-str_num+1]] <- mod
    
    print(summary(mod))
    
    # Non-Anomalous Regression
    print(paste(names(df)[i], "without Anomolies"))
    
    if (i == end_num) {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE)
      
    } else {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE &
                         df[[i+(length(str_num:end_num)*2)]] == FALSE)
      
    }
    
    assign(paste0("mod", names(df)[i], "w/o_anom"),
           mod.sub <- lm(df.sub[[i]] ~ SubAbuse_Dum, data = df.sub))
    
    mod_na_list[[i-str_num+1]] <- mod.sub
    
    print(summary(mod.sub))
    
  }
  
  # Stargazer 
  dv_list = c('Request to Call End',
              'Call End to Assign',
              'Request to Assign',
              'Assign to Dispatch',
              'Dispatch to Arrival',
              'Arrival to Assesment Start',
              'Assesment Time',
              'Assesment End to Clear',
              'Total Time')
  
  # CAT Team, Step Time, LINEAR
  #####
  stargazer(mod_list, 
            type = 'html',
            out = paste0('SubAbuseStepReg_', sheet.name, '.html'),
            title = "Linear Regression of Substance Abuse as Predictor of Step Time in Minutes",
            dep.var.labels.include = FALSE,
            column.labels = dv_list,
            model.numbers = FALSE,
            covariate.labels = c("Substance Abuse",
                                 "Constant"),
            notes = paste(note_source, 
                          "(*p<0.1; **p<0.05; ***p<0.01)"),
            notes.align = "r",
            notes.append = FALSE)
  #####
  
  # CAT Team, Step Time, LINEAR (NON-ANOMOLOUS)
  #####
  stargazer(mod_na_list, 
            type = 'html',
            out = paste0('SubAbuseStepRegNA_', sheet.name, '.html'),
            title = "Linear Regression of Substance Abuse as Predictor of Step Time in Minutes (Non-Anomalous)",
            dep.var.labels.include = FALSE,
            column.labels = dv_list,
            model.numbers = FALSE,
            covariate.labels = c("Substance Abuse",
                                 "Constant"),
            notes = paste(note_source,
                          "Anomalies are entries which are negative, outliers,",
                          "or greater than the total time.",
                          "(*p<0.1; **p<0.05; ***p<0.01)"),
            notes.align = "r",
            notes.append = FALSE)
  #####
}

#####
# Temporal Predictors
#####

### Time of Day as Pred. of Step Time ###
{
  # DVs
  str_num <- match("Req_CallEnd", names(df)) # first step var
  end_num <- match("TotalTime", names(df)) # last step var
  
  # Scatter Plot
  for (i in str_num:end_num) {
    
    if (i == end_num) {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE)
      
    } else {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE &
                         df[[i+(length(str_num:end_num)*2)]] == FALSE)
      
    }
    
    p <- ggplot(df.sub, aes(x = TimeDay_Req, y = df.sub[[i]])) +
      geom_point() +
      ylab(names(df)[i]) +
      geom_smooth(method = "lm",
                  formula = y ~ poly(x, 4))
    
    print(p)
    
  }
  
  # Linear Regression loop
  mod_list <- list()
  mod_na_list <- list()
  
  for (i in str_num:end_num) {
    
    # Make iv_sqaured for parabolic linear regression
    df$iv_squared <- df$TimeDay_Req*df$TimeDay_Req
    df$iv_cubed <- df$TimeDay_Req*df$TimeDay_Req*df$TimeDay_Req
    df$iv_quad <- df$TimeDay_Req*df$TimeDay_Req*df$TimeDay_Req*df$TimeDay_Req
    
    # Regression
    print(names(df)[i])
    
    assign(paste0("mod", names(df)[i]),
           mod <- lm(df[[i]] ~ 
                       TimeDay_Req +
                       iv_squared +
                       iv_cubed +
                       iv_quad, 
                     data = df))
    
    mod_list[[i-str_num+1]] <- mod
    
    print(summary(mod))
    
    # Non-Anomalous Regression
    print(paste(names(df)[i], "without Anomolies"))
    
    if (i == end_num) {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE)
      
    } else {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE &
                         df[[i+(length(str_num:end_num)*2)]] == FALSE)
      
    }
    
    assign(paste0("mod", names(df)[i], "w/o_anom"),
           mod.sub <- lm(df.sub[[i]] ~ 
                           TimeDay_Req +
                           iv_squared +
                           iv_cubed +
                           iv_quad, 
                         data = df.sub))
    
    mod_na_list[[i-str_num+1]] <- mod.sub
    
    print(summary(mod.sub))
    
  }
  
  # Stargazer 
  dv_list = c('Request to Call End',
              'Call End to Assign',
              'Request to Assign',
              'Assign to Dispatch',
              'Dispatch to Arrival',
              'Arrival to Assesment Start',
              'Assesment Time',
              'Assesment End to Clear',
              'Total Time')
  
  #Time of Day , Step Time, LINEAR
  #####
  stargazer(mod_list,
            type = 'html',
            out = paste0('ToDtepReg_',sheet.name,'.html'),
            title = "Quartic Linear Regression of Time of Day as Predictor of Step Time in Minutes",
            dep.var.labels.include = FALSE,
            column.labels = dv_list,
            model.numbers = FALSE,
            covariate.labels = c("Time of Day",
                                 "Time of Day-Squared",
                                 "Time of Day-Cubed",
                                 "Time of Day-Quartic",
                                 "Constant"),
            notes = paste(note_source,
                          "(*p<0.1; **p<0.05; ***p<0.01)"),
            notes.align = "r",
            notes.append = FALSE)
  #####
  
  # Time of Day, Step Time, LINEAR (NON-ANOMOLOUS)
  #####
  stargazer(mod_na_list, 
            type = 'html',
            out = paste0('ToDStepRegNA',sheet.name,'.html'),
            title = "Quartic Linear Regression of Time of Day as Predictor of Step Time in Minutes (Non-Anomalous)",
            dep.var.labels.include = FALSE,
            column.labels = dv_list,
            model.numbers = FALSE,
            covariate.labels = c("Time of Day",
                                 "Time of Day-Squared",
                                 "Time of Day-Cubed",
                                 "Time of Day-Quartic",
                                 "Constant"),
            notes = paste(note_source,
                          "Anomalies are entries which are negative, outliers,",
                          "or greater than the total time.",
                          "(*p<0.1; **p<0.05; ***p<0.01)"),
            notes.align = "r",
            notes.append = FALSE)
  #####
  
  # R-squared check
  #####
  R_lin <- list()
  R_poly2 <- list()
  R_poly3 <- list()
  R_poly4 <- list()
  R_linNA <- list()
  R_poly2NA <- list()
  R_poly3NA <- list()
  R_poly4NA <- list()
  
  
  for (i in str_num:end_num) {
    
    df$iv_squared <- df$TimeDay_Req*df$TimeDay_Req
    df$iv_cubed <- df$TimeDay_Req*df$TimeDay_Req*df$TimeDay_Req
    df$iv_quad <- df$TimeDay_Req*df$TimeDay_Req*df$TimeDay_Req*df$TimeDay_Req
    
    if (i == end_num) {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE)
      
    } else {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE &
                         df[[i+(length(str_num:end_num)*2)]] == FALSE)
      
    }
    
    R_lin[[i-str_num+1]] <- summary(lm(df[[i]] ~ TimeDay_Req, 
                                       data = df))$r.squared
    
    R_linNA[[i-str_num+1]] <- summary(lm(df.sub[[i]] ~ TimeDay_Req, 
                                         data = df.sub))$r.squared
    
    R_poly2[[i-str_num+1]] <- summary(lm(df[[i]] ~ TimeDay_Req + 
                                           iv_squared, 
                                         data = df))$r.squared
    
    R_poly2NA[[i-str_num+1]] <- summary(lm(df.sub[[i]] ~ TimeDay_Req + 
                                             iv_squared, 
                                           data = df.sub))$r.squared
    
    R_poly3[[i-str_num+1]] <- summary(lm(df[[i]] ~ TimeDay_Req + 
                                           iv_squared +
                                           iv_cubed, 
                                         data = df))$r.squared
    
    R_poly3NA[[i-str_num+1]] <- summary(lm(df.sub[[i]] ~ TimeDay_Req + 
                                             iv_squared +
                                             iv_cubed, 
                                           data = df.sub))$r.squared
    
    R_poly4[[i-str_num+1]] <- summary(lm(df[[i]] ~ TimeDay_Req + 
                                           iv_squared +
                                           iv_cubed +
                                           iv_quad, 
                                         data = df))$r.squared
    
    R_poly4NA[[i-str_num+1]] <- summary(lm(df.sub[[i]] ~ TimeDay_Req + 
                                             iv_squared +
                                             iv_cubed +
                                             iv_quad, 
                                           data = df.sub))$r.squared
    
    
  }
  #####
}

### Day of the Week as Pred. of Step Time ###
{
  # DVs
  str_num <- match("Req_CallEnd", names(df)) # first step var
  end_num <- match("TotalTime", names(df)) # last step var
  
  # Scatter Plot
  for (i in str_num:end_num) {
    
    if (i == end_num) {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE)
      
    } else {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE &
                         df[[i+(length(str_num:end_num)*2)]] == FALSE)
      
    }
    
    p <- ggplot(df.sub, aes(x = DayWeek_Req, y = df.sub[[i]])) +
      geom_point() +
      ylab(names(df)[i]) +
      geom_smooth(method = "lm",
                  formula = y ~ poly(x, 4))
    
    print(p)
    
  }
  
  # Linear Regression loop
  mod_list <- list()
  mod_na_list <- list()
  
  for (i in str_num:end_num) {
    
    # Make iv_sqaured for parabolic linear regression
    df$iv_squared <- df$DayWeek_Req*df$DayWeek_Req
    df$iv_cubed <- df$DayWeek_Req*df$DayWeek_Req*df$DayWeek_Req
    df$iv_quad <- df$DayWeek_Req*df$DayWeek_Req*df$DayWeek_Req*df$DayWeek_Req
    
    # Regression
    print(names(df)[i])
    
    assign(paste0("mod", names(df)[i]),
           mod <- lm(df[[i]] ~ 
                       DayWeek_Req +
                       iv_squared +
                       iv_cubed +
                       iv_quad, 
                     data = df))
    
    mod_list[[i-str_num+1]] <- mod
    
    print(summary(mod))
    
    # Non-Anomalous Regression
    print(paste(names(df)[i], "without Anomolies"))
    
    if (i == end_num) {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE)
      
    } else {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE &
                         df[[i+(length(str_num:end_num)*2)]] == FALSE)
      
    }
    
    assign(paste0("mod", names(df)[i], "w/o_anom"),
           mod.sub <- lm(df.sub[[i]] ~ 
                           DayWeek_Req +
                           iv_squared +
                           iv_cubed +
                           iv_quad,
                         data = df.sub))
    
    mod_na_list[[i-str_num+1]] <- mod.sub
    
    print(summary(mod.sub))
    
  }
  
  # Stargazer 
  dv_list = c('Request to Call End',
              'Call End to Assign',
              'Request to Assign',
              'Assign to Dispatch',
              'Dispatch to Arrival',
              'Arrival to Assesment Start',
              'Assesment Time',
              'Assesment End to Clear',
              'Total Time')
  
  #Day of Week , Step Time, LINEAR
  #####
  stargazer(mod_list,
            type = 'html',
            out = paste0('DoWtepReg',sheet.name,'.html'),
            title = "Quartic Linear Regression of Day of the Week as Predictor of Step Time in Minutes",
            dep.var.labels.include = FALSE,
            column.labels = dv_list,
            model.numbers = FALSE,
            covariate.labels = c("Day of the Week",
                                 "Day of the Week-Squared",
                                 "Day of the Week-Cubed",
                                 "Day of the Week-Quartic",
                                 "Constant"),
            notes = paste(note_source,
                          "(*p<0.1; **p<0.05; ***p<0.01)"),
            notes.align = "r",
            notes.append = FALSE)
  #####
  
  # Day of the Week, Step Time, LINEAR (NON-ANOMOLOUS)
  #####
  stargazer(mod_na_list, 
            type = 'html',
            out = paste0('DoWStepRegNA',sheet.name,'.html'),
            title = "Quartic Linear Regression of Day of the Week as Predictor of Step Time in Minutes (Non-Anomalous)",
            dep.var.labels.include = FALSE,
            column.labels = dv_list,
            model.numbers = FALSE,
            covariate.labels = c("Day of the Week",
                                 "Day of the Week-Squared",
                                 "Day of the Week-Cubed",
                                 "Day of the Week-Quartic",
                                 "Constant"),
            notes = paste(note_source,
                          "Anomalies are entries which are negative, outliers,",
                          "or greater than the total time.",
                          "(*p<0.1; **p<0.05; ***p<0.01)"),
            notes.align = "r",
            notes.append = FALSE)
  #####
  
  # R-squared check
  #####
  R_lin <- list()
  R_poly2 <- list()
  R_poly3 <- list()
  R_poly4 <- list()
  R_linNA <- list()
  R_poly2NA <- list()
  R_poly3NA <- list()
  R_poly4NA <- list()
  
  
  for (i in str_num:end_num) {
    
    df$iv_squared <- df$DayWeek_Req*df$DayWeek_Req
    df$iv_cubed <- df$DayWeek_Req*df$DayWeek_Req*df$DayWeek_Req
    df$iv_quad <- df$DayWeek_Req*df$DayWeek_Req*df$DayWeek_Req*df$DayWeek_Req
    
    if (i == end_num) {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE)
      
    } else {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE &
                         df[[i+(length(str_num:end_num)*2)]] == FALSE)
      
    }
    
    R_lin[[i-str_num+1]] <- summary(lm(df[[i]] ~ DayWeek_Req, 
                                       data = df))$r.squared
    
    R_linNA[[i-str_num+1]] <- summary(lm(df.sub[[i]] ~ DayWeek_Req, 
                                         data = df.sub))$r.squared
    
    R_poly2[[i-str_num+1]] <- summary(lm(df[[i]] ~ DayWeek_Req + 
                                           iv_squared, 
                                         data = df))$r.squared
    
    R_poly2NA[[i-str_num+1]] <- summary(lm(df.sub[[i]] ~ DayWeek_Req + 
                                             iv_squared, 
                                           data = df.sub))$r.squared
    
    R_poly3[[i-str_num+1]] <- summary(lm(df[[i]] ~ DayWeek_Req + 
                                           iv_squared +
                                           iv_cubed, 
                                         data = df))$r.squared
    
    R_poly3NA[[i-str_num+1]] <- summary(lm(df.sub[[i]] ~ DayWeek_Req + 
                                             iv_squared +
                                             iv_cubed, 
                                           data = df.sub))$r.squared
    
    R_poly4[[i-str_num+1]] <- summary(lm(df[[i]] ~ DayWeek_Req + 
                                           iv_squared +
                                           iv_cubed +
                                           iv_quad, 
                                         data = df))$r.squared
    
    R_poly4NA[[i-str_num+1]] <- summary(lm(df.sub[[i]] ~ DayWeek_Req + 
                                             iv_squared +
                                             iv_cubed +
                                             iv_quad, 
                                           data = df.sub))$r.squared
    
    
  }
  #####
}

### Month as Pred. of Step Time ###
{
  
  if (length(unique(df$Month_Req)) > 1) {
    
    # Month subset - Exclude Jan-May 2021 because of low entry amount 
    df.month <- subset(df,!(grepl("2021-", df$Request_date, fixed = TRUE) & 
                              between(df$Month_Req,1,5)))
    
    # Regression loop
    {
      # DVs
      str_num <- match("Req_CallEnd", names(df.month)) # first step var
      end_num <- match("TotalTime", names(df.month)) # last step var
      
      # Scatter Plot
      for (i in str_num:end_num) {
        
        if (i == end_num) {
          
          df.sub <- subset(df.month, df.month[[i]] >= 0 & 
                             df.month[[i+length(str_num:end_num)]] == FALSE)
          
        } else {
          
          df.sub <- subset(df.month, df.month[[i]] >= 0 & 
                             df.month[[i+length(str_num:end_num)]] == FALSE &
                             df.month[[i+(length(str_num:end_num)*2)]] == FALSE)
          
        }
        
        p <- ggplot(df.sub, aes(x = Month_Req, y = df.sub[[i]])) +
          geom_point() +
          ylab(names(df)[i]) +
          geom_smooth(method = "lm",
                      formula = y ~ poly(x, 4))
        
        print(p)
        
      }
      
      # Linear Regression loop
      mod_list <- list()
      mod_na_list <- list()
      
      for (i in str_num:end_num) {
        
        # Make iv_sqaured for parabolic linear regression
        df.month$iv_squared <- df.month$Month_Req*df.month$Month_Req
        df.month$iv_cubed <- df.month$iv_squared*df.month$Month_Req
        df.month$iv_quad <- df.month$iv_cubed*df.month$Month_Req
        
        # Regression
        print(names(df)[i])
        
        assign(paste0("mod", names(df)[i]),
               mod <- lm(df.month[[i]] ~ 
                           Month_Req +
                           iv_squared +
                           iv_cubed +
                           iv_quad, 
                         data = df.month))
        
        mod_list[[i-str_num+1]] <- mod
        
        print(summary(mod))
        
        # Non-Anomalous Regression
        print(paste(names(df)[i], "without Anomolies"))
        
        if (i == end_num) {
          
          df.sub <- subset(df.month, df.month[[i]] >= 0 & 
                             df.month[[i+length(str_num:end_num)]] == FALSE)
          
        } else {
          
          df.sub <- subset(df.month, df.month[[i]] >= 0 & 
                             df.month[[i+length(str_num:end_num)]] == FALSE &
                             df.month[[i+(length(str_num:end_num)*2)]] == FALSE)
          
        }
        
        assign(paste0("mod", names(df)[i], "w/o_anom"),
               mod.sub <- lm(df.sub[[i]] ~ 
                               DayWeek_Req +
                               iv_squared +
                               iv_cubed +
                               iv_quad,
                             data = df.sub))
        
        mod_na_list[[i-str_num+1]] <- mod.sub
        
        print(summary(mod.sub))
        
      }
      
      # Stargazer 
      dv_list = c('Request to Call End',
                  'Call End to Assign',
                  'Request to Assign',
                  'Assign to Dispatch',
                  'Dispatch to Arrival',
                  'Arrival to Assesment Start',
                  'Assesment Time',
                  'Assesment End to Clear',
                  'Total Time')
      
      #Day of Week , Step Time, LINEAR
      #####
      stargazer(mod_list,
                type = 'html',
                out = paste0('MonthStepReg',sheet.name,'.html'),
                title = "Quartic Linear Regression of Month as Predictor of Step Time in Minutes",
                dep.var.labels.include = FALSE,
                column.labels = dv_list,
                model.numbers = FALSE,
                covariate.labels = c("Month",
                                     "Month-Squared",
                                     "Month-Cubed",
                                     "Month-Quartic",
                                     "Constant"),
                notes = paste(note_source,
                              "(*p<0.1; **p<0.05; ***p<0.01)"),
                notes.align = "r",
                notes.append = FALSE)
      #####
      
      # Day of the Week, Step Time, LINEAR (NON-ANOMOLOUS)
      #####
      stargazer(mod_na_list, 
                type = 'html',
                out = paste0('MonthStepRegNA',sheet.name,'.html'),
                title = "Quartic Linear Regression of Month as Predictor of Step Time in Minutes (Non-Anomalous)",
                dep.var.labels.include = FALSE,
                column.labels = dv_list,
                model.numbers = FALSE,
                covariate.labels = c("Month",
                                     "Month-Squared",
                                     "Month-Cubed",
                                     "Month-Quartic",
                                     "Constant"),
                notes = paste(note_source,
                              "Anomalies are entries which are negative, outliers,",
                              "or greater than the total time.",
                              "(*p<0.1; **p<0.05; ***p<0.01)"),
                notes.align = "r",
                notes.append = FALSE)
      #####
      
      # R-squared check
      #####
      R_lin <- list()
      R_poly2 <- list()
      R_poly3 <- list()
      R_poly4 <- list()
      R_linNA <- list()
      R_poly2NA <- list()
      R_poly3NA <- list()
      R_poly4NA <- list()
      
      
      for (i in str_num:end_num) {
        
        df.month$iv_squared <- df.month$Month_Req*df.month$Month_Req
        df.month$iv_cubed <- df.month$iv_squared*df.month$Month_Req
        df.month$iv_quad <- df.month$iv_cubed*df.month$Month_Req
        
        if (i == end_num) {
          
          df.sub <- subset(df.month, df.month[[i]] >= 0 & 
                             df.month[[i+length(str_num:end_num)]] == FALSE)
          
        } else {
          
          df.sub <- subset(df.month, df.month[[i]] >= 0 & 
                             df.month[[i+length(str_num:end_num)]] == FALSE &
                             df.month[[i+(length(str_num:end_num)*2)]] == FALSE)
          
        }
        
        R_lin[[i-str_num+1]] <- summary(lm(df[[i]] ~ Month_Req, 
                                           data = df))$r.squared
        
        R_linNA[[i-str_num+1]] <- summary(lm(df.sub[[i]] ~ Month_Req, 
                                             data = df.sub))$r.squared
        
        R_poly2[[i-str_num+1]] <- summary(lm(df[[i]] ~ Month_Req + 
                                               iv_squared, 
                                             data = df))$r.squared
        
        R_poly2NA[[i-str_num+1]] <- summary(lm(df.sub[[i]] ~ Month_Req + 
                                                 iv_squared, 
                                               data = df.sub))$r.squared
        
        R_poly3[[i-str_num+1]] <- summary(lm(df[[i]] ~ Month_Req + 
                                               iv_squared +
                                               iv_cubed, 
                                             data = df))$r.squared
        
        R_poly3NA[[i-str_num+1]] <- summary(lm(df.sub[[i]] ~ Month_Req + 
                                                 iv_squared +
                                                 iv_cubed, 
                                               data = df.sub))$r.squared
        
        R_poly4[[i-str_num+1]] <- summary(lm(df[[i]] ~ Month_Req + 
                                               iv_squared +
                                               iv_cubed +
                                               iv_quad, 
                                             data = df))$r.squared
        
        R_poly4NA[[i-str_num+1]] <- summary(lm(df.sub[[i]] ~ Month_Req + 
                                                 iv_squared +
                                                 iv_cubed +
                                                 iv_quad, 
                                               data = df.sub))$r.squared
        
        
      }
      #####
    }
    
  } else {
    
    print("Data only includes one month, so month cannot be used as predictor")
    
  }
  
}

################################################################################
# Summary Analysis
################################################################################

### Summary Data Frame (for Lucidchart) ###
{
  
  str_num <- match("Req_CallEnd", names(df)) # first step var
  end_num <- match("TotalTime", names(df)) # last step var
  
  # Empty Data Fram
  Steps <- rep(NA, 27)
  Mean <- Steps
  Median <- Mean
  Count <- Median
  Mean_NA <- Count
  Median_NA <- Mean_NA
  Count_NA <- Median_NA
  
  
  df.sum <- data.frame(Steps, Mean, Median, Count, Mean_NA, Median_NA, Count_NA)
  
  # Calculation Loop
  indx <- 1
  
  for (i in str_num:end_num) {
    
    # Child and Adult/PERT
    df.sum$Steps[indx] <- colnames(df[i])
    
    # Anomalous
    df.sum$Mean[indx] <- mean(df[[i]], na.rm = TRUE)
    
    df.sum$Median[indx] <- median(df[[i]], na.rm = TRUE)
    
    df.sum$Count[indx] <- sum(!is.na(df[[i]]))
    
    # Non-Anomalous
    if (i == end_num) {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE)
      
    } else {
      
      df.sub <- subset(df, df[[i]] >= 0 & 
                         df[[i+length(str_num:end_num)]] == FALSE &
                         df[[i+(length(str_num:end_num)*2)]] == FALSE)
      
    }
    
    df.sum$Mean_NA[indx] <- mean(df.sub[[i]], na.rm = TRUE)
    
    df.sum$Median_NA[indx] <- median(df.sub[[i]], na.rm = TRUE)
    
    df.sum$Count_NA[indx] <- sum(!is.na(df.sub[[i]]))
    
    # Index update
    indx <- indx + 1
    
    
    # Just Adult/Pert
    df.sum$Steps[indx] <- paste0(colnames(df[i]), "_Adult")
    
    # Anomalous
    df.sum$Mean[indx] <- mean(df[[i]][df$Cat_team != "Child"], na.rm = TRUE)
    
    df.sum$Median[indx] <- median(df[[i]][df$Cat_team != "Child"], na.rm = TRUE)
    
    df.sum$Count[indx] <- sum(!is.na(df[[i]][df$Cat_team != "Child"]))
    
    # Non-Anomalous
    df.sum$Mean_NA[indx] <- mean(df.sub[[i]][df$Cat_team != "Child"], na.rm = TRUE)
    
    df.sum$Median_NA[indx] <- median(df.sub[[i]][df$Cat_team != "Child"], na.rm = TRUE)
    
    df.sum$Count_NA[indx] <- sum(!is.na(df.sub[[i]][df$Cat_team != "Child"]))
    
    # Index update
    indx <- indx + 1
    
    
    # Just Child
    df.sum$Steps[indx] <- paste0(colnames(df[i]), "_Child")
    
    # Anomalous
    df.sum$Mean[indx] <- mean(df[[i]][df$Cat_team == "Child"], na.rm = TRUE)
    
    df.sum$Median[indx] <- median(df[[i]][df$Cat_team == "Child"], na.rm = TRUE)
    
    df.sum$Count[indx] <- sum(!is.na(df[[i]][df$Cat_team == "Child"]))
    
    # Non-Anomalous
    df.sum$Mean_NA[indx] <- mean(df.sub[[i]][df$Cat_team == "Child"], na.rm = TRUE)
    
    df.sum$Median_NA[indx] <- median(df.sub[[i]][df$Cat_team == "Child"], na.rm = TRUE)
    
    df.sum$Count_NA[indx] <- sum(!is.na(df.sub[[i]][df$Cat_team == "Child"]))
    
    # Index update
    indx <- indx + 1
    
  }
  
}

# Mean Breakdowns (Hours, Minutes, Seconds)
{
  
  mean_per <- seconds_to_period(df.sum$Mean*60)
  
  df.sum$Mean_Hrs <- mean_per@hour + day(mean_per)*24
  
  df.sum$Mean_Min <- abs(minute(mean_per))
  
  df.sum$Mean_Sec <- round(abs(second(mean_per)))
  
  
  mean_perNA <- seconds_to_period(df.sum$Mean_NA*60)
  
  df.sum$Mean_NA_Hrs <- mean_perNA@hour + day(mean_perNA)*24
  
  df.sum$Mean_NA_Min <- abs(minute(mean_perNA))
  
  df.sum$Mean_NA_Sec <- round(abs(second(mean_perNA)))
  
}

# Summary Text and Breakdown Removal
{
  
  df.sum$Sum_Text <- paste0(ifelse(df.sum$Mean_Hrs != 0, df.sum$Mean_Hrs, ""),
                            ifelse(df.sum$Mean_Hrs != 0, " hr: ", ""),
                            ifelse(df.sum$Mean_Min != 0, df.sum$Mean_Min, ""),
                            ifelse(df.sum$Mean_Min != 0, " min: ", ""),
                            df.sum$Mean_Sec,
                            " sec (",
                            df.sum$Median,
                            " min)")
  
  df.sum$NA_Sum_Text <- paste0(ifelse(df.sum$Mean_NA_Hrs != 0, df.sum$Mean_NA_Hrs, ""),
                               ifelse(df.sum$Mean_NA_Hrs != 0, " hr: ", ""),
                               ifelse(df.sum$Mean_NA_Min != 0, df.sum$Mean_NA_Min, ""),
                               ifelse(df.sum$Mean_NA_Min != 0, " min: ", ""),
                               df.sum$Mean_NA_Sec,
                               " sec (",
                               df.sum$Median_NA,
                               " min)")
  
  df.sum$Count_Text <- paste0(df.sum$Count_NA, "/", df.sum$Count)
  
  # Remove breakdown vars
  df.sum = subset(df.sum, select = -c(8:13))
  
}

### Export Summary Data Frame ###
{
  # As .csv ### KEEP "/CAT_Summary.csv" BUT CHANGE DIRECTORY BEFORE"
  write_csv(df.sum, 
            "CAT_Summary.csv") 
  
  # As .html
  stargazer(df.sum[,1:7], 
            summary = FALSE,
            rownames = FALSE,
            type = "html",
            out = 'CAT_Summary.html',
            title = "CAT Summary Analysis",
            notes = paste("NA indicates Non-Anomalous Data.",
                          "Anomalies are entries which are", 
                          "negative, outliers, or greater than the total time.",
                          note_source))
  
}

### Tableau (Histogram) Dataframe ###
{
  
histmin_vec <- c(15, 10, 3)

H15_LB <- seq(-15, 240, 15)
H10_LB <- seq(-10, 180, 10)
H3_LB <- seq(-3, 60, 3)

LB_list <- list(H15_LB, H10_LB, H3_LB)

df.hist <- data.frame(rep(NA, length(LB_list[length(LB_list)][[1]])))

str_num <- match("Req_CallEnd", names(df)) # first time step var
end_num <- match("TotalTime", names(df)) # last time step var


for (histmin in histmin_vec) {
  
  #histmin <- 15
  
  histname <- paste0("H", histmin, "_")
  
  # Make LB variable
  LB_vec <- LB_list[match(histmin, histmin_vec)][[1]]
  
  df.hist[[ncol(df.hist)]][1:length(LB_vec)] <- LB_vec
  
  names(df.hist)[ncol(df.hist)] <- paste0(histname, "LB")
  
  # Make UB variable
  df.hist[[ncol(df.hist)+1]] <- df.hist[[ncol(df.hist)]] + histmin
  
  names(df.hist)[ncol(df.hist)] <- paste0(histname, "UB")
  
  # Make bin variables
  
  # Find bound indexes
  LB_idx <- match(paste0(histname, "LB"), colnames(df.hist))
  
  UB_idx <- match(paste0(histname, "UB"), colnames(df.hist))
  
  last_numrow <- length(df.hist[[UB_idx]][!is.na(df.hist[[UB_idx]])])
  
  
  for (i in str_num:end_num) {
    
    # Make blank variable
    col_n <- ncol(df.hist) + 1
    
    df.hist[[col_n]] <- rep(NA, nrow(df.hist))
    
    step_var <- colnames(df)[i]
    
    names(df.hist)[col_n] <- paste0(histname, step_var)
    
    
    # Fill bins
    for (row in 1:last_numrow) {
      
      # Less than 0
      if (row == 1) {
        
        df.hist[row, col_n] <- sum(df[[step_var]] < 0, na.rm = TRUE)
        
      # Greater than last bound     
      } else if (row == last_numrow) {
        
        df.hist[row, col_n] <- sum(df[[step_var]] >= df.hist[row, LB_idx], 
                                   na.rm = TRUE)
        
      # Between lower and upper bound    
      } else {
        
        df.hist[row, col_n] <- sum(df[[step_var]] >= df.hist[row, LB_idx] &
                                     df[[step_var]] < df.hist[row, UB_idx], 
                                   na.rm = TRUE)
        
      }
      
    }
    
  }
  
  # Make empty variable to be replaced in next iteration (exclude last iterate)
  if (histmin != histmin_vec[length(histmin_vec)]) {
    
    df.hist[[ncol(df.hist)+1]] <- rep(NA, length(LB_list[length(LB_list)][[1]]))
    
  }
  
}

}

### Export Tableau Dataframe ###
{
write_csv(df.hist, 
          "CAT_Tableau(Histograms).csv") 
}
