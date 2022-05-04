# CAT Regression
# Matthew Miltimore
# Updated: 5/4/2022

### Start-up ###
# This section includes spot to change sheet (i.e. change data being analyzed)
# Must also update working directory to folder which holds 'CAT_R_load.xlsx'
{
# Clear workspace
if(!is.null(dev.list())) dev.off
cat("\014")
rm(list = ls())

# Packages
# If these packages are not yet on your the system, must run install.packages
# install.packages("package_name")
library(readxl)
library(dplyr)
library(stargazer)
library(ggplot2)

# Load Data
sheet.name <- "January21-April22" ### <- CHANGE DATA SHEET HERE ###

setwd('/Users/Matthew Miltimore/Documents/CAT') ### <- CHANGE DIRECTORY HERE ###

df <- read_xlsx('CAT_R_load.xlsx', 
                sheet = sheet.name)

# Remove rows where all values are N/A
df <- df[rowSums(is.na(df)) != ncol(df), ]

# Graph note
if (grepl("-", sheet.name, fixed = TRUE)) {
  
  note_source <- paste0("Data pulled from ",
                        substr(sub("-.*", "", sheet.name),
                               1,
                               nchar(sub("-.*", "", sheet.name))-2),
                        " 20",
                        substr(sub("-.*", "", sheet.name),
                               nchar(sub("-.*", "", sheet.name))-1,
                               nchar(sub("-.*", "", sheet.name))),
                        " through ",
                        substr(sub(".*-", "", sheet.name),
                               1,
                               nchar(sub(".*-", "", sheet.name))-2),
                        " 20",
                        substr(sheet.name,nchar(sheet.name)-1,nchar(sheet.name)),
                        ".")
  
} else {
  
  note_source <- paste0("Data pulled from ", 
                        substr(sheet.name,1,nchar(sheet.name)-2),
                        " 20",
                        substr(sheet.name,nchar(sheet.name)-1,nchar(sheet.name)),
                        ".")
  
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
  df.month <- subset(df,!(grepl("/2021", df$Request_date, fixed = TRUE) & 
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
