# OC Cares
# Matthew Miltimore
# Updated: 6/1/2022

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
  # install.packages("writexl")
  library(readxl)
  library(writexl)
  library(lubridate)
  library(readr)
  library(dplyr)
  library(tidyr)

  # Set Working directory
  setwd('/Users/Matthew Miltimore/Documents/R Docs') ### <- CHANGE DIRECTORY HERE ###
  
  # Load/clean sheets with same fields
  for (i in 3:6) {
    
    # Load Data
    df.load <- read_xlsx('OC Cares FY21.22 Qtr 2.xlsx', sheet = i) ### <- CHANGE DATA HERE ###
    
    # Remove rows where all values are N/A
    df.load <- df.load[rowSums(is.na(df.load)) != ncol(df.load), ]
    
    # Remove extra columns
    df.load <- df.load[, !names(df.load) %in% 
                         names(df.load)[grepl("...", names(df.load), 
                                              fixed = TRUE) == TRUE]]
    
    # Add sheet variable
    df.load$SHEET <- rep(excel_sheets('OC Cares FY21.22 Qtr 2.xlsx')[i], 
                         nrow(df.load))
    
    # Convert POSIXct variables with character
    l <- sapply(df.load, is.POSIXct)
    df.load[l] <- lapply(df.load[l], as.character)
    
    # Name dataframe, name is "df.sheet number"
    assign(paste0("df.", i), df.load)
    
    
  }
  
  # Combine dataframes
  df <- rbind(df.3, df.4, df.5, df.6)
  
   
}

### Convert step time strings to date variables ###

# Replace Text response in EOC_START_DATE with NA
df$EOC_START_DATE[grepl("-", df$EOC_START_DATE, fixed = TRUE) == FALSE &
                    is.na(as.numeric(df$EOC_START_DATE)) == TRUE] <- NA


# Vector of EOC_START_DATE that is digit, but not date
days_from_1900 <- df$EOC_START_DATE[grepl("-", 
                                          df$EOC_START_DATE,
                                          fixed = TRUE) == FALSE & 
                                      is.na(as.numeric(df$EOC_START_DATE)) == FALSE]

days_from_1900 <- as.numeric(days_from_1900)

# Replace digits with dates
df$EOC_START_DATE[grepl("-", 
                        df$EOC_START_DATE,
                        fixed = TRUE) == FALSE & 
                    is.na(as.numeric(df$EOC_START_DATE)) == FALSE] <- as.character(as.Date("1899-12-30") + days_from_1900)



{
  
  # list of step variable names to convert
  # Not all "EOC_START_DATE" are dates (Sheet 4)
  dates_vec <- c("DOB",
                 "LAST_REG_DATE",
                 "EOC_START_DATE",
                 "EOC_END_DATE",
                 "DISCHARGE_DATE")
  
  datesnum_vec <- match(dates_vec, names(df))
  
  # loop
  for (i in datesnum_vec) {
    
    df[[i]] <- as.POSIXct(df[[i]])
    
    df[[i]] <- as.Date(df[[i]])
  }
  
}

# Export this dataframe as csv
write_csv(df, "OCCares_Sheets3_6.csv")

write_xlsx(df, "OCCares_Sheets3_6.xlsx")




# Sheet 1 (OpenAcess)
df.1 <- read_xlsx('OC Cares FY21.22 Qtr 2.xlsx', sheet = 1)

mean(difftime(df.1$`Discharge Date`, 
              df.1$`Admission Date`, units = 'days'))[[1]]


table(df.1$`Referral Source`, df.1$CurrentClinic)


df.tmp <- df.1 %>% 
  group_by(`Referral Source`, CurrentClinic) %>%
  summarize(m = mean(difftime(`Discharge Date`, 
                              `Admission Date`, units = 'days'))) %>%
  spread(CurrentClinic, m) %>%
  transmute(`North OA` = as.numeric(`North Open Access`),
            `South OA` = as.numeric(`South Open Access`)) 




