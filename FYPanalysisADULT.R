# Matthew Miltimore

# Start-up
if(!is.null(dev.list())) dev.off
cat("\014")
rm(list = ls())

# Packages 
library(readxl)
library(dplyr)
library(plotrix)
library(ggplot2)

## Set working directory
setwd('/Users/Matthew Miltimore/Documents/R Docs/FYPanalysis')



### CHANGE THESE WITH EACH SHEET/DATA SET ###
## Sheets of interest start with hospitalization, end with education
data.name <- 'Adult'

data.year <- '20-21'

sheet.number <- 13

### Loading Data
df <- read_excel('AdultFSPDataFY2021 - Copy.LC.xlsx', sheet = sheet.number)

### Cleaning Sheet Name (CHAGE DATA HERE TOO)
sheet.name <- excel_sheets('AdultFSPDataFY2021 - Copy.LC.xlsx')[sheet.number]
sheet.name <- gsub("\\(.*", "", sheet.name)
sheet.name <- gsub("\\ .*", "", sheet.name)




### Cleaning variable names and Progam Names
df <- dplyr::rename(df, MHSA_Age_Group = `MHSA Age Group`)

df$ProgramName <- gsub("-", " ", df$ProgramName)

var.count <- ncol(df)

df$prior <- df[var.count - 1] %>% pull(variable.names(df)[var.count - 1])

df$post <- df[var.count] %>% pull(variable.names(df)[var.count])

df$difference <- df$post - df$prior


# Running Models

### Testing interaction between Program and Age Group for post
interaction <- aov(post ~ ProgramName*MHSA_Age_Group, 
                   data = df)
summary(interaction)

#### Plot Bar Plot of Interaction Model
data_summary <- group_by(df, ProgramName, MHSA_Age_Group) %>%
  summarise(Out_mean = mean(post, na.rm = TRUE),
            Out_se = std.error(post, na.rm = TRUE)) %>%
  arrange(desc(Out_mean))

data_summary <- subset(data_summary, !(is.na(data_summary$MHSA_Age_Group)))

ggplot(data_summary, 
       aes(x = MHSA_Age_Group,
           y = Out_mean,
           group = ProgramName)) +
  geom_bar(position=position_dodge(0.9),
           stat = "identity", 
           aes(fill = ProgramName), 
           show.legend = TRUE) +
  geom_errorbar(aes(ymin = Out_mean - Out_se, 
                    ymax = Out_mean + Out_se),
                width = 0.1, 
                position=position_dodge(0.9)) +
  xlab('Age Group') +
  ylab('Mean') +
  labs(fill = 'Program Name') +
  ggtitle(paste(data.name, data.year, sheet.name, '(Post)'))


ggsave(paste(data.name, data.year, sheet.name, '(Post)', '.pdf', 
             sep = ""),
       width = 8,
       height = 6)

### Same for prior
interaction <- aov(prior ~ ProgramName*MHSA_Age_Group, 
                   data = df)
summary(interaction)

#### Plot Bar Plot of Interaction Model
data_summary <- group_by(df, ProgramName, MHSA_Age_Group) %>%
  summarise(Out_mean = mean(prior, na.rm = TRUE),
            Out_se = std.error(prior, na.rm = TRUE)) %>%
  arrange(desc(Out_mean))

data_summary <- subset(data_summary, !(is.na(data_summary$MHSA_Age_Group)))

ggplot(data_summary, 
       aes(x = MHSA_Age_Group,
           y = Out_mean,
           group = ProgramName)) +
  geom_bar(position=position_dodge(0.9),
           stat = "identity", 
           aes(fill = ProgramName), 
           show.legend = TRUE) +
  geom_errorbar(aes(ymin = Out_mean - Out_se, 
                    ymax = Out_mean + Out_se),
                width = 0.1, 
                position=position_dodge(0.9)) +
  xlab('Age Group') +
  ylab('Mean') +
  labs(fill = 'Program Name') +
  ggtitle(paste(data.name, data.year, sheet.name, '(Prior)'))


ggsave(paste(data.name, data.year, sheet.name, '(Prior)', '.pdf', 
             sep = ""),
       width = 8,
       height = 6)

### Same for difference
interaction <- aov(difference ~ ProgramName*MHSA_Age_Group, 
                   data = df)
summary(interaction)

#### Plot Bar Plot of Interaction Model
data_summary <- group_by(df, ProgramName, MHSA_Age_Group) %>%
  summarise(Out_mean = mean(difference, na.rm = TRUE),
            Out_se = std.error(difference, na.rm = TRUE)) %>%
  arrange(desc(Out_mean))

data_summary <- subset(data_summary, !(is.na(data_summary$MHSA_Age_Group)))

ggplot(data_summary, 
       aes(x = MHSA_Age_Group,
           y = Out_mean,
           group = ProgramName)) +
  geom_bar(position=position_dodge(0.9),
           stat = "identity", 
           aes(fill = ProgramName), 
           show.legend = TRUE) +
  geom_errorbar(aes(ymin = Out_mean - Out_se, 
                    ymax = Out_mean + Out_se),
                width = 0.1, 
                position=position_dodge(0.9)) +
  xlab('Age Group') +
  ylab('Mean') +
  labs(fill = 'Program Name') +
  ggtitle(paste(data.name, data.year, sheet.name, '(Difference)'))


ggsave(paste(data.name, data.year, sheet.name, '(Dif)', '.pdf', 
             sep = ""),
       width = 8,
       height = 6)

