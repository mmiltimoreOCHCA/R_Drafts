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
data.name <- 'A-O'

data.year <- '20-21'

sheet.number.adult <- 13

sheet.number.oasis <- 11

### Loading Data
df.a <- read_excel('AdultFSPDataFY2021 - Copy.LC.xlsx',
                   sheet = sheet.number.adult)

df.o <- read_excel('OASIS_FSPDataFY2021-Copy.LC.xlsx',
                   sheet = sheet.number.oasis)

### Cleaning Sheet Name (CHAGE DATA HERE TOO)
sheet.name <- 
  excel_sheets('AdultFSPDataFY2021 - Copy.LC.xlsx')[sheet.number.adult]
sheet.name <- gsub("\\(.*", "", sheet.name)
sheet.name <- gsub("\\ .*", "", sheet.name)

### Check sheet names match
sheet.name.o <- 
  excel_sheets('OASIS_FSPDataFY2021-Copy.LC.xlsx')[sheet.number.oasis]
sheet.name.o <- gsub("\\(.*", "", sheet.name.o)
sheet.name.o <- gsub("\\ .*", "", sheet.name.o)



### Cleaning variable names and Program Names for adult data
df.a <- dplyr::rename(df.a, MHSA_Age_Group = `MHSA Age Group`)

df.a$ProgramName <- gsub("-", " ", df.a$ProgramName)

var.count <- ncol(df.a)

df.a$prior <- df.a[var.count - 1] %>% pull(variable.names(df.a)[var.count - 1])

df.a$post <- df.a[var.count] %>% pull(variable.names(df.a)[var.count])

df.a$difference <- df.a$post - df.a$prior


df.a <- df.a %>% 
  select(ProgramName, MHSA_Age_Group, prior, post, difference)

### Cleaning variable names and Progam Names for oasis data
df.o <- dplyr::rename(df.o, MHSA_Age_Group = `MHSA Age Group`)

df.o$ProgramName <- gsub("-", " ", df.o$ProgramName)

var.count <- ncol(df.o)

df.o$prior <- df.o[var.count - 1] %>% pull(variable.names(df.o)[var.count - 1])

df.o$post <- df.o[var.count] %>% pull(variable.names(df.o)[var.count])

df.o$difference <- df.o$post - df.o$prior


df.o <- df.o %>% 
  select(ProgramName, MHSA_Age_Group, prior, post, difference)

### Combine adult and oasis data set
df <- rbind(df.a, df.o)



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




