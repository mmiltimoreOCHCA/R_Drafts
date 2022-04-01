# Matthew Miltimore

#################################################
# Start-up
#################################################
if(!is.null(dev.list())) dev.off
cat("\014")
rm(list = ls())

# Packages 
library(readxl)
library(dplyr)
library(plotrix)
library(ggplot2)
library(stargazer)
library(rstatix)
library(tidyr)
library(scales)
library(ggrepel)

# Set working directory
setwd('/Users/Matthew Miltimore/Documents/R Docs/FYPanalysis')

# Cleaning Data/ Making data sheets

for (i in 1:9){
  
  sheet.number.1819 <- i+3
  
  sheet.number.1920 <- sheet.number.1819 - 1
  
  sheet.number.2021 <- sheet.number.1819 + 1
  
  sheet.number.1819.o <- sheet.number.1920
  
  sheet.number.1920.o <- sheet.number.1920
  
  sheet.number.2021.o <- sheet.number.1920
  
  # Children
  sheet.number.1920.choc <- sheet.number.1819 - 2
  
  sheet.number.2021.choc <- sheet.number.1819 - 2
  
  sheet.number.1920.kcs <- sheet.number.1819 - 2
  
  sheet.number.2021.kcs <- sheet.number.1819 - 2
  
  sheet.number.1920.ocapica <- sheet.number.1819 - 2
  
  sheet.number.2021.ocapica <- sheet.number.1819 - 2
  
  sheet.number.1920.ocf <- sheet.number.1819 - 2
  
  sheet.number.1920.pr <- sheet.number.1819 - 2
  
  sheet.number.2021.pr <- sheet.number.1819 - 2
  
  sheet.number.1920.ps <- sheet.number.1819 - 2
  
  sheet.number.2021.ps <- sheet.number.1819 - 2
  
  sheet.number.1920.wc <- sheet.number.1819 - 2
  
  sheet.number.2021.wc <- sheet.number.1819 - 2
  
  sheet.number.1920.wy <- sheet.number.1819 - 2
  
  sheet.number.2021.wy <- sheet.number.1819 - 2
  
  # Loading Data
  df.1819 <- read_excel('AdultFSPDataFY1819 - Copy.LC.xlsx',
                        sheet = sheet.number.1819)
  comment(df.1819) <- '18/19'
  
  df.1920 <- read_excel('AdultFSPDataFY1920 - Copy.LC.xlsx',
                        sheet = sheet.number.1920)
  comment(df.1920) <-'19/20'
  
  df.2021 <- read_excel('AdultFSPDataFY2021 - Copy.LC.xlsx',
                        sheet = sheet.number.2021)
  comment(df.2021) <- '20/21'
  
  df.1819.o <- read_excel('OASISFSPDataFY1819.LC.xlsx',
                          sheet = sheet.number.1819.o)
  comment(df.1819.o) <- '18/19'
  
  df.1920.o <- read_excel('OASISFSPDataFY1920-Copy.LC.xlsx',
                          sheet = sheet.number.1920.o)
  comment(df.1920.o) <-'19/20'
  
  df.2021.o <- read_excel('OASIS_FSPDataFY2021-Copy.LC.xlsx',
                          sheet = sheet.number.2021.o)
  comment(df.2021.o) <- '20/21'
  
  # Loading Children's (Not Education)
  if (i <= 8) {
    
    df.1920.choc <- read_excel('CHOCFSP_FY19-20_Complete.xlsx',
                               sheet = sheet.number.1920.choc)
    comment(df.1920.choc) <- '19/20'
    
    df.2021.choc <- read_excel('CHOCFSP_FY20-21_Complete.xlsx',
                               sheet = sheet.number.2021.choc)
    comment(df.2021.choc) <- '20/21'
    
    df.1920.kcs <- read_excel('KCS_FY19-20_Complete.xlsx',
                               sheet = sheet.number.1920.kcs)
    comment(df.1920.kcs) <- '19/20'
    
    df.2021.kcs <- read_excel('KCSProjectFSP_FY2021_Complete.xlsx',
                              sheet = sheet.number.2021.kcs)
    comment(df.2021.kcs) <- '20/21'
    
    df.1920.ocapica <- read_excel('OCAPICAFSP_FY19-20_Complete.xlsx',
                              sheet = sheet.number.1920.ocapica)
    comment(df.1920.ocapica) <- '19/20'
    
    df.2021.ocapica <- read_excel('OCAPICAProjectFSP_FY2021_Complete.xlsx',
                                  sheet = sheet.number.2021.ocapica)
    comment(df.2021.ocapica) <- '20/21'
    
    df.1920.ocf <- read_excel('OrangewoodCCFSP_FY19-20_Complete.xlsx',
                                  sheet = sheet.number.1920.ocf)
    comment(df.1920.ocf) <- '19/20'
    
    df.1920.pr <- read_excel('PathwaysRenewFSP_FY1920_Complete.xlsx',
                              sheet = sheet.number.1920.pr)
    comment(df.1920.pr) <- '19/20'
    
    df.2021.pr <- read_excel('PathwaysRenewFSP_FY2021_Complete.xlsx',
                             sheet = sheet.number.2021.pr)
    comment(df.2021.pr) <- '20/21'
    
    df.1920.ps <- read_excel('PathwaysSTAYFSP_FY1920_Complete.xlsx',
                             sheet = sheet.number.1920.ps)
    comment(df.1920.ps) <- '19/20'
    
    df.2021.ps <- read_excel('PathwaysSTAYFSP_FY2021_Complete.xlsx',
                             sheet = sheet.number.2021.ps)
    comment(df.2021.ps) <- '20/21'
    
    df.1920.wc <- read_excel('Waymakers CCFSP_FY19-20_Complete.xlsx',
                             sheet = sheet.number.1920.wc)
    comment(df.1920.wc) <- '19/20'
    
    df.2021.wc <- read_excel('WaymakersCCFSP_FY2021_Complete.xlsx',
                             sheet = sheet.number.2021.wc)
    comment(df.2021.wc) <- '20/21'
    
    df.1920.wy <- read_excel('Waymakers YOWFSP_FY19-20_Complete.xlsx',
                             sheet = sheet.number.1920.wy)
    comment(df.1920.wy) <- '19/20'
    
    df.2021.wy <- read_excel('WaymakersYOWFSP_FY2021_Complete.xlsx',
                             sheet = sheet.number.2021.wy)
    comment(df.2021.wy) <- '20/21'
    
  }
  
  
  # Cleaning Sheet Name
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[sheet.number.1819]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  
  
  # Combine and clean data
  if (i <= 8){
    
    list.df <- list(df.1819, df.1819.o, df.1920, df.1920.o, df.2021, df.2021.o,
                    df.1920.choc, df.2021.choc, df.1920.kcs, df.2021.kcs,
                    df.1920.ocapica, df.2021.ocapica, df.1920.ocf,
                    df.1920.pr, df.2021.pr, df.1920.ps, df.2021.ps,
                    df.1920.wc, df.2021.wc, df.1920.wy, df.2021.wy)
    
  } else {
    
    list.df <- list(df.1819, df.1819.o, df.1920, df.1920.o, df.2021, df.2021.o)
  }
  
  
  list.df <- lapply(list.df, function(x){
    x <- dplyr::rename(x, MHSA_Age_Group = `MHSA Age Group`)
    
    x$MHSA_Age_Group <- factor(x$MHSA_Age_Group, 
                               levels = c('Child',
                                          'TAY',
                                          'Adult',
                                          'Older Adult'))
    
    x <- dplyr::rename(x, Race_Ethnicity = `Race/Ethnicity`)
    
    x$ProgramName <- gsub("-", " ", x$ProgramName)
    
    var.count <- ncol(x)
    
    x$prior <- x[var.count - 1] %>% 
      pull(variable.names(x)[var.count - 1])
    
    x$post <- x[var.count] %>% 
      pull(variable.names(x)[var.count])
    
    # Check/fix for 'Education'
    
    if(sheet.name == 'Education'){
      
      x$post <- replace(x$post, x$post > 1, 1)
      
    }
    
    x$difference <- x$post - x$prior
    
    x$year <- comment(x)
    
    x <- x %>% 
      select(ProgramName,
             ClientID,
             MHSA_Age_Group,
             Race_Ethnicity,
             year,
             prior, 
             post, 
             difference)
  })
  
  df <- do.call("rbind", list.df)
  
  # Remove NA
  df <- na.omit(df)
  
  # Recode ProgramName
  df.recode <- read_xlsx("FSPrecode.xlsx", sheet = 1)
  
  recode_count <- length(df.recode$`Old Program Name`)
  
  for (i in 1:recode_count){
    
    old.name <- as.character(df.recode[i,1])
    
    new.name <- as.character(df.recode[i,2])
    
    df$ProgramName[df$ProgramName == old.name] <- new.name 
  }
  
  # Recode Race/Ethnicity
  df.recode <- read_xlsx("FSPrecodeRACE.xlsx", sheet = 1)
  
  recode_count <- length(df.recode$Old_Name)
  
  for (i in 1:recode_count){
    
    old.name <- as.character(df.recode[i,1])
    
    new.name <- as.character(df.recode[i,2])
    
    df$Race_Ethnicity[df$Race_Ethnicity == old.name] <- new.name 
  }
  
  # Add Program Age Group
  df.recode <- read_xlsx("FSPrecodePROGAGE.xlsx", sheet = 1)
  
  recode_count <- length(df.recode$ProgramName)
  
  for (i in 1:recode_count){
    
    prog.name <- as.character(df.recode[i,1])
    
    prog.ag <- as.character(df.recode[i,2])
    
    df$ProgAgeGroup[df$ProgramName == prog.name] <- prog.ag 
  }
  
  
  df$ProgAgeGroup <- factor(df$ProgAgeGroup,
                            levels = c('Child FSP',
                                       'TAY FSP',
                                       'Adult FSP',
                                       'Older Adult FSP'))
  
  # Fix TAY/Child FSP
  df$ProgAgeGroup <- replace(df$ProgAgeGroup, 
                             (df$ProgAgeGroup == 'Child FSP') &
                               (df$ProgramName != 'Pathways RENEW') &
                               (df$MHSA_Age_Group == 'TAY'),
                             'TAY FSP')
  
  # Name Data
  assign(paste0('df.', sheet.name), df)
  
}

# List of outcome data frames
list.df.out <- list(df.Hospitalizations, 
                    df.Jail,
                    df.Homelessness,
                    df.EmergShelt,
                    df.EmergencyInterventions,
                    df.IndepLivingDays,
                    df.Arrests,
                    df.Employment,
                    df.Education)

# Test stuff
# Loading Count Data
########
# Adult
df.1819 <- read_excel('AdultFSPDataFY1819 - Copy.LC.xlsx',
                      sheet = 2)
df.1819 <- df.1819 %>% select(ProgramName, ClientID, `Age at 7/1/2018`)
comment(df.1819) <- '18/19'

df.1920 <- read_excel('AdultFSPDataFY1920 - Copy.LC.xlsx',
                      sheet = 2)
df.1920 <- df.1920 %>% select(ProgramName, ClientID, `Age at 7/1/2019`)
comment(df.1920) <- '19/20'

df.2021 <- read_excel('AdultFSPDataFY2021 - Copy.LC.xlsx',
                      sheet = 4)
df.2021 <- df.2021 %>% select(ProgramName, ClientID, `Age as of 7/1/2020`)
comment(df.2021) <- '20/21'

#Older Adut
df.1819.o <- read_excel('OASISFSPDataFY1819.LC.xlsx',
                        sheet = 2)
df.1819.o <- df.1819.o %>% select(ProgramName, ClientID, `Age on 7/1/2018`)
comment(df.1819.o) <- '18/19'

df.1920.o <- read_excel('OASISFSPDataFY1920-Copy.LC.xlsx',
                        sheet = 2)
df.1920.o <- df.1920.o %>% select(ProgramName, ClientID, `Age at 7/1/2019`)
comment(df.1920.o) <- '19/20'

df.2021.o <- read_excel('OASIS_FSPDataFY2021-Copy.LC.xlsx',
                        sheet = 2)
df.2021.o <- df.2021.o %>% select(ProgramName, ClientID, `Age at 7/1/2020`)
comment(df.2021.o) <- '20/21'

#Children/TAY
# CHOC
df.1920.choc <- read_excel('CHOCFSP_FY19-20_Complete.xlsx',
                           sheet = 1)
df.1920.choc <- df.1920.choc %>% select(ProgramName, ClientID, `Ageon7/1/19`)
comment(df.1920.choc) <- '19/20'

df.2021.choc <- read_excel('CHOCFSP_FY20-21_Complete.xlsx',
                           sheet = 1)
df.2021.choc <- df.2021.choc %>% 
  select(ProgramName, ClientID, `Ageon7/1/2019`) %>%
  rename(`Ageon7/1/2020` = `Ageon7/1/2019`) %>%
  mutate(`Ageon7/1/2020` = `Ageon7/1/2020` + 1)
comment(df.2021.choc) <- '20/21'

#KCS
df.1920.kcs <- read_excel('KCS_FY19-20_Complete.xlsx',
                          sheet = 1)
df.1920.kcs <- df.1920.kcs %>% select(ProgramName, ClientID, `Ageon7/1/2019`)
comment(df.1920.kcs) <- '19/20'

df.2021.kcs <- read_excel('KCSProjectFSP_FY2021_Complete.xlsx',
                          sheet = 1)
df.2021.kcs <- df.2021.kcs %>% select(ProgramName, ClientID, `Ageon7/1/2020`)
comment(df.2021.kcs) <- '20/21'

#OCAPICA
df.1920.ocapica <- read_excel('OCAPICAFSP_FY19-20_Complete.xlsx',
                              sheet = 1)
df.1920.ocapica <- df.1920.ocapica %>% select(ProgramName, ClientID, `Ageat7/1/2019`)
comment(df.1920.ocapica) <- '19/20'

df.2021.ocapica <- read_excel('OCAPICAProjectFSP_FY2021_Complete.xlsx',
                              sheet = 1)
df.2021.ocapica <- df.2021.ocapica %>% select(ProgramName, ClientID, `Ageon7/1/2020`)
comment(df.2021.ocapica) <- '20/21'

#Orangewood
df.1920.ocf <- read_excel('OrangewoodCCFSP_FY19-20_Complete.xlsx',
                          sheet = 1)
df.1920.ocf <- df.1920.ocf %>% select(ProgramName, ClientID, `Ageat7/1/2019`)
comment(df.1920.ocf) <- '19/20'

#Renew
df.1920.pr <- read_excel('PathwaysRenewFSP_FY1920_Complete.xlsx',
                         sheet = 1)
df.1920.pr <- df.1920.pr %>% select(ProgramName, ClientID, `Ageon7/1/2019`)
comment(df.1920.pr) <- '19/20'

df.2021.pr <- read_excel('PathwaysRenewFSP_FY2021_Complete.xlsx',
                         sheet = 1)
df.2021.pr <- df.2021.pr %>% select(ProgramName, ClientID, `Ageon7/1/2020`)
comment(df.2021.pr) <- '20/21'

#STAY
df.1920.ps <- read_excel('PathwaysSTAYFSP_FY1920_Complete.xlsx',
                         sheet = 1)
df.1920.ps <- df.1920.ps %>% select(ProgramName, ClientID, `Ageon7/1/19`)
comment(df.1920.ps) <- '19/20'

df.2021.ps <- read_excel('PathwaysSTAYFSP_FY2021_Complete.xlsx',
                         sheet = 1)
df.2021.ps <- df.2021.ps %>% select(ProgramName, ClientID, `Ageon7/1/20`)
comment(df.2021.ps) <- '20/21'

#WC
df.1920.wc <- read_excel('Waymakers CCFSP_FY19-20_Complete.xlsx',
                         sheet = 1)
df.1920.wc <- df.1920.wc %>% select(ProgramName, ClientID, `Ageat7/1/2019`)
comment(df.1920.wc) <- '19/20'

df.2021.wc <- read_excel('WaymakersCCFSP_FY2021_Complete.xlsx',
                         sheet = 1)
df.2021.wc <- df.2021.wc %>% 
  select(ProgramName, ClientID, `Ageat7/1/2020`) %>%
  mutate(`Ageat7/1/2020` = round(`Ageat7/1/2020`))
comment(df.2021.wc) <- '20/21'

#WY
df.1920.wy <- read_excel('Waymakers YOWFSP_FY19-20_Complete.xlsx',
                         sheet = 1)
df.1920.wy <- df.1920.wy %>% select(ProgramName, ClientID, `Ageon7/1/2019`)
comment(df.1920.wy) <- '19/20'

df.2021.wy <- read_excel('WaymakersYOWFSP_FY2021_Complete.xlsx',
                         sheet = 1)
df.2021.wy <- df.2021.wy %>% select(ProgramName, ClientID, `Ageon7/1/2020`)
comment(df.2021.wy) <- '20/21'

########
#Clean Count Data
########
list.df <- list(df.1819, df.1819.o, df.1920, df.1920.o, df.2021, df.2021.o,
                df.1920.choc, df.2021.choc, df.1920.kcs, df.2021.kcs,
                df.1920.ocapica, df.2021.ocapica, df.1920.ocf,
                df.1920.pr, df.2021.pr, df.1920.ps, df.2021.ps,
                df.1920.wc, df.2021.wc, df.1920.wy, df.2021.wy)

list.df <- lapply(list.df, function(x){
  
  x$AgeStartFY <- x[3] %>% 
    pull(variable.names(x)[3])
  
  x$year <- comment(x)
  
  x <- x %>%
    mutate(AgeEndFY = as.numeric(AgeStartFY) + 1)
  
  x <- x %>%
    mutate(MHSA_Age_GroupStart = case_when(AgeStartFY < 16 ~ 'Child',
                                           AgeStartFY < 26 ~ 'TAY',
                                           AgeStartFY < 60 ~ 'Adult',
                                           AgeStartFY >= 60 ~ 'Older Adult'),
           MHSA_Age_GroupEnd = case_when(AgeEndFY < 16 ~ 'Child',
                                         AgeEndFY < 26 ~ 'TAY',
                                         AgeEndFY < 60 ~ 'Adult',
                                         AgeEndFY >= 60 ~ 'Older Adult'))
  
  x <- x %>% 
    select(ProgramName,
           ClientID,
           year,
           AgeStartFY,
           AgeEndFY,
           MHSA_Age_GroupStart,
           MHSA_Age_GroupEnd)
  
})

df.Count <- do.call("rbind", list.df)

# Recode ProgramName
df.recode <- read_xlsx("FSPrecode.xlsx", sheet = 1)

recode_count <- length(df.recode$`Old Program Name`)

for (i in 1:recode_count){
  
  old.name <- as.character(df.recode[i,1])
  
  new.name <- as.character(df.recode[i,2])
  
  df.Count$ProgramName[df.Count$ProgramName == old.name] <- new.name 
}

# Add Program Age Group
df.recode <- read_xlsx("FSPrecodePROGAGE.xlsx", sheet = 1)

recode_count <- length(df.recode$ProgramName)

for (i in 1:recode_count){
  
  prog.name <- as.character(df.recode[i,1])
  
  prog.ag <- as.character(df.recode[i,2])
  
  df.Count$ProgAgeGroupStart[df.Count$ProgramName == prog.name] <- prog.ag 
}

df.Count$ProgAgeGroupStart <- factor(df.Count$ProgAgeGroupStart,
                                     levels = c('Child FSP',
                                                'TAY FSP',
                                                'Adult FSP',
                                                'Older Adult FSP'))

# Fix TAY/Child FSP
df.Count$ProgAgeGroupStart <- replace(
  df.Count$ProgAgeGroupStart,
  (df.Count$ProgAgeGroupStart == 'Child FSP') &
    (df.Count$ProgramName != 'Pathways RENEW') &
    (df.Count$MHSA_Age_GroupStart == 'TAY'),
  'TAY FSP')

# Add Program Age Group (END)
for (i in 1:recode_count){
  
  prog.name <- as.character(df.recode[i,1])
  
  prog.ag <- as.character(df.recode[i,2])
  
  df.Count$ProgAgeGroupEnd[df.Count$ProgramName == prog.name] <- prog.ag 
}

df.Count$ProgAgeGroupEnd <- factor(df.Count$ProgAgeGroupEnd,
                                     levels = c('Child FSP',
                                                'TAY FSP',
                                                'Adult FSP',
                                                'Older Adult FSP'))

# Fix TAY/Child FSP
df.Count$ProgAgeGroupEnd <- replace(
  df.Count$ProgAgeGroupEnd,
  (df.Count$ProgAgeGroupEnd == 'Child FSP') &
    (df.Count$ProgramName != 'Pathways RENEW') &
    (df.Count$MHSA_Age_GroupEnd == 'TAY'),
  'TAY FSP')

########
table(df.Hospitalizations$ProgAgeGroup, df.Hospitalizations$year)
table(df.Count$ProgAgeGroupStart, df.Count$year)
table(df.Count$ProgAgeGroupEnd, df.Count$year)





########
# Count Graphs 
########

t1 <- table(df.Hospitalizations$year, df.Hospitalizations$MHSA_Age_Group)

t2 <- table(df.Hospitalizations$year, df.Hospitalizations$ProgAgeGroup)

d1 <- as.data.frame(t1)
d1 <- d1[(d1$Freq > 200),]

ggplot(data = d1,
       aes(x = Var1,
           y = Freq,
           group = Var2,
           label = Freq)) +
  geom_line(aes(color = Var2)) +
  geom_point() +
  geom_text_repel(size = 3) +
  ylim(0, 1500) +
  guides(color = guide_legend(title = "Age Group")) +
  labs(x = 'Fiscal Year',
       y = '',
       title = 'Members Served by Age Group at Start of FY')

ggsave(filename = 'Served-Age-FY.png',
       width = 6, height = 4, units = 'in')

d2 <- as.data.frame(t2)

ggplot(data = d2,
       aes(x = Var1,
           y = Freq,
           group = Var2,
           label = Freq)) +
  geom_line(aes(color = Var2)) +
  geom_point() +
  geom_text_repel(size = 3) +
  ylim(0.1, 1500) +
  guides(color = guide_legend(title = "FSP Age Group")) +
  labs(x = 'Fiscal Year',
       y = '',
       title = 'Members Served by FSP and FY')

ggsave(filename = 'Served-FSP-FY.png',
       width = 6, height = 4, units = 'in')


#################################################
# Step 1: One-Way Analysis of Variance
#################################################

########
# Variance by Program Name 
########

## Prior ##
### Summary Tables
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  df.summary <- df %>% group_by(ProgramName) %>%
    summarise(Minimum = min(prior),
              Maximum = max(prior),
              Mean = mean(prior))
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  stargazer(df.summary, 
            summary = FALSE, 
            type = 'text',
            title = paste0(sheet.name, '-Prior'))
  
}

### Models (Kruskal Test)
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  print(kruskal.test(prior ~ ProgramName, data = df))
  
}

### Kruskal Effect size
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  print(kruskal_effsize(df, prior ~ ProgramName))
  
}

### Models (Pairwise Wilcox)
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  print(pairwise.wilcox.test(df$prior, df$ProgramName, p.adjust.method = "BH"))
  
}

### Plot Prior Means by Program Name (NO Child/TAY FSP)
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  # Remove Child and TAY FSP
  df <- df[(df$ProgAgeGroup != 'Child FSP' & df$ProgAgeGroup != 'TAY FSP'),]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  # Summarize
  data_summary <- group_by(df, ProgramName) %>%
    summarise(Out_mean = mean(prior),
              Out_se = std.error(prior),
              Count = n()) %>%
    arrange(desc(Out_mean))
  
  # Re-order ProgramName
  data_summary$ProgramName <- 
    factor(data_summary$ProgramName, levels = c('OASIS Enrolled',
                                                'TAO North',
                                                'TAO Central',
                                                'TAO South',
                                                'Home First FSP',
                                                'Telecare STEPs',
                                                'Telecare AOT',
                                                'Telecare STEPs Collaborative Court',
                                                'OK Enrollment / FSP',
                                                'Telecare WIT'))
  
  data_summary <- data_summary[order(data_summary$ProgramName),]
  
  # Add Program_Count variable
  data_summary$Program_Count <- paste0(data_summary$ProgramName,
                                       ' ', 
                                       '(n = ',
                                       data_summary$Count,
                                       ')')
  
  # Match Factor level of ProgramName and Program_Count
  data_summary$Program_Count <- 
    factor(data_summary$Program_Count, 
           levels = c(
             paste0('Oasis Enrolled (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'OASIS Enrolled', 
                                   4]),
                    ')'),
             paste0('TAO North (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'TAO North',
                                   4]),
                    ')'),
             paste0('TAO Central (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'TAO Central',
                                   4]),
                    ')'),
             paste0('TAO South (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'TAO South',
                                   4]),
                    ')'),
             paste0('Home First FSP (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'Home First FSP',
                                   4]),
                    ')'),
             paste0('Telecare STEPs (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'Telecare STEPs',
                                   4]),
                    ')'),
             paste0('Telecare AOT (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'Telecare AOT',
                                   4]),
                    ')'),
             paste0('Telecare STEPs Collaborative Court (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'Telecare STEPs Collaborative Court',
                                   4]),
                    ')'),
             paste0('OK Enrollment / FSP (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'OK Enrollment / FSP',
                                   4]),
                    ')'),
             paste0('Telecare WIT (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'Telecare WIT',
                                   4]),
                    ')')))
  
  data_summary$Program_Count <- replace(
    data_summary$Program_Count, c(1), levels(data_summary$Program_Count)[1])
  
  
  # Plot
  plot <- ggplot(data_summary, 
         aes(x = ProgramName,
             y = Out_mean,
             group = ProgramName)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = Program_Count), 
             show.legend = TRUE) +
    scale_fill_manual(values = c("red1",
                                 "orange1", 'orange2', 'orange3',
                                 'gold1',
                                 'forestgreen', 'darkgreen',
                                 'blue1', 'blue2', 'blue3')) +
    geom_errorbar(aes(ymin = Out_mean - Out_se, 
                      ymax = Out_mean + Out_se),
                  width = 0.1, 
                  position=position_dodge(0.9)) +
    xlab('') +
    ylab('Mean') +
    labs(fill = 'Program Name') + 
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) +
    ggtitle(paste('Average Prior', sheet.name,'by Program')) +
    coord_flip() +
    annotate('text', 
             x = 1.25,
             y = (max(data_summary$Out_mean) + .3*max(data_summary$Out_mean)),
             size = 3,
             color = 'red1',
             label = "") +
    annotate('text', 
             x = 1.25,
             y = (max(data_summary$Out_mean) + .17*max(data_summary$Out_mean)),
             size = 3,
             color = 'red1',
             label = "General") +
    annotate('text', 
             x = 3.25,
             y = (max(data_summary$Out_mean) + .17*max(data_summary$Out_mean)),
             size = 3,
             color = 'orange2',
             label = "Homeless") +
    annotate('text', 
             x = 5.25,
             y = (max(data_summary$Out_mean) + .175*max(data_summary$Out_mean)),
             size = 3,
             color = 'gold1',
             label = "Homeless Prevention") +
    annotate('text', 
             x = 7.25,
             y = (max(data_summary$Out_mean) + .17*max(data_summary$Out_mean)),
             size = 3,
             color = 'darkgreen',
             label = "Intensive Treatment") +
    annotate('text', 
             x = 9.25,
             y = (max(data_summary$Out_mean) + .17*max(data_summary$Out_mean)),
             size = 3,
             color = 'blue2',
             label = "Justice Involved") +
    guides(fill = guide_legend(reverse = TRUE))
  
  print(plot)
  
  ggsave(filename = paste0('MeanPriorProg_',
                           sheet.name,
                           '.png'),
         width = 8, height = 4, units = 'in')

} 
# ggsave Export 4x8in

### Plot Prior Means by Program Name (Just CHILD/TAY)
for (i in 1:8){
  
  df <- list.df.out[[i]]
  
  # Remove all but Child and TAY FSP
  df <- df[(df$ProgAgeGroup != 'Adult FSP' & df$ProgAgeGroup != 'Older Adult FSP'),]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  # Summarize
  data_summary <- group_by(df, ProgramName) %>%
    summarise(Out_mean = mean(prior),
              Out_se = std.error(prior),
              Count = n()) %>%
    arrange(desc(Out_mean))
  
  # Make Race/Ethnicity a fact
  data_summary$ProgramName <- as.factor(data_summary$ProgramName)
  
  # Add Group_Count variable
  data_summary$Group_Count <- paste0(data_summary$ProgramName,
                                     ' ', 
                                     '(n = ',
                                     data_summary$Count,
                                     ')')
  # Make Group_Count a factor
  data_summary$Group_Count <- as.factor(data_summary$Group_Count)
  
  
  # Plot
  plot <- ggplot(data_summary, 
                 aes(x = Group_Count,
                     y = Out_mean,
                     group = Group_Count)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = Group_Count), 
             show.legend = TRUE) +
    geom_errorbar(aes(ymin = Out_mean - Out_se, 
                      ymax = Out_mean + Out_se),
                  width = 0.1, 
                  position=position_dodge(0.9)) +
    xlab('') +
    ylab('Mean') +
    labs(fill = 'Program Name') + 
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) +
    ggtitle(paste('Average Prior', sheet.name,'by Program')) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))
  
  print(plot)
  
  #ggsave(filename = paste0('MeanPriorProgCHILD_',
  #sheet.name,
  #'.png'),
  #width = 8, height = 5, units = 'in')
}

### Plot Prior Means by Program Name
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  # Summarize
  data_summary <- group_by(df, ProgramName) %>%
    summarise(Out_mean = mean(prior),
              Out_se = std.error(prior),
              Count = n()) %>%
    arrange(desc(Out_mean))
  
  # Make Race/Ethnicity a fact
  data_summary$ProgramName <- as.factor(data_summary$ProgramName)
  
  # Add Group_Count variable
  data_summary$Group_Count <- paste0(data_summary$ProgramName,
                                     ' ', 
                                     '(n = ',
                                     data_summary$Count,
                                     ')')
  # Make Group_Count a factor
  data_summary$Group_Count <- as.factor(data_summary$Group_Count)
  
  
  # Plot
  plot <- ggplot(data_summary, 
                 aes(x = Group_Count,
                     y = Out_mean,
                     group = Group_Count)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = Group_Count), 
             show.legend = TRUE) +
    geom_errorbar(aes(ymin = Out_mean - Out_se, 
                      ymax = Out_mean + Out_se),
                  width = 0.1, 
                  position=position_dodge(0.9)) +
    xlab('') +
    ylab('Mean') +
    labs(fill = 'Program Name') + 
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) +
    ggtitle(paste('Average Prior', sheet.name,'by Program')) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))
  
  print(plot)
  
  #ggsave(filename = paste0('MeanPriorProgCHILD_',
                          #sheet.name,
                           #'.png'),
         #width = 8, height = 5, units = 'in')
}

## Post ##
### Summary Tables
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  df.summary <- df %>% group_by(ProgramName) %>%
    summarise(Minimum = min(post),
              Maximum = max(post),
              Mean = mean(post))
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  stargazer(df.summary, 
            summary = FALSE, 
            type = 'text',
            title = paste0(sheet.name, '-Post'))
  
}

### Models (Kruskal Test)
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  print(kruskal.test(post ~ ProgramName, data = df))
  
}

### Kruskal Effect size
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  print(kruskal_effsize(df, post ~ ProgramName))
  
}

### Models (Pairwise Wilcox)
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  print(pairwise.wilcox.test(df$post, df$ProgramName, p.adjust.method = "BH"))
  
}

### Plot Post Means by Program Name (No Child/TAY FSP)
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  # Remove Child and TAY FSP
  df <- df[(df$ProgAgeGroup != 'Child FSP' & df$ProgAgeGroup != 'TAY FSP'),]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  # Summarize
  data_summary <- group_by(df, ProgramName) %>%
    summarise(Out_mean = mean(post),
              Out_se = std.error(post),
              Count = n()) %>%
    arrange(desc(Out_mean))
  
  # Re-order ProgramName
  data_summary$ProgramName <- 
    factor(data_summary$ProgramName, levels = c('OASIS Enrolled',
                                                'TAO North',
                                                'TAO Central',
                                                'TAO South',
                                                'Home First FSP',
                                                'Telecare STEPs',
                                                'Telecare AOT',
                                                'Telecare STEPs Collaborative Court',
                                                'OK Enrollment / FSP',
                                                'Telecare WIT'))
  
  data_summary <- data_summary[order(data_summary$ProgramName),]
  
  # Add Program_Count variable
  data_summary$Program_Count <- paste0(data_summary$ProgramName,
                                       ' ', 
                                       '(n = ',
                                       data_summary$Count,
                                       ')')
  
  # Match Factor level of ProgramName and Program_Count
  data_summary$Program_Count <- 
    factor(data_summary$Program_Count, 
           levels = c(
             paste0('Oasis Enrolled (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'OASIS Enrolled', 
                                   4]),
                    ')'),
             paste0('TAO North (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'TAO North',
                                   4]),
                    ')'),
             paste0('TAO Central (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'TAO Central',
                                   4]),
                    ')'),
             paste0('TAO South (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'TAO South',
                                   4]),
                    ')'),
             paste0('Home First FSP (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'Home First FSP',
                                   4]),
                    ')'),
             paste0('Telecare STEPs (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'Telecare STEPs',
                                   4]),
                    ')'),
             paste0('Telecare AOT (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'Telecare AOT',
                                   4]),
                    ')'),
             paste0('Telecare STEPs Collaborative Court (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'Telecare STEPs Collaborative Court',
                                   4]),
                    ')'),
             paste0('OK Enrollment / FSP (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'OK Enrollment / FSP',
                                   4]),
                    ')'),
             paste0('Telecare WIT (n = ',
                    as.character(
                      data_summary[data_summary$ProgramName == 'Telecare WIT',
                                   4]),
                    ')')))
  
  data_summary$Program_Count <- replace(
    data_summary$Program_Count, c(1), levels(data_summary$Program_Count)[1])
  
  
  # Plot
  plot <- ggplot(data_summary, 
                 aes(x = ProgramName,
                     y = Out_mean,
                     group = ProgramName)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = Program_Count), 
             show.legend = TRUE) +
    scale_fill_manual(values = c("red1",
                                 "orange1", 'orange2', 'orange3',
                                 'gold1',
                                 'forestgreen', 'darkgreen',
                                 'blue1', 'blue2', 'blue3')) +
    geom_errorbar(aes(ymin = Out_mean - Out_se, 
                      ymax = Out_mean + Out_se),
                  width = 0.1, 
                  position=position_dodge(0.9)) +
    xlab('') +
    ylab('Mean') +
    labs(fill = 'Program Name') + 
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) +
    ggtitle(paste('Average Post', sheet.name,'by Program')) +
    coord_flip() +
    annotate('text', 
             x = 1.25,
             y = (max(data_summary$Out_mean) + .3*max(data_summary$Out_mean)),
             size = 3,
             color = 'red1',
             label = "") +
    annotate('text', 
             x = 1.25,
             y = (max(data_summary$Out_mean) + .17*max(data_summary$Out_mean)),
             size = 3,
             color = 'red1',
             label = "General") +
    annotate('text', 
             x = 3.25,
             y = (max(data_summary$Out_mean) + .17*max(data_summary$Out_mean)),
             size = 3,
             color = 'orange2',
             label = "Homeless") +
    annotate('text', 
             x = 5.25,
             y = (max(data_summary$Out_mean) + .175*max(data_summary$Out_mean)),
             size = 3,
             color = 'gold1',
             label = "Homeless Prevention") +
    annotate('text', 
             x = 7.25,
             y = (max(data_summary$Out_mean) + .17*max(data_summary$Out_mean)),
             size = 3,
             color = 'darkgreen',
             label = "Intensive Treatment") +
    annotate('text', 
             x = 9.25,
             y = (max(data_summary$Out_mean) + .17*max(data_summary$Out_mean)),
             size = 3,
             color = 'blue2',
             label = "Justice Involved") +
    guides(fill = guide_legend(reverse = TRUE))
  
  print(plot)
  
  ggsave(filename = paste0('MeanPostProg_',
                           sheet.name,
                           '.png'),
         width = 8, height = 4, units = 'in')
}
# ggsave Export 4x8in

### Plot Post Means by Program Name (Just Child/TAY FSP)
for (i in 1:8){
  
  df <- list.df.out[[i]]
  
  # Just Child and TAY FSP
  df <- df[(df$ProgAgeGroup != 'Adult FSP' & df$ProgAgeGroup != 'Older ADult FSP'),]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  # Summarize
  data_summary <- group_by(df, ProgramName) %>%
    summarise(Out_mean = mean(post),
              Out_se = std.error(post),
              Count = n()) %>%
    arrange(desc(Out_mean))
  
  # Make Race/Ethnicity a fact
  data_summary$ProgramName <- as.factor(data_summary$ProgramName)
  
  # Add Group_Count variable
  data_summary$Group_Count <- paste0(data_summary$ProgramName,
                                     ' ', 
                                     '(n = ',
                                     data_summary$Count,
                                     ')')
  # Make Group_Count a factor
  data_summary$Group_Count <- as.factor(data_summary$Group_Count)
  
  
  # Plot
  plot <- ggplot(data_summary, 
                 aes(x = Group_Count,
                     y = Out_mean,
                     group = Group_Count)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = Group_Count), 
             show.legend = TRUE) +
    geom_errorbar(aes(ymin = Out_mean - Out_se, 
                      ymax = Out_mean + Out_se),
                  width = 0.1, 
                  position=position_dodge(0.9)) +
    xlab('') +
    ylab('Mean') +
    labs(fill = 'Program Name') + 
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) +
    ggtitle(paste('Average Post', sheet.name,'by Program')) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))
  
  print(plot)
  
  #ggsave(filename = paste0('MeanPriorProgCHILD_',
  #sheet.name,
  #'.png'),
  #width = 8, height = 5, units = 'in')
}

### Plot Post Means by Program Name
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  # Summarize
  data_summary <- group_by(df, ProgramName) %>%
    summarise(Out_mean = mean(post),
              Out_se = std.error(post),
              Count = n()) %>%
    arrange(desc(Out_mean))
  
  # Make Race/Ethnicity a fact
  data_summary$ProgramName <- as.factor(data_summary$ProgramName)
  
  # Add Group_Count variable
  data_summary$Group_Count <- paste0(data_summary$ProgramName,
                                     ' ', 
                                     '(n = ',
                                     data_summary$Count,
                                     ')')
  # Make Group_Count a factor
  data_summary$Group_Count <- as.factor(data_summary$Group_Count)
  
  
  # Plot
  plot <- ggplot(data_summary, 
                 aes(x = Group_Count,
                     y = Out_mean,
                     group = Group_Count)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = Group_Count), 
             show.legend = TRUE) +
    geom_errorbar(aes(ymin = Out_mean - Out_se, 
                      ymax = Out_mean + Out_se),
                  width = 0.1, 
                  position=position_dodge(0.9)) +
    xlab('') +
    ylab('Mean') +
    labs(fill = 'Program Name') + 
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) +
    ggtitle(paste('Average Post', sheet.name,'by Program')) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))
  
  print(plot)
  
  #ggsave(filename = paste0('MeanPriorProgCHILD_',
  #sheet.name,
  #'.png'),
  #width = 8, height = 5, units = 'in')
}

########
# Variance by Race/Ethnicity
########

## Prior ##
### Summary Tables
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  df.summary <- df %>% group_by(Race_Ethnicity) %>%
    summarise(Minimum = min(prior),
              Maximum = max(prior),
              Mean = mean(prior))
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  stargazer(df.summary, 
            summary = FALSE, 
            type = 'text',
            title = paste0(sheet.name, '-Prior'))
  
}

### Models (Kruskal Test)
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  print(kruskal.test(prior ~ Race_Ethnicity, data = df))
  
}

### Kruskal Effect size
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  print(kruskal_effsize(df, prior ~ Race_Ethnicity))
  
}

### Models (Pairwise Wilcox)
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  print(pairwise.wilcox.test(df$prior, 
                             df$Race_Ethnicity, 
                             p.adjust.method = "BH"))
  
}

### Plot Prior Means by Race/Ethnicity 
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  # Summarize
  data_summary <- group_by(df, Race_Ethnicity) %>%
    summarise(Out_mean = mean(prior),
              Out_se = std.error(prior),
              Count = n()) %>%
    arrange(desc(Out_mean))
  
  # Make Race/Ethnicity a fact
  data_summary$Race_Ethnicity <- as.factor(data_summary$Race_Ethnicity)
  
  # Add Group_Count variable
  data_summary$Group_Count <- paste0(data_summary$Race_Ethnicity,
                                       ' ', 
                                       '(n = ',
                                       data_summary$Count,
                                       ')')
  # Make Group_Count a factor
  data_summary$Group_Count <- as.factor(data_summary$Group_Count)
  
  
  # Plot
  plot <- ggplot(data_summary, 
                 aes(x = Group_Count,
                     y = Out_mean,
                     group = Group_Count)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = Group_Count), 
             show.legend = TRUE) +
    geom_errorbar(aes(ymin = Out_mean - Out_se, 
                      ymax = Out_mean + Out_se),
                  width = 0.1, 
                  position=position_dodge(0.9)) +
    xlab('') +
    ylab('Mean') +
    labs(fill = 'Race/Ethnicity') + 
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) +
    ggtitle(paste('Average Prior', sheet.name,'by Race/Ethnicity')) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))
  
  print(plot)
  
  ggsave(filename = paste0('MeanPriorRace_',
                           sheet.name,
                           '.png'),
         width = 8, height = 4, units = 'in')
}
# ggsave Export 4x8in

## Post ##
### Summary Tables
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  df.summary <- df %>% group_by(Race_Ethnicity) %>%
    summarise(Minimum = min(post),
              Maximum = max(post),
              Mean = mean(post))
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  stargazer(df.summary, 
            summary = FALSE, 
            type = 'text',
            title = paste0(sheet.name, '-Post'))
  
}

### Models (Kruskal Test)
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  print(kruskal.test(post ~ Race_Ethnicity, data = df))
  
}

### Kruskal Effect size
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  print(kruskal_effsize(df, post ~ Race_Ethnicity))
  
}

### Models (Pairwise Wilcox)
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  print(pairwise.wilcox.test(df$post, 
                             df$Race_Ethnicity, 
                             p.adjust.method = "BH"))
  
}

### Plot Post Means by Race/Ethnicity 
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  # Summarize
  data_summary <- group_by(df, Race_Ethnicity) %>%
    summarise(Out_mean = mean(post),
              Out_se = std.error(post),
              Count = n()) %>%
    arrange(desc(Out_mean))
  
  # Make Race/Ethnicity a fact
  data_summary$Race_Ethnicity <- as.factor(data_summary$Race_Ethnicity)
  
  # Add Group_Count variable
  data_summary$Group_Count <- paste0(data_summary$Race_Ethnicity,
                                     ' ', 
                                     '(n = ',
                                     data_summary$Count,
                                     ')')
  # Make Group_Count a factor
  data_summary$Group_Count <- as.factor(data_summary$Group_Count)
  
  
  # Plot
  plot <- ggplot(data_summary, 
                 aes(x = Group_Count,
                     y = Out_mean,
                     group = Group_Count)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = Group_Count), 
             show.legend = TRUE) +
    geom_errorbar(aes(ymin = Out_mean - Out_se, 
                      ymax = Out_mean + Out_se),
                  width = 0.1, 
                  position=position_dodge(0.9)) +
    xlab('') +
    ylab('Mean') +
    labs(fill = 'Race/Ethnicity') + 
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) +
    ggtitle(paste('Average Post', sheet.name,'by Race/Ethnicity')) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))
  
  print(plot)
  
  ggsave(filename = paste0('MeanPostRace_',
                           sheet.name,
                           '.png'),
         width = 8, height = 4, units = 'in')
  
}
# ggsave Export 4x8in


########
# Variance by Age Group
########
# Plot Prior Means by Age Group
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  # Summarize
  data_summary <- group_by(df, MHSA_Age_Group) %>%
    summarise(Out_mean = mean(prior),
              Out_se = std.error(prior),
              Count = n()) %>%
    arrange(desc(Out_mean))
  
  # Make Age Group a factor
  data_summary$MHSA_Age_Group <- as.factor(data_summary$MHSA_Age_Group)
  
  # Add Group_Count variable
  data_summary$Group_Count <- paste0(data_summary$MHSA_Age_Group ,
                                     ' ', 
                                     '(n = ',
                                     data_summary$Count,
                                     ')')
  # Make Group_Count a factor
  data_summary$Group_Count <- as.factor(data_summary$Group_Count)
  
  
  # Plot
  plot <- ggplot(data_summary, 
                 aes(x = Group_Count,
                     y = Out_mean,
                     group = Group_Count)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = Group_Count), 
             show.legend = TRUE) +
    geom_errorbar(aes(ymin = Out_mean - Out_se, 
                      ymax = Out_mean + Out_se),
                  width = 0.1, 
                  position=position_dodge(0.9)) +
    xlab('') +
    ylab('Mean') +
    labs(fill = 'Age Group') + 
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) +
    ggtitle(paste('Average Prior', sheet.name,'by Age Group')) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))
  
  print(plot)
  
  ggsave(filename = paste0('MeanPriorAge_',
                           sheet.name,
                           '.png'),
         width = 8, height = 3, units = 'in')
}

# Plot Post Means by Age Group
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  # Summarize
  data_summary <- group_by(df, MHSA_Age_Group) %>%
    summarise(Out_mean = mean(post),
              Out_se = std.error(post),
              Count = n()) %>%
    arrange(desc(Out_mean))
  
  # Make Age Group a factor
  data_summary$MHSA_Age_Group <- as.factor(data_summary$MHSA_Age_Group)
  
  # Add Group_Count variable
  data_summary$Group_Count <- paste0(data_summary$MHSA_Age_Group ,
                                     ' ', 
                                     '(n = ',
                                     data_summary$Count,
                                     ')')
  # Make Group_Count a factor
  data_summary$Group_Count <- as.factor(data_summary$Group_Count)
  
  
  # Plot
  plot <- ggplot(data_summary, 
                 aes(x = Group_Count,
                     y = Out_mean,
                     group = Group_Count)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = Group_Count), 
             show.legend = TRUE) +
    geom_errorbar(aes(ymin = Out_mean - Out_se, 
                      ymax = Out_mean + Out_se),
                  width = 0.1, 
                  position=position_dodge(0.9)) +
    xlab('') +
    ylab('Mean') +
    labs(fill = 'Age Group') + 
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) +
    ggtitle(paste('Average Post', sheet.name,'by Age Group')) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))
  
  print(plot)
  
  ggsave(filename = paste0('MeanPostAge_',
                           sheet.name,
                           '.png'),
         width = 8, height = 3, units = 'in')
}


########
# Variance by FSP Age Group
########
# Plot Prior Means by FSP Age Group
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  # Summarize
  data_summary <- group_by(df, ProgAgeGroup) %>%
    summarise(Out_mean = mean(prior),
              Out_se = std.error(prior),
              Count = n()) %>%
    arrange(desc(Out_mean))
  
  # Make Age Group a factor
  data_summary$ProgAgeGroup <- as.factor(data_summary$ProgAgeGroup)
  
  # Add Group_Count variable
  data_summary$Group_Count <- paste0(data_summary$ProgAgeGroup ,
                                     ' ', 
                                     '(n = ',
                                     data_summary$Count,
                                     ')')
  # Make Group_Count a factor
  data_summary$Group_Count <- as.factor(data_summary$Group_Count)
  
  
  # Plot
  plot <- ggplot(data_summary, 
                 aes(x = Group_Count,
                     y = Out_mean,
                     group = Group_Count)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = Group_Count), 
             show.legend = TRUE) +
    geom_errorbar(aes(ymin = Out_mean - Out_se, 
                      ymax = Out_mean + Out_se),
                  width = 0.1, 
                  position=position_dodge(0.9)) +
    xlab('') +
    ylab('Mean') +
    labs(fill = 'FSP Age Group') + 
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) +
    ggtitle(paste('Average Prior', sheet.name,'by FSP Age Group')) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))
  
  print(plot)
  
  #ggsave(filename = paste0('MeanPriorAge_',
                           #sheet.name,
                           #'.png'),
         #width = 8, height = 3, units = 'in')
}

# Plot Post Means by Age Group
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  # Summarize
  data_summary <- group_by(df, ProgAgeGroup) %>%
    summarise(Out_mean = mean(post),
              Out_se = std.error(post),
              Count = n()) %>%
    arrange(desc(Out_mean))
  
  # Make Age Group a factor
  data_summary$ProgAgeGroup <- as.factor(data_summary$ProgAgeGroup)
  
  # Add Group_Count variable
  data_summary$Group_Count <- paste0(data_summary$ProgAgeGroup ,
                                     ' ', 
                                     '(n = ',
                                     data_summary$Count,
                                     ')')
  # Make Group_Count a factor
  data_summary$Group_Count <- as.factor(data_summary$Group_Count)
  
  
  # Plot
  plot <- ggplot(data_summary, 
                 aes(x = Group_Count,
                     y = Out_mean,
                     group = Group_Count)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = Group_Count), 
             show.legend = TRUE) +
    geom_errorbar(aes(ymin = Out_mean - Out_se, 
                      ymax = Out_mean + Out_se),
                  width = 0.1, 
                  position=position_dodge(0.9)) +
    xlab('') +
    ylab('Mean') +
    labs(fill = 'FSP Age Group') + 
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) +
    ggtitle(paste('Average Post', sheet.name,'by FSP Age Group')) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))
  
  print(plot)
  
  #ggsave(filename = paste0('MeanPostAge_',
                           #sheet.name,
                           #'.png'),
        # width = 8, height = 3, units = 'in')
}


########
# Variance by Fiscal Year
########
# Plot Prior Means by Fiscal Year
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  # Summarize
  data_summary <- group_by(df, year) %>%
    summarise(Out_mean = mean(prior),
              Out_se = std.error(prior),
              Count = n()) %>%
    arrange(desc(Out_mean))
  
  # Make Age Group a factor
  data_summary$year <- as.factor(data_summary$year)
  
  # Add Group_Count variable
  data_summary$Group_Count <- paste0(data_summary$year ,
                                     ' ', 
                                     '(n = ',
                                     data_summary$Count,
                                     ')')
  # Make Group_Count a factor
  data_summary$Group_Count <- as.factor(data_summary$Group_Count)
  
  
  # Plot
  plot <- ggplot(data_summary, 
                 aes(x = Group_Count,
                     y = Out_mean,
                     group = Group_Count)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = Group_Count), 
             show.legend = TRUE) +
    geom_errorbar(aes(ymin = Out_mean - Out_se, 
                      ymax = Out_mean + Out_se),
                  width = 0.1, 
                  position=position_dodge(0.9)) +
    xlab('') +
    ylab('Mean') +
    labs(fill = 'Fiscal Year') + 
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) +
    ggtitle(paste('Average Prior', sheet.name,'by Fiscal Year')) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))
  
  print(plot)
  
  ggsave(filename = paste0('MeanPriorYear_',
                           sheet.name,
                           '.png'),
         width = 8, height = 3, units = 'in')
}

# Plot Post Means by Fiscal Year
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  # Summarize
  data_summary <- group_by(df, year) %>%
    summarise(Out_mean = mean(post),
              Out_se = std.error(post),
              Count = n()) %>%
    arrange(desc(Out_mean))
  
  # Make Age Group a factor
  data_summary$year <- as.factor(data_summary$year)
  
  # Add Group_Count variable
  data_summary$Group_Count <- paste0(data_summary$year ,
                                     ' ', 
                                     '(n = ',
                                     data_summary$Count,
                                     ')')
  # Make Group_Count a factor
  data_summary$Group_Count <- as.factor(data_summary$Group_Count)
  
  
  # Plot
  plot <- ggplot(data_summary, 
                 aes(x = Group_Count,
                     y = Out_mean,
                     group = Group_Count)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = Group_Count), 
             show.legend = TRUE) +
    geom_errorbar(aes(ymin = Out_mean - Out_se, 
                      ymax = Out_mean + Out_se),
                  width = 0.1, 
                  position=position_dodge(0.9)) +
    xlab('') +
    ylab('Mean') +
    labs(fill = 'Fiscal Year') + 
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) +
    ggtitle(paste('Average Post', sheet.name,'by Fiscal Year')) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE))
  
  print(plot)
  
  ggsave(filename = paste0('MeanPostYear_',
                           sheet.name,
                           '.png'),
         width = 8, height = 3, units = 'in')
}


#################################################
# Step 2: Two-Way Analysis of Variance
#################################################
### Plot post means by Program and Age
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  # Summarize
  data_summary <- group_by(df, ProgramName, MHSA_Age_Group) %>%
    summarise(Out_mean = mean(post),
              Out_se = std.error(post),
              Count = n()) %>%
    arrange(desc(Out_mean))
  
  # Re-order ProgramName
  data_summary$ProgramName <- 
    factor(data_summary$ProgramName, levels = c('OASIS Enrolled',
                                                'TAO North',
                                                'TAO Central',
                                                'TAO South',
                                                'Home First FSP',
                                                'Telecare STEPs',
                                                'Telecare AOT',
                                                'Telecare STEPs Collaborative Court',
                                                'OK Enrollment / FSP',
                                                'Telecare WIT'))
  
  data_summary <- data_summary[order(data_summary$ProgramName),]
  
  # Plot
  plot <- ggplot(data_summary, 
         aes(x = MHSA_Age_Group,
             y = Out_mean,
             group = ProgramName)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = ProgramName), 
             show.legend = TRUE) +
    scale_fill_manual(values = c("red1",
                                 "orange1", 'orange2', 'orange3',
                                 'gold1',
                                 'forestgreen', 'darkgreen',
                                 'blue1', 'blue2', 'blue3')) +
    geom_errorbar(aes(ymin = Out_mean - Out_se, 
                      ymax = Out_mean + Out_se),
                  width = 0.1, 
                  position=position_dodge(0.9)) +
    xlab('Age Group') +
    ylab('Mean') +
    labs(fill = 'Program Name') + 
    theme(axis.title.y=element_blank()) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE)) +
    annotate('text', 
             x = 1.25,
             y = (max(data_summary$Out_mean) + .3*max(data_summary$Out_mean)),
             size = 3,
             color = 'red1',
             label = "") +
    annotate('text', 
             x = c(.5, 1.5, 2.5),
             y = (max(data_summary$Out_mean) + .17*max(data_summary$Out_mean)),
             size = 2,
             color = 'red1',
             label = "General") +
    annotate('text', 
             x = c(.7, 1.7, 2.7),
             y = (max(data_summary$Out_mean) + .17*max(data_summary$Out_mean)),
             size = 2,
             color = 'orange2',
             label = "Homeless") +
    annotate('text', 
             x = c(.9, 1.9, 2.9),
             y = (max(data_summary$Out_mean) + .175*max(data_summary$Out_mean)),
             size = 2,
             color = 'gold1',
             label = "Homeless Prevention") +
    annotate('text', 
             x = c(1.1, 2.1, 3.1),
             y = (max(data_summary$Out_mean) + .17*max(data_summary$Out_mean)),
             size = 2,
             color = 'darkgreen',
             label = "Intensive Treatment") +
    annotate('text', 
             x = c(1.3, 2.3, 3.3),
             y = (max(data_summary$Out_mean) + .17*max(data_summary$Out_mean)),
             size = 2,
             color = 'blue2',
             label = "Justice Involved") +
    ggtitle(paste('Average Post', 
                  sheet.name,
                  'by Program and Age Group'))
  
  print(plot)
  
}

for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  # Summarize
  data_summary <- group_by(df, ProgramName, MHSA_Age_Group) %>%
    summarise(Out_mean = mean(post),
              Out_se = std.error(post),
              Count = n()) %>%
    arrange(desc(Out_mean))
  
  # Re-order ProgramName
  data_summary$ProgramName <- 
    factor(data_summary$ProgramName, levels = c('OASIS Enrolled',
                                                'TAO North',
                                                'TAO Central',
                                                'TAO South',
                                                'Home First FSP',
                                                'Telecare STEPs',
                                                'Telecare AOT',
                                                'Telecare STEPs Collaborative Court',
                                                'OK Enrollment / FSP',
                                                'Telecare WIT'))
  
  data_summary <- data_summary[order(data_summary$ProgramName),]
  
  # Plot
  plot <- ggplot(data_summary, 
                 aes(x = MHSA_Age_Group,
                     y = Out_mean,
                     group = ProgramName)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = ProgramName), 
             show.legend = TRUE) +
    scale_fill_manual(values = c("red1",
                                 "orange1", 'orange2', 'orange3',
                                 'gold1',
                                 'forestgreen', 'darkgreen',
                                 'blue1', 'blue2', 'blue3')) +
    geom_errorbar(aes(ymin = Out_mean - Out_se, 
                      ymax = Out_mean + Out_se),
                  width = 0.1, 
                  position=position_dodge(0.9)) +
    xlab('Age Group') +
    ylab('Mean') +
    labs(fill = 'Program Name') + 
    theme(axis.title.y=element_blank()) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE)) +
    annotate('text', 
             x = 1.25,
             y = (max(data_summary$Out_mean) + .5*max(data_summary$Out_mean)),
             size = 3,
             color = 'red1',
             label = "") +
    annotate('text', 
             x = .9,
             y = (max(data_summary$Out_mean) + .3*max(data_summary$Out_mean)),
             size = 3,
             color = 'red1',
             label = "General") +
    annotate('text', 
             x = 1.6,
             y = (max(data_summary$Out_mean) + .3*max(data_summary$Out_mean)),
             size = 3,
             color = 'orange2',
             label = "Homeless") +
    annotate('text', 
             x = 1.8,
             y = (max(data_summary$Out_mean) + .3*max(data_summary$Out_mean)),
             size = 3,
             color = 'gold1',
             label = "Homeless Prevention") +
    annotate('text', 
             x = 2.2,
             y = (max(data_summary$Out_mean) + .3*max(data_summary$Out_mean)),
             size = 3,
             color = 'darkgreen',
             label = "Intensive Treatment") +
    annotate('text', 
             x = 2.9,
             y = (max(data_summary$Out_mean) + .3*max(data_summary$Out_mean)),
             size = 3,
             color = 'blue2',
             label = "Justice Involved") +
    ggtitle(paste('Average Post', 
                  sheet.name,
                  'by Program and Age Group'))
  
  print(plot)
  
}

#################################################
# Regression
#################################################
# Make mods
for (i in 1:9) {
  
  df <- list.df.out[[i]]
  
  mod <- lm(post ~ prior + 
               ProgramName*MHSA_Age_Group + 
               ProgramName + 
               MHSA_Age_Group + 
               Race_Ethnicity + 
               year, 
             data = df)
  
  assign(paste0('mod', i), mod)
}

namevec <- c('Hospitalization',
             'Jail',
             'Homelessness',
             'Emergency Shelter',
             'Emergency Intervention',
             'Independent Living Days',
             'Arrests',
             'Employment',
             'Education')

# Output table
stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9,
          title = 'Linear Regression Models',
          type = 'html',
          out = 'test.html',
          dep.var.labels = "Post",
          column.labels = c('Hospitalization',
                            'Jail',
                            'Homelessness',
                            'Emergency Shelter',
                            'Emergency Intervention',
                            'Independent Living Days',
                            'Arrests',
                            'Employment',
                            'Education'),
          covariate.labels = c('Prior',
                               'Oasis',
                               'Oppurtunity Knocks',
                               'TAO Central',
                               'TAO North',
                               'TAO South',
                               'Telecare AOT',
                               'Telecare STEPs',
                               'STEPs Collaborative Court',
                               'Telecare WIT',
                               'Older Adult',
                               'TAY',
                               'Black/African American',
                               'Hispanic/Latinx',
                               'MENAP',
                               'Native American/Am Indian',
                               'Other Specified Race',
                               'Race not Reported',
                               'White (Non-Hispanic)',
                               'Race Withheld',
                               '19/20',
                               '20/21',
                               'OASIS/Older Adult',
                               'OK/Older Adult',
                               'TAO Central/Older Adult',
                               'TAO North/Older Adult',
                               'TAO South/Older Adult',
                               'Telecare AOT/Older Adult',
                               'Telecare STEPs/Older Adult',
                               'STEPs Collab Court/Older Adult',
                               'Telecare WIT/Older Adult',
                               'Oasis/TAY',
                               'OK/TAY',
                               'TAO Central/TAY',
                               'TAO North/TAY',
                               'TAO South/TAY',
                               'Telecare AOT/TAY',
                               'Telecare STEPs/TAY',
                               'STEPs Collab Court/TAY',
                               'Telecare WIT/TAY'))
          

# Just Prior
for (i in 1:9) {
  
  df <- list.df.out[[i]]
  
  mod <- lm(post ~ prior,
            data = df)
  
  assign(paste0('mod', i), mod)
}

# Output table
stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9,
          title = 'Linear Regression Models',
          type = 'html',
          out = 'test1.html',
          dep.var.labels = "Post",
          column.labels = namevec,
          covariate.labels = 'Prior')

# Just Program (NO EDU)
for (i in 1:9) {
  
  df <- list.df.out[[i]]
  
  if (i != 9){
    factor <- as.factor(df$ProgramName)
  }
  mod <- lm(post ~ ProgramName,
            data = df)
  
  assign(paste0('mod', i), mod)
}

# Output table
stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9,
          title = 'Linear Regression Models',
          type = 'html',
          out = 'test2.html',
          dep.var.labels = "Post",
          column.labels = namevec,
          covariate.labels = levels(factor)[2:length(levels(factor))])


# Just Race/Ethnicity
for (i in 1:9) {
  
  df <- list.df.out[[i]]
  
  factor <- as.factor(df$Race_Ethnicity)
  
  mod <- lm(post ~ Race_Ethnicity,
            data = df)
  
  assign(paste0('mod', i), mod)
}

# Output table
stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9,
          title = 'Linear Regression Models',
          type = 'html',
          out = 'test3.html',
          dep.var.labels = "Post",
          column.labels = namevec,
          covariate.labels = levels(factor)[2:length(levels(factor))])

# Just Age Group
for (i in 1:9) {
  
  df <- list.df.out[[i]]
  
  factor <- as.factor(df$MHSA_Age_Group)
  
  mod <- lm(post ~ MHSA_Age_Group,
            data = df)
  
  assign(paste0('mod', i), mod)
}

# Output table
stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9,
          title = 'Linear Regression Models',
          type = 'html',
          out = 'test4.html',
          dep.var.labels = "Post",
          column.labels = namevec,
          covariate.labels = levels(factor)[2:length(levels(factor))])

# Just FSP Age Group
for (i in 1:9) {
  
  df <- list.df.out[[i]]
  
  factor <- as.factor(df$ProgAgeGroup)
  
  mod <- lm(post ~ ProgAgeGroup,
            data = df)
  
  assign(paste0('mod', i), mod)
}

# Output table
stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9,
          title = 'Linear Regression Models',
          type = 'html',
          out = 'test5.html',
          dep.var.labels = "Post",
          column.labels = namevec,
          covariate.labels = levels(factor)[2:length(levels(factor))])

# Just Fiscal Year
for (i in 1:9) {
  
  df <- list.df.out[[i]]
  
  factor <- as.factor(df$year)
  
  mod <- lm(post ~ year,
            data = df)
  
  assign(paste0('mod', i), mod)
}

# Output table
stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9,
          title = 'Linear Regression Models',
          type = 'html',
          out = 'test6.html',
          dep.var.labels = "Post",
          column.labels = namevec,
          covariate.labels = levels(factor)[2:length(levels(factor))])

#################################################  
### FSP Improvement Plots (Prior to Post, grouped by Year and Age Group)
#################################################
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  df.summary <- df %>% 
    select(-difference) %>%
    gather(key = "phase", value = "value", 5:6) %>%
    group_by(year, MHSA_Age_Group) %>%
    cohens_d(value ~ phase, paired = TRUE)
  
  df.summary$effsize <- abs(df.summary$effsize)
  
  df.summary
  
  plot <- ggplot(df.summary, 
         aes(x = year,
             y = effsize,
             group = MHSA_Age_Group)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = MHSA_Age_Group), 
             show.legend = TRUE) + 
    xlab('Year') + 
    ylab('Effect Size') +
    ylim(0, 1) +
    labs(fill = 'Age Group') +
    coord_flip() +
    ggtitle(paste('FSP Improvement on', sheet.name))
  
  print(plot)
  
  ggsave(filename = paste0('FSPImrp_',
                           sheet.name,
                           '.png'),
         width = 8, height = 3, units = 'in')

}
# ggsave Export 3x8in #

# Remove (0,0)s for all but Employment/Independent Living 
# Do not include children in Independent
for (i in 1:8){
  
  df <- list.df.out[[i]]
  
  if (i != 6 & i != 8){
    
    df <- df[!(df$prior == 0 & df$post == 0),] # Remove (0,0)s
    
  }
  
  if (i == 6){
    
    df <- subset(df, df$MHSA_Age_Group != 'Child') # Remove 'Child'
    
    colorvec <- c("#7CAE00", "#00BFC4", "#C77CFF")
    
  } else {
    
    colorvec <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
    
  }

  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  df.summary <- df %>% 
    select(-difference) %>%
    gather(key = "phase", value = "value", 6:7) %>%
    group_by(year, MHSA_Age_Group) %>%
    cohens_d(value ~ phase, paired = TRUE)
  
  df.summary$effsize <- abs(df.summary$effsize)
  
  df.summary
  
  nsum <- sum(df.summary$n1)
  
  plot <- ggplot(df.summary, 
                 aes(x = year,
                     y = effsize,
                     group = MHSA_Age_Group)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = MHSA_Age_Group), 
             show.legend = TRUE) +
    geom_text(aes(y = 0,
                  label = paste0('ES = ', 
                                round(effsize, 2), 
                                ' (n = ', 
                                n1,
                                ')'),
                  fill = MHSA_Age_Group),
              position = position_dodge(0.9),
              hjust = -.1) +
    scale_fill_manual(values = colorvec) +
    xlab('Year') + 
    ylab('Effect Size') +
    labs(fill = 'Age Group',
         caption = paste('Total Sample Size:',
                         nsum)) +
    coord_flip(ylim = c(0, 1.5)) + 
    guides(fill = guide_legend(reverse = TRUE)) +
    ggtitle(paste('FSP Improvement on', sheet.name))
  
  print(plot)
  
  ggsave(filename = paste0('FSPImrp_',
                           sheet.name,
                           '.png'),
         width = 8, height = 3, units = 'in')
  
}

# Remove (0,0)s for all but Employment/Independent Living 
# Do not include children in Independent
# FSP Age Group
for (i in 1:8){
  
  df <- list.df.out[[i]]
  
  if (i != 6 & i != 8){
    
    df <- df[!(df$prior == 0 & df$post == 0),] # Remove (0,0)s
    
  }
  
  if (i == 6){
    
    df <- subset(df, df$ProgAgeGroup != 'Child FSP') # Remove 'Child'
    
    colorvec <- c("#7CAE00", "#00BFC4", "#C77CFF")
    
  } else {
    
    colorvec <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
    
  }
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  df.summary <- df %>% 
    select(-difference) %>%
    gather(key = "phase", value = "value", 6:7) %>%
    group_by(year, ProgAgeGroup) %>%
    cohens_d(value ~ phase, paired = TRUE)
  
  df.summary$effsize <- abs(df.summary$effsize)
  
  df.summary
  
  nsum <- sum(df.summary$n1)
  
  plot <- ggplot(df.summary, 
                 aes(x = year,
                     y = effsize,
                     group = ProgAgeGroup)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = ProgAgeGroup), 
             show.legend = TRUE) +
    geom_text(aes(y = 0,
                  label = paste0('ES = ', 
                                 round(effsize, 2), 
                                 ' (n = ', 
                                 n1,
                                 ')'),
                  fill = ProgAgeGroup),
              position = position_dodge(0.9),
              hjust = -.1) +
    scale_fill_manual(values = colorvec) +
    xlab('Year') + 
    ylab('Effect Size') +
    labs(fill = 'FSP Age Group',
         caption = paste('Total Sample Size:',
                         nsum)) +
    coord_flip(ylim = c(0, 1.5)) + 
    guides(fill = guide_legend(reverse = TRUE)) +
    ggtitle(paste('FSP Improvement on', sheet.name, 'by FSP Age Group'))
  
  print(plot)
  
  ggsave(filename = paste0('FSPImrpFSP_',
                           sheet.name,
                           '.png'),
         width = 8, height = 3, units = 'in')
  
}

#################################################  
### FSP Improvement Plots (Prior to Post, grouped by Age and Year Group)
#################################################
# Remove (0,0)s for all but Employment/Independent Living 
# Do not include children in Independent
for (i in 1:8){
  
  df <- list.df.out[[i]]
  
  if (i != 6 & i != 8){
    
    df <- df[!(df$prior == 0 & df$post == 0),] # Remove (0,0)s
    
  }
  
  if (i == 6){
    
    df <- subset(df, df$MHSA_Age_Group != 'Child') # Remove 'Child'
    
    # colorvec <- c("#00BFC4", "#C77CFF")
    
  } else {
    
    colorvec <- c("#7CAE00", "#00BFC4", "#C77CFF")
    
  }
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  df.summary <- df %>% 
    select(-difference) %>%
    gather(key = "phase", value = "value", 6:7) %>%
    group_by(year, MHSA_Age_Group) %>%
    cohens_d(value ~ phase, paired = TRUE)
  
  df.summary$effsize <- abs(df.summary$effsize)
  
  df.summary
  
  nsum <- sum(df.summary$n1)
  
  plot <- ggplot(df.summary, 
                 aes(x = MHSA_Age_Group,
                     y = effsize,
                     group = year)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = year), 
             show.legend = TRUE) +
    geom_text(aes(y = 0,
                  label = paste0('ES = ', 
                                 round(effsize, 2), 
                                 ' (n = ', 
                                 n1,
                                 ')'),
                  fill = year),
              position = position_dodge(0.9),
              hjust = -.1) +
    scale_fill_manual(values = colorvec) +
    xlab('') + 
    ylab('Effect Size') +
    labs(fill = 'Fiscal Year',
         caption = paste('Total Sample Size:',
                         nsum)) +
    coord_flip(ylim = c(0, 1.5)) + 
    guides(fill = guide_legend(reverse = TRUE)) +
    ggtitle(paste('FSP Improvement on', sheet.name))
  
  print(plot)
  
  ggsave(filename = paste0('FSPImrpYAGE_',
                           sheet.name,
                           '.png'),
         width = 8, height = 4, units = 'in')
  
}

# Remove (0,0)s for all but Employment/Independent Living 
# Do not include children in Independent
# FSP Age Group
for (i in 1:8){
  
  df <- list.df.out[[i]]
  
  if (i != 6 & i != 8){
    
    df <- df[!(df$prior == 0 & df$post == 0),] # Remove (0,0)s
    
  }
  
  if (i == 6){
    
    df <- subset(df, df$ProgAgeGroup != 'Child FSP') # Remove 'Child'
    
    #colorvec <- c("#7CAE00", "#00BFC4", "#C77CFF")
    
  } else {
    
    colorvec <- c("#7CAE00", "#00BFC4", "#C77CFF")
    
  }
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  df.summary <- df %>% 
    select(-difference) %>%
    gather(key = "phase", value = "value", 6:7) %>%
    group_by(year, ProgAgeGroup) %>%
    cohens_d(value ~ phase, paired = TRUE)
  
  df.summary$effsize <- abs(df.summary$effsize)
  
  df.summary
  
  nsum <- sum(df.summary$n1)
  
  plot <- ggplot(df.summary, 
                 aes(x = ProgAgeGroup,
                     y = effsize,
                     group = year)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = year), 
             show.legend = TRUE) +
    geom_text(aes(y = 0,
                  label = paste0('ES = ', 
                                 round(effsize, 2), 
                                 ' (n = ', 
                                 n1,
                                 ')'),
                  fill = year),
              position = position_dodge(0.9),
              hjust = -.1) +
    scale_fill_manual(values = colorvec) +
    xlab('Year') + 
    ylab('Effect Size') +
    labs(fill = 'Fiscal Year',
         caption = paste('Total Sample Size:',
                         nsum)) +
    coord_flip(ylim = c(0, 1.5)) + 
    guides(fill = guide_legend(reverse = TRUE)) +
    ggtitle(paste('FSP Improvement on', 
                  sheet.name, 
                  '\nby FSP Age Group & Fiscal Year'))
  
  print(plot)
  
  ggsave(filename = paste0('FSPImrpFSPYAGE_',
                           sheet.name,
                           '.png'),
         width = 8, height = 4, units = 'in')
  
}
#################################################  
### FSP Improvement Plots (Prior to Post, grouped by Program) 
#################################################
# No Child or TAY FSP
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  # Remove Child and TAY FSP
  df <- df[(df$ProgAgeGroup != 'Child FSP' & df$ProgAgeGroup != 'TAY FSP'),]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  df.summary <- df %>% 
    select(-difference) %>%
    gather(key = "phase", value = "value", 6:7) %>%
    group_by(ProgramName) %>%
    cohens_d(value ~ phase, paired = TRUE)
  
  df.summary$effsize <- abs(df.summary$effsize)
  
  df.summary
  
  # Re-order ProgramName
  df.summary$ProgramName <- 
    factor(df.summary$ProgramName, levels = c('OASIS Enrolled',
                                                'TAO North',
                                                'TAO Central',
                                                'TAO South',
                                                'Home First FSP',
                                                'Telecare STEPs',
                                                'Telecare AOT',
                                                'Telecare STEPs Collaborative Court',
                                                'OK Enrollment / FSP',
                                                'Telecare WIT'))
  
  df.summary <- df.summary[order(df.summary$ProgramName),]
  
  # Add Program_Count variable
  df.summary$Program_Count <- paste0(df.summary$ProgramName,
                                       ' ', 
                                       '(n = ',
                                     df.summary$n1,
                                       ')')
  
  # Match Factor level of ProgramName and Program_Count
  df.summary$Program_Count <- 
    factor(df.summary$Program_Count, 
           levels = c(
             paste0('Oasis Enrolled (n = ',
                    as.character(
                      df.summary[df.summary$ProgramName == 'OASIS Enrolled', 
                                   6]),
                    ')'),
             paste0('TAO North (n = ',
                    as.character(
                      df.summary[df.summary$ProgramName == 'TAO North',
                                   6]),
                    ')'),
             paste0('TAO Central (n = ',
                    as.character(
                      df.summary[df.summary$ProgramName == 'TAO Central',
                                   6]),
                    ')'),
             paste0('TAO South (n = ',
                    as.character(
                      df.summary[df.summary$ProgramName == 'TAO South',
                                   6]),
                    ')'),
             paste0('Home First FSP (n = ',
                    as.character(
                      df.summary[df.summary$ProgramName == 'Home First FSP',
                                   6]),
                    ')'),
             paste0('Telecare STEPs (n = ',
                    as.character(
                      df.summary[df.summary$ProgramName == 'Telecare STEPs',
                                   6]),
                    ')'),
             paste0('Telecare AOT (n = ',
                    as.character(
                      df.summary[df.summary$ProgramName == 'Telecare AOT',
                                   6]),
                    ')'),
             paste0('Telecare STEPs Collaborative Court (n = ',
                    as.character(
                      df.summary[df.summary$ProgramName == 'Telecare STEPs Collaborative Court',
                                   6]),
                    ')'),
             paste0('OK Enrollment / FSP (n = ',
                    as.character(
                      df.summary[df.summary$ProgramName == 'OK Enrollment / FSP',
                                   6]),
                    ')'),
             paste0('Telecare WIT (n = ',
                    as.character(
                      df.summary[df.summary$ProgramName == 'Telecare WIT',
                                   6]),
                    ')')))
  
  df.summary$Program_Count <- replace(
    df.summary$Program_Count, c(1), levels(df.summary$Program_Count)[1])
  
  # Plot
  plot <- ggplot(df.summary, 
                 aes(x = ProgramName,
                     y = effsize,
                     label = round(effsize, digits = 2),
                     group = ProgramName)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = Program_Count), 
             show.legend = TRUE) +
    geom_text(size = 3, hjust = 0) +
    scale_fill_manual(values = c("red1",
                                 "orange1", 'orange2', 'orange3',
                                 'gold1',
                                 'forestgreen', 'darkgreen',
                                 'blue1', 'blue2', 'blue3')) +
    xlab('') + 
    ylab('Effect Size') +
    ylim(0, 1.55) +
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) +
    labs(fill = 'Program') +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE)) + 
    annotate('text', 
             x = 1.25,
             y = 1.25,
             size = 3,
             color = 'red1',
             label = "") +
    annotate('text', 
             x = 1.25,
             y = 1.25,
             size = 3,
             color = 'red1',
             label = "General") +
    annotate('text', 
             x = 3.25,
             y = 1.25,
             size = 3,
             color = 'orange2',
             label = "Homeless") +
    annotate('text', 
             x = 5.5,
             y = 1.25,
             size = 3,
             color = 'gold1',
             label = "Homeless Prevention") +
    annotate('text', 
             x = 7.25,
             y = 1.25,
             size = 3,
             color = 'darkgreen',
             label = "Intensive Treatment") +
    annotate('text', 
             x = 9.25,
             y = 1.25,
             size = 3,
             color = 'blue2',
             label = "Justice Involved") +
    ggtitle(paste('FSP Improvement on', 
                  sheet.name,
                  'by Program'))
  
  print(plot)
  
  ggsave(filename = paste0('FSPImrpProgram_',
                           sheet.name,
                           '.png'),
         width = 8, height = 4, units = 'in')
}
# ggsave export 4x8 in #

# No Child or TAY FSP (year by year)
for (i in 1:9){
  
  df.s <- list.df.out[[i]]
  
  # Remove Child and TAY FSP
  df.s <- df.s[(df.s$ProgAgeGroup != 'Child FSP' & df.s$ProgAgeGroup != 'TAY FSP'),]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  for (yr in unique(df.s$year)) {
    
    df <- subset(df.s, df.s$year == yr)
    
    df.summary <- df %>% 
      select(-difference) %>%
      gather(key = "phase", value = "value", 6:7) %>%
      group_by(ProgramName) %>%
      cohens_d(value ~ phase, paired = TRUE)
    
    df.summary$effsize <- abs(df.summary$effsize)
    
    df.summary
    
    # Re-order ProgramName
    df.summary$ProgramName <- 
      factor(df.summary$ProgramName, levels = c('OASIS Enrolled',
                                                'TAO North',
                                                'TAO Central',
                                                'TAO South',
                                                'Home First FSP',
                                                'Telecare STEPs',
                                                'Telecare AOT',
                                                'Telecare STEPs Collaborative Court',
                                                'OK Enrollment / FSP',
                                                'Telecare WIT'))
    
    df.summary <- df.summary[order(df.summary$ProgramName),]
    
    # Add Program_Count variable
    df.summary$Program_Count <- paste0(df.summary$ProgramName,
                                       ' ', 
                                       '(n = ',
                                       df.summary$n1,
                                       ')')
    
    # Match Factor level of ProgramName and Program_Count
    df.summary$Program_Count <- 
      factor(df.summary$Program_Count, 
             levels = c(
               paste0('Oasis Enrolled (n = ',
                      as.character(
                        df.summary[df.summary$ProgramName == 'OASIS Enrolled', 
                                   6]),
                      ')'),
               paste0('TAO North (n = ',
                      as.character(
                        df.summary[df.summary$ProgramName == 'TAO North',
                                   6]),
                      ')'),
               paste0('TAO Central (n = ',
                      as.character(
                        df.summary[df.summary$ProgramName == 'TAO Central',
                                   6]),
                      ')'),
               paste0('TAO South (n = ',
                      as.character(
                        df.summary[df.summary$ProgramName == 'TAO South',
                                   6]),
                      ')'),
               paste0('Home First FSP (n = ',
                      as.character(
                        df.summary[df.summary$ProgramName == 'Home First FSP',
                                   6]),
                      ')'),
               paste0('Telecare STEPs (n = ',
                      as.character(
                        df.summary[df.summary$ProgramName == 'Telecare STEPs',
                                   6]),
                      ')'),
               paste0('Telecare AOT (n = ',
                      as.character(
                        df.summary[df.summary$ProgramName == 'Telecare AOT',
                                   6]),
                      ')'),
               paste0('Telecare STEPs Collaborative Court (n = ',
                      as.character(
                        df.summary[df.summary$ProgramName == 'Telecare STEPs Collaborative Court',
                                   6]),
                      ')'),
               paste0('OK Enrollment / FSP (n = ',
                      as.character(
                        df.summary[df.summary$ProgramName == 'OK Enrollment / FSP',
                                   6]),
                      ')'),
               paste0('Telecare WIT (n = ',
                      as.character(
                        df.summary[df.summary$ProgramName == 'Telecare WIT',
                                   6]),
                      ')')))
    
    df.summary$Program_Count <- replace(
      df.summary$Program_Count, c(1), levels(df.summary$Program_Count)[1])
    
    # Plot
    if (yr == '20/21'){
      colorvec <- c("red1",
                    "orange1", 'orange2', 'orange3',
                    'gold1',
                    'forestgreen', 'darkgreen',
                    'blue1', 'blue2', 'blue3')
    } else {
      colorvec <- c("red1",
                    "orange1", 'orange2', 'orange3',
                    'forestgreen', 'darkgreen',
                    'blue1', 'blue2', 'blue3') 
    }
    
    
    plot <- ggplot(df.summary, 
                   aes(x = ProgramName,
                       y = effsize,
                       label = round(effsize, digits = 2),
                       group = ProgramName)) +
      geom_bar(position=position_dodge(0.9),
               stat = "identity", 
               aes(fill = Program_Count), 
               show.legend = TRUE) +
      geom_text(size = 3, hjust = 0) +
      scale_fill_manual(values = colorvec) +
      xlab('') + 
      ylab('Effect Size') +
      ylim(0, 1.55) +
      theme(axis.title.y=element_blank(), 
            axis.text.y=element_blank(), 
            axis.ticks.y=element_blank()) +
      labs(fill = 'Program') +
      coord_flip() +
      guides(fill = guide_legend(reverse = TRUE)) + 
      annotate('text', 
               x = 1.25,
               y = 1.25,
               size = 3,
               color = 'red1',
               label = "") +
      annotate('text', 
               x = 1.25,
               y = 1.25,
               size = 3,
               color = 'red1',
               label = "General") +
      annotate('text', 
               x = 3.25,
               y = 1.25,
               size = 3,
               color = 'orange2',
               label = "Homeless") +
      {if(yr == '20/21')annotate('text', 
               x = 5.5,
               y = 1.25,
               size = 3,
               color = 'gold1',
               label = "Homeless Prevention")} +
      annotate('text', 
               x = 7.25,
               y = 1.25,
               size = 3,
               color = 'darkgreen',
               label = "Intensive Treatment") +
      annotate('text', 
               x = 9.25,
               y = 1.25,
               size = 3,
               color = 'blue2',
               label = "Justice Involved") +
      ggtitle(paste('FSP Improvement on', 
                    sheet.name,
                    'by Program',
                    yr))
    
    print(plot)
  }
  
}

# Just Child or TAY FSP
for (i in 1:8){
  
  df <- list.df.out[[i]]
  df <- df.Arrests
  # Remove Child and TAY FSP
  df <- df[(df$ProgAgeGroup != 'Adult FSP' & df$ProgAgeGroup != 'Older Adult FSP'),]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  df.summary <- df %>% 
    select(-difference) %>%
    gather(key = "phase", value = "value", 6:7) %>%
    group_by(ProgramName) %>%
    cohens_d(value ~ phase, paired = TRUE)
  
  df.summary$effsize <- abs(df.summary$effsize)
  
  df.summary
  
   # Re-order ProgramName
  df.summary$ProgramName <- as.factor(df.summary$ProgramName)
  
  # Add Program_Count variable
  df.summary$Program_Count <- paste0(df.summary$ProgramName,
                                     ' ', 
                                     '(n = ',
                                     df.summary$n1,
                                     ')')
  
  # Plot
  plot <- ggplot(df.summary, 
                 aes(x = ProgramName,
                     y = effsize,
                     label = round(effsize, digits = 2),
                     group = ProgramName)) +
    geom_bar(position=position_dodge(0.9),
             stat = "identity", 
             aes(fill = Program_Count), 
             show.legend = TRUE) +
    geom_text(size = 3, hjust = 0) +
    xlab('') + 
    ylab('Effect Size') +
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) +
    labs(fill = 'Program') +
    coord_flip(ylim = c(0, 1.5)) +
    guides(fill = guide_legend(reverse = TRUE)) + 
    ggtitle(paste('FSP Improvement on', 
                  sheet.name,
                  'by Program'))
  
  print(plot)
  
  #ggsave(filename = paste0('FSPImrpProgram_',
                           #sheet.name,
                           #'.png'),
         #width = 8, height = 4, units = 'in')
}


#################################################  
### FSP Improvement Plots (Prior to Post, by Year. Grouped by Quantile/Age)
#################################################
# A function factory for getting integer axis values.
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

for (i in 1:8){
  
  df.s <- list.df.out[[i]]
  
  if (i != 6 & i != 8){
    
    df.s <- df.s[!(df.s$prior == 0 & df.s$post == 0),] # Remove (0,0)s
    
  }
  
  if (i == 6){
    
    df.s <- subset(df.s, df.s$MHSA_Age_Group != 'Child') # Remove 'Child'
  
  }
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  for (yr in unique(df.s$year)){
    
    # Subset by Year
    df <- subset(df.s, year == yr)
    
    # Set colorvec
    if (yr == '18/19' | i == 6){
      
      colorvec <- c("#7CAE00", "#00BFC4", "#C77CFF")
      
    } else {
      
      colorvec <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
      
    }
    
    # Create Quantile variable
    df <- df %>% 
      group_by(MHSA_Age_Group) %>%
      mutate(start_quantile = ntile(prior, 4))
    
    # Collapse quantiles with same prior value
    for (ag in unique(df$MHSA_Age_Group)){
      
      if(mean(df$prior[df$start_quantile == 1 & 
                       df$MHSA_Age_Group == ag]) == 0 &
         mean(df$prior[df$start_quantile == 2 & 
                       df$MHSA_Age_Group == ag]) == 0){
        
        df$start_quantile <- replace(df$start_quantile, 
                                     df$start_quantile == 1 &
                                       df$MHSA_Age_Group == ag, 
                                     2)
      }
    }
    
    for (ag in unique(df$MHSA_Age_Group)){
      
      if(mean(df$prior[df$start_quantile == 2 & 
                       df$MHSA_Age_Group == ag]) == 0 & 
         mean(df$prior[df$start_quantile == 3 & 
                       df$MHSA_Age_Group == ag]) == 0){
        
        df$start_quantile <- replace(df$start_quantile, 
                                     df$start_quantile == 2 &
                                       df$MHSA_Age_Group == ag, 
                                     3)
      }
    }
    
    # Summary
    df.summary <- df %>% 
      select(-difference) %>%
      gather(key = "phase", value = "value", 6:7) %>%
      group_by(start_quantile, MHSA_Age_Group) %>%
      cohens_d(value ~ phase, paired = TRUE)
    
    df.summary$effsize <- abs(df.summary$effsize)
    
    df.summary$effsize <- replace(df.summary$effsize,
                                  is.nan(df.summary$effsize),
                                  0.01)
    
    plot <- ggplot(df.summary, 
                   aes(x = start_quantile,
                       y = effsize,
                       group = MHSA_Age_Group)) +
      geom_bar(position=position_dodge(0.9),
               stat = "identity", 
               aes(fill = MHSA_Age_Group), 
               show.legend = TRUE) +
      geom_text(aes(y = 0,
                    label = paste0('ES = ', 
                                   round(effsize, 2), 
                                   ' (n = ', 
                                   n1,
                                   ')'),
                    fill = MHSA_Age_Group),
                position = position_dodge(0.9),
                size = 3,
                hjust = -.1) +
      scale_fill_manual(values = colorvec) +
      xlab('Prior Quantile') +
      scale_x_continuous(breaks = integer_breaks()) +
      ylab('Effect Size') +
      labs(fill = 'Age Group') +
      coord_flip(ylim = c(0, 1.5)) + 
      guides(fill = guide_legend(reverse = TRUE)) +
      ggtitle(paste('FSP Improvement on', 
                    sheet.name, 
                    '\nby Quantile', 
                    yr))
    
    print(plot)
    
    yr <- gsub('/', '-', yr)
    
    ggsave(filename = paste0('FSPImrpYear_QuanAge_',
                             sheet.name,
                             yr,
                             '.png'),
           width = 6, height = 4, units = 'in')
  }

}  
# Export 4x8in
for (i in 1:8){
  
  df.s <- list.df.out[[i]]
  
  if (i != 6 & i != 8){
    
    df.s <- df.s[!(df.s$prior == 0 & df.s$post == 0),] # Remove (0,0)s
    
  }
  
  if (i == 6){
    
    df.s <- subset(df.s, df.s$ProgAgeGroup != 'Child FSP') # Remove 'Child FSP'
    
  }
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  for (yr in unique(df.s$year)){
    
    # Subset by Year
    df <- subset(df.s, year == yr)
    
    # Set colorvec
    if (yr == '18/19' | i == 6){
      
      colorvec <- c("#7CAE00", "#00BFC4", "#C77CFF")
      
    } else {
      
      colorvec <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
      
    }
    
    # Create Quantile variable
    df <- df %>% 
      group_by(ProgAgeGroup) %>%
      mutate(start_quantile = ntile(prior, 4))
    
    # Collapse quantiles with same prior value
    for (ag in unique(df$ProgAgeGroup)){
      
      if(mean(df$prior[df$start_quantile == 1 & 
                       df$ProgAgeGroup == ag]) == 0 &
         mean(df$prior[df$start_quantile == 2 & 
                       df$ProgAgeGroup == ag]) == 0){
        
        df$start_quantile <- replace(df$start_quantile, 
                                     df$start_quantile == 1 &
                                       df$ProgAgeGroup == ag, 
                                     2)
      }
    }
    
    for (ag in unique(df$ProgAgeGroup)){
      
      if(mean(df$prior[df$start_quantile == 2 & 
                       df$ProgAgeGroup == ag]) == 0 & 
         mean(df$prior[df$start_quantile == 3 & 
                       df$ProgAgeGroup == ag]) == 0){
        
        df$start_quantile <- replace(df$start_quantile, 
                                     df$start_quantile == 2 &
                                       df$ProgAgeGroup == ag, 
                                     3)
      }
    }
    
    # Summary
    df.summary <- df %>% 
      select(-difference) %>%
      gather(key = "phase", value = "value", 6:7) %>%
      group_by(start_quantile, ProgAgeGroup) %>%
      cohens_d(value ~ phase, paired = TRUE)
    
    df.summary$effsize <- abs(df.summary$effsize)
    
    df.summary$effsize <- replace(df.summary$effsize,
                                  is.nan(df.summary$effsize),
                                  0.01)
    
    plot <- ggplot(df.summary, 
                   aes(x = start_quantile,
                       y = effsize,
                       group = ProgAgeGroup)) +
      geom_bar(position=position_dodge(0.9),
               stat = "identity", 
               aes(fill = ProgAgeGroup), 
               show.legend = TRUE) +
      geom_text(aes(y = 0,
                    label = paste0('ES = ', 
                                   round(effsize, 2), 
                                   ' (n = ', 
                                   n1,
                                   ')'),
                    fill = ProgAgeGroup),
                position = position_dodge(0.9),
                size = 3,
                hjust = -.1) +
      scale_fill_manual(values = colorvec) +
      xlab('Prior Quantile') +
      scale_x_continuous(breaks = integer_breaks()) +
      ylab('Effect Size') +
      labs(fill = 'Age Group') +
      coord_flip(ylim = c(0, 1.5)) + 
      guides(fill = guide_legend(reverse = TRUE)) +
      ggtitle(paste('FSP Improvement on', 
                    sheet.name, 
                    '\nby Quantile', 
                    yr))
    
    print(plot)
    
    yr <- gsub('/', '-', yr)
    
    #ggsave(filename = paste0('FSPImrpYear_QuanAge_',
                             #sheet.name,
                             #yr,
                             #'.png'),
           #width = 6, height = 4, units = 'in')
  }
  
} #FSP Age Group

#################################################  
### FSP Improvement Plots (Prior to Post, by Age. Grouped by Quantile/Year)
#################################################
for (i in 1:8){
  
  df.s <- list.df.out[[i]]
  
  if (i != 6 & i != 8){
    
    df.s <- df.s[!(df.s$prior == 0 & df.s$post == 0),] # Remove (0,0)s
    
  }
  
  if (i == 6){
    
    df.s <- subset(df.s, df.s$MHSA_Age_Group != 'Child') # Remove 'Child'
    
  }
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  for (ag in unique(df.s$MHSA_Age_Group)){
    
    # Subset by Age
    df <- subset(df.s, MHSA_Age_Group == ag)
    
    # Set colorvec
    if (ag == 'Child'){
      
      colorvec <- c("#00BFC4", "#C77CFF")
      
    } else {
      
      colorvec <- c("#7CAE00", "#00BFC4", "#C77CFF")
      
    }
    
    # Create Quantile variable
    df <- df %>% group_by(year) %>%
      mutate(start_quantile = ntile(prior, 4))
    
    # Collapse quantiles with same prior value
    for (yr in unique(df$year)){
      
      if(mean(df$prior[df$start_quantile == 1 & 
                       df$year == yr]) == 0 &
         mean(df$prior[df$start_quantile == 2 & 
                       df$year == yr]) == 0){
        
        df$start_quantile <- replace(df$start_quantile, 
                                     df$start_quantile == 1 &
                                       df$year == yr, 
                                     2)
      }
    }
    
    for (yr in unique(df$year)){
      
      if(mean(df$prior[df$start_quantile == 2 & 
                       df$year == yr]) == 0 &
         mean(df$prior[df$start_quantile == 3 & 
                       df$year == yr]) == 0){
        
        df$start_quantile <- replace(df$start_quantile, 
                                     df$start_quantile == 2 &
                                       df$year == yr, 
                                     3)
      }
    }
    
    # Summary
    df.summary <- df %>% 
      select(-difference) %>%
      gather(key = "phase", value = "value", 6:7) %>%
      group_by(start_quantile, year) %>%
      cohens_d(value ~ phase, paired = TRUE)
    
    df.summary$effsize <- abs(df.summary$effsize)
    
    df.summary$effsize <- replace(df.summary$effsize,
                                  is.nan(df.summary$effsize),
                                  0.01)
    
    plot <- ggplot(df.summary, 
                   aes(x = start_quantile,
                       y = effsize,
                       group = year)) +
      geom_bar(position=position_dodge(0.9),
               stat = "identity", 
               aes(fill = year), 
               show.legend = TRUE) + 
      geom_text(aes(y = 0,
                    label = paste0('ES = ', 
                                   round(effsize, 2), 
                                   ' (n = ', 
                                   n1,
                                   ')'),
                    fill = year),
                position = position_dodge(0.9),
                size = 3,
                hjust = -.1) +
      scale_fill_manual(values = colorvec) +
      xlab('Prior Quantile') +
      scale_x_continuous(breaks = integer_breaks()) +
      ylab('Effect Size') +
      labs(fill = 'Year') +
      coord_flip(ylim = c(0, 1.5)) + 
      guides(fill = guide_legend(reverse = TRUE)) +
      ggtitle(paste('FSP Improvement on', 
                    sheet.name, 
                    '\nby Quantile for', 
                    ag))
    
    print(plot)
    
    yr <- gsub('/', '-', yr)
    
    ggsave(filename = paste0('FSPImrpAge_QuanYear_',
                             sheet.name,
                             ag,
                             '.png'),
           width = 6, height = 4, units = 'in')
  }
  
}  
# Export 4x8in

#################################################  
### FSP Improvement Plots (Prior to Post, by Year. Grouped by Age/Program)
#################################################
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  for (yr in unique(df$year)){
    
    df.summary <- df %>% 
      subset(year == yr) %>%
      select(-difference) %>%
      gather(key = "phase", value = "value", 5:6) %>%
      group_by(MHSA_Age_Group, ProgramName) %>%
      cohens_d(value ~ phase, paired = TRUE)
    
    df.summary$effsize <- abs(df.summary$effsize)
    
    df.summary
    
    
    df.summary$ProgramName <- 
      factor(df.summary$ProgramName, levels = c('OASIS Enrolled',
                                                'TAO North',
                                                'TAO Central',
                                                'TAO South',
                                                'Home First FSP',
                                                'Telecare STEPs',
                                                'Telecare AOT',
                                                'Telecare STEPs Collaborative Court',
                                                'OK Enrollment / FSP',
                                                'Telecare WIT'))
    
    plot <- ggplot(df.summary, 
           aes(x = MHSA_Age_Group,
               y = effsize,
               group = ProgramName)) +
      geom_bar(position=position_dodge(0.9),
               stat = "identity", 
               aes(fill = ProgramName), 
               show.legend = TRUE) + 
      scale_fill_manual(values = c("red1",
                                   "orange1", 'orange2', 'orange3',
                                   'gold1',
                                   'forestgreen', 'darkgreen',
                                   'blue1', 'blue2', 'blue3')) +
      xlab('') + 
      ylab('Effect Size') +
      ylim(0, 1.19) +
      labs(fill = 'Program Name') +
      coord_flip() + 
      annotate('label', 
               x = c(.6, 1.6, 2.6),
               y = 1,
               size = 2,
               color = 'red1',
               label = "General") + 
      annotate('label', 
               x = c(.8, 1.8, 2.8),
               y = 1,
               size = 2,
               color = 'orange2',
               label = "Homeless") +
      annotate('label', 
               x = c(1, 2, 3),
               y = 1,
               size = 2,
               color = 'gold1',
               label = "Homeless Prevention") +
      annotate('label', 
               x = c(1.2, 2.2, 3.2),
               y = 1,
               size = 2,
               color = 'darkgreen',
               label = "Intensive Treatment") +
      annotate('label', 
               x = c(1.4, 2.4, 3.4),
               y = 1,
               size = 2,
               color = 'blue2',
               label = "Justice Involved") +
      guides(fill = guide_legend(reverse = TRUE)) +
      ggtitle(paste('FSP Improvement on', sheet.name, 'in', yr))
    
    print(plot)
    
  }

}

#################################################  
### FSP Improvement Plots (Prior to Post, by Age. Grouped by Year/Program)
#################################################
for (i in 1:9){
  
  df <- list.df.out[[i]]
  
  sheet.name <- 
    excel_sheets('AdultFSPDataFY1819 - Copy.LC.xlsx')[i+3]
  sheet.name <- gsub("\\(.*", "", sheet.name)
  sheet.name <- gsub("\\ .*", "", sheet.name)
  
  print(sheet.name)
  
  for (ag in unique(df$MHSA_Age_Group)){
    
    df.summary <- df %>% 
      subset(MHSA_Age_Group == ag) %>%
      select(-difference) %>%
      gather(key = "phase", value = "value", 5:6) %>%
      group_by(year, ProgramName) %>%
      cohens_d(value ~ phase, paired = TRUE)
    
    df.summary$effsize <- abs(df.summary$effsize)
    
    df.summary
    
    
    df.summary$ProgramName <- 
      factor(df.summary$ProgramName, levels = c('OASIS Enrolled',
                                                'TAO North',
                                                'TAO Central',
                                                'TAO South',
                                                'Home First FSP',
                                                'Telecare STEPs',
                                                'Telecare AOT',
                                                'Telecare STEPs Collaborative Court',
                                                'OK Enrollment / FSP',
                                                'Telecare WIT'))
    
    plot <- ggplot(df.summary, 
                   aes(x = year,
                       y = effsize,
                       group = ProgramName)) +
      geom_bar(position=position_dodge(0.9),
               stat = "identity", 
               aes(fill = ProgramName), 
               show.legend = TRUE) + 
      scale_fill_manual(values = c("red1",
                                   "orange1", 'orange2', 'orange3',
                                   'gold1',
                                   'forestgreen', 'darkgreen',
                                   'blue1', 'blue2', 'blue3')) +
      xlab('') + 
      ylab('Effect Size') +
      ylim(0, 1.19) +
      labs(fill = 'Program Name') +
      coord_flip() + 
      annotate('text', 
               x = c(.6, 1.6, 2.6),
               y = 1,
               size = 2,
               color = 'red1',
               label = "General") + 
      annotate('text', 
               x = c(.8, 1.8, 2.8),
               y = 1,
               size = 2,
               color = 'orange2',
               label = "Homeless") +
      annotate('text', 
               x = c(1, 2, 3),
               y = 1,
               size = 2,
               color = 'gold1',
               label = "Homeless Prevention") +
      annotate('text', 
               x = c(1.2, 2.2, 3.2),
               y = 1,
               size = 2,
               color = 'darkgreen',
               label = "Intensive Treatment") +
      annotate('text', 
               x = c(1.4, 2.4, 3.4),
               y = 1,
               size = 2,
               color = 'blue2',
               label = "Justice Involved") +
      guides(fill = guide_legend(reverse = TRUE)) +
      ggtitle(paste('FSP Improvement on', sheet.name, 'for', ag))
    
    print(plot)
    
  }
  
}

  
