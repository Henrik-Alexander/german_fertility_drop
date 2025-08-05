###
# Project: Explaining fertility drop in Germany
# Purpose: Data manipulation
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 2025/07/30
###

## Structure:
# 1. bio child
# 2. bioact

rm(list = ls()); gc(T)

library(haven)
library(tidyverse)
library(lubridate)

# Set the path to Pairfam
path_pairfam <- "U:/data/deu/pairfam/Release14-0"

# Load the functions
source("code/functions.R")


# Define the waves
waves <- c(3:4, 13:14)

# 1 Load the Anchor-data ======================================

# Create a vector with the variables
basic_vars <- c("id", "demodiff", "wave", "inty", "intm", "age","cohort", "sex_gen")
analytical_vars <- c("nkidsbio", "homosex",
                     
                     # Resource variables
                     "isced", "hhincnet", "isei", "hlt1",
                     # Survey weights
                     "dweight", "d1weight", "d2weight", "d3weight", "cdweight", "cd1weight", "cd2weight", "cd3weight",
                     
                     # Settlement structure
                     "bik", "gkpol", "bula",
                     # Fertility intentions
                     
                     # Financial hardship
                     paste0("inc27i", 2:3),
                     # Division of labour
                     paste0("pa17i", 1:5),
                     
                     # Fertility questionnaires
                     "frt1", "frt2", "frt3", "frt5", "frt7","frt27", "frt28",
                     
                     # Migration status
                     "relstat", "ethni", "migstatus", "cob",
                     
                     # Satisfaction
                     paste0("sat1i", c(1:4)),
                     
                     # Helper variables: infertility and couple childbirth
                     "f1", "f2", 
                     # "sex8", "per2i3", # This does not exist in the first 2 waves of Pairfam
                     paste0("sex", c(3:5, 8)), paste0("job", c(3:4, 7)), #"hlt18",
                     # Contraceptive methods
                     paste0("sex6i", 1:11), paste0("sex6i", 7:8)
)

# Load the variables
variables <- readxl::read_xlsx("raw/variable_selection.xlsx")
variables <- variables[ifelse(variables$Column1=="TRUE", TRUE, FALSE), ]
variables$variables[!(variables %in% c(basic_vars, analytical_vars))]


# Load the anchor data for waves 12 to 14
anchor_old <- lapply(waves[1:2], FUN=function(nr) {
  
  anchor_files <- list.files(file.path(path_pairfam, "DATA", "Stata"), pattern=paste0("anchor", nr, "(_DD)?.dta$"), full.names=T)
  anchor_files <- lapply(anchor_files, read_dta)
  anchor_files <- bind_rows(anchor_files)
  
  # Create the helper variable: f1 (no fertility intentions) <- sex3==1|sex4==1
  anchor_files$f1 <- ifelse(anchor_files$sex3==1|anchor_files$sex4==1, 1, 0)
  
  # Create the helper column f2 (infertility or partner infertility)
  fertile_couple <- (anchor_files$frt1 %in% c(3, 4, -2) | anchor_files$frt2 %in% c(3, 4, -2) | anchor_files$sex6i1==1 | anchor_files$sex6i2==1 | anchor_files$sex6i7==1 | anchor_files$sex6i8==1)
  anchor_files$f2 <- ifelse(fertile_couple, 0, 1)
  
  anchor_files <- anchor_files[, c(basic_vars, analytical_vars)]
  
  return(anchor_files)
  
})

# Load the anchor data for waves 12 to 14
anchor_recent <- lapply(waves[3:4], FUN=function(nr) {
  
  anchor_files <- list.files(file.path(path_pairfam, "DATA", "Stata"), pattern=paste0("anchor", nr), full.names=T)
  anchor_files <- lapply(anchor_files, read_dta)
  anchor_files <- bind_rows(anchor_files)
  
  # Select the important variables
  anchor_files <- anchor_files[, c(basic_vars, analytical_vars)]
  
  return(anchor_files)
  
})


# Remove the filtering variables
rm(analytical_vars, basic_vars, remove_vars)

# Data manipulation =====================================

# Create interview data
df <- bind_rows(anchor_old[[1]], anchor_old[[2]], anchor_recent[[1]], anchor_recent[[2]])

# Create the interview date
df$int_date <- as.Date(paste(df$inty, df$intm, "01", sep="-"))

# Clean the intended fertility
df$unclear_fertility_intention <- ifelse(df$frt5==-1, 1, 0)

# Create the missing variables
df[df < 0] <- NA

## Select the survey weights ===========================

## Create the predictor variable ========================

### Demographic variables ---------------

# Create the age group
df$age_group <- cut(df$age, breaks=c(15, 25, 35, 45, 55), labels=c("15-25", "25-35", "35-45", "45-55"))

# Cohort
df$cohort <- factor(df$cohort, labels=c("1971-73", "1981-83", "1991-93", "2001-03"))

# Relationship status
df$relationship <- NA
df$relationship[df$relstat %in% c(9, 6, 1)] <- "Single"
df$relationship[df$relstat %in% c(2, 7, 10)] <- "Dating/in a sexual relationship"
df$relationship[df$relstat %in% c(3, 8, 11)] <-"Cohabiting"
df$relationship[df$relstat %in% c(4, 5)] <-"Married"

### Migration status -------------------------

# Ethnicity
tab(df$ethni)
df$ethnicity <- factor(df$ethni, labels=c("German native, no migration background", 
                                          "Ethnic-German Immigrant (Aussiedler)",
                                          "Half-German",
                                          "Turkish background", 
                                          "Other non-German background"),
                       ordered=T)
# Foreign born
tab(df$cob)
df$foreign_born <- ifelse(df$cob%in%c(1, 2), "native", "foreigner")

# Migration status
df$migstatus <- factor(df$migstatus, labels=c("No migration background", "1st generation", "2nd generation"))

# Federal state in Germany
df$bula <- factor(df$bula, labels=names(labelled::val_labels(df$bula)[-1]))

# Frequency of sexual intercourse
#df$freq_coitus <- df$sex8

# Contraception in the last 3 months
df$contraception <- create_dummy(df$sex5)

### Socio-economic variables --------------------

# Education
df$education <- factor(df$isced, labels = c("0 currently enrolled", "1 no degree (1b)", 
                                            "2 lower secondary education (2b)", "3 lower secondary education (2a)", 
                                            "4 upper secondary education vocational (3b)", "5 upper secondary education general (3a)",
                                            "6 post-secondary non tertiary education general (4a)", "7 first stage of tertiary education (5)",
                                            "8 second stage of tertiary education (6)"), ordered=TRUE)


# Household Income
df$hhinc_quartile <- cut(df$hhincnet, quantile(df$hhincnet, probs=seq(0, 1, by=0.25), na.rm=T), labels= paste(1:4, "quartile"), include.lowest = T, ordered_result = TRUE)

# Position on social ladder
df$social_ladder <- cut(df$isei, breaks=seq(0, 100, by=10), labels=1:10, include.lowest = T, ordered_result = TRUE)

# Do you have a fixed-term work contract? (missings=7k)
df$tenure_job <- create_dummy(df$job3)

# Public sector job
df$public_sector <- create_dummy(df$job4)

# Number of hours per week
df$working_hours <- as.numeric(df$job7)

# Economic hardship
df$economic_hardship <- factor(ifelse(df$inc27i2 %in%c(4, 5) |df$inc27i3 %in% c(4, 5), 1, 0), labels=c("no", "yes"))


###  Life goals and domains ==============

### Health variables ---------------------


# Self-rated health
df$health <- factor(df$hlt1, labels=c("Poor", "Not well", "Alright", "Good", "Very good"), ordered=TRUE)

# Self-rated fecundity
df$fecundity <- factor(df$frt1, labels=c("Defintely", "Likely yes", "Likely no", "Impossible"), ordered=TRUE)

# Depression
#df$depression <- factor(df$per2i3, labels = c("Almost never", "Sometimes", "Often", "Almost always"), ordered=TRUE)

# Desired level of education
df$desired_education <- cut(df$sat1i1, breaks=seq(0, 10, by=2), labels=1:5, include.lowest=T, ordered_result = T)

## Create the outcome variables ------------------------

# Expecting a child
df$expecting_child <- factor(df$f1==1, label=c("no", "yes"))

# Infertile couple
df$fertile_couple <- factor(df$f2, label= c("infertile", "fertile"))

# Short term childbearing intentions
df$intend_chilbirth <- NA
df$frt7[df$frt27==2] <- 4
df$intend_childbirth <- factor(df$frt7, labels= c("1 Defintely yes", "2 Probably yes", "3 Probably not", "4 Definetely not", "No thoughs"))

# Create a parity variable which is the number of biological children
df$parity <- df$nkidsbio

# Create the intended fertility size
df$intended_parity <- df$frt5

# Create number of additional intended children
df$intend_more_children <- df$intended_parity - df$parity 

# Achieved intended parity
df$reached_intended_parity <- ifelse(df$intend_more_children>1, 1, 0)

# Trying to conceive since the last interview
df$trying <- create_dummy(df$frt3)

# Create the outcome of conception or birth
df <- df %>% 
  arrange(id, wave) %>% 
  group_by(id) %>% 
  mutate(birth = ifelse(expecting_child==1|lag(parity) < parity|lead(parity)>parity, 1, 0))

# Create an indicator for childless or mother
df$childless <- ifelse(df$parity==0, 1, 0)

# Estimate the interview difference
df <- df %>% 
  arrange(id, wave) %>% 
  group_by(id) %>% 
  mutate(interview_gap = time_length(max(int_date)-min(int_date), unit="months")) %>% 
  select(-inty, -intm)

# Select the important variables ======================

# Create the proportions for wave 14 for the childless
categorical_vars <- c("cohort", "ethnicity", "education",  "desired_education", "foreign_born", "social_ladder", "relationship", #"depression", 
                      "health", "age_group" , "fecundity", "hhinc_quartile", "bula", "bik", "gkpol", "economic_hardship", "tenure_job", "public_sector", "working_hours", "contraception")
continuous_vars <- c("intended_parity", "parity", "age")
outcome_vars <- c("expecting_child", "fertile_couple", "intend_more_children", "intend_childbirth", "reached_intended_parity", "trying", "birth", "childless", "unclear_fertility_intention")
vars <- mget(ls(pattern="vars$"))

# Select the variables
df <- df[, c("id", "demodiff", "sex_gen", "wave", "int_date", "interview_gap", vars$categorical_vars, vars$outcome_vars, vars$continuous_vars)]

## Create the birth outcome variable ===============

# Conception: 1 if subsequent wave has a childbirth
df <- df %>% 
  arrange(id, wave) %>% 
  group_by(id) %>% 
  mutate(conception = ifelse(lead(birth)==1 & wave %in% waves[c(1, 3)], 1, 0),
         intention_realization= ifelse((conception==1&intend_childbirth%in%c(1,2)) | (conception==1&intend_childbirth%in%c(1,2)), 1, 0))

# Missing cases ====================================

# Find missing cases
missing_distribution <- apply(df, 2, function(x) sum(is.na(x)))

# Remove the missings
df <- df[!apply(df[, c("unclear_fertility_intention", "parity")], 1, function(x) any(is.na(x))), ]

# Save the data
save(df, file="data/analysis_data.Rda")
save(vars, file="data/variables.Rda")

### END ###################################################