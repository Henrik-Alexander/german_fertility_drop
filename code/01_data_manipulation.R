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

#=====================================================
# 1. Prepare bio child ===============================
#=====================================================

# Load the biochild data
biochild <- read_dta(file.path(path_pairfam, "DATA", "Stata", "biochild.dta"))


## 1.1. Prepare biochild_long ------------------------

# Select the important variables
remove_vars <- c("surveykid", "currliv_detail", "pid", "parentid", "smid", "sfid", "mid", "fid", "b1livkbeg", "b1livkend", "flag_parentid", "imp_livkbeg", "imp_livkend", "livkbeg", "livkend")
biochild <- biochild[ , !(names(biochild) %in% remove_vars)]

# Filter observations that have a child ID
biochild <- biochild[!is.na(biochild$cid), ]

# Delete the stepchildren
biochild <- biochild[biochild$statusk!=3, ]


# Calculate the age at childdeath
biochild$age_of_childdeath <- ifelse(biochild$dodk!=-3&biochild$dobk!=-7, biochild$dodk-biochild$dobk, NA)

# Estimate censored
biochild$censorcd <- ifelse(biochild$age_of_childdeath <= 36 & biochild$number == 1, 1, NA)

# Generate an ID-variable
biochild$id <- biochild$cid - 200 - biochild$number

# Reshapde the dataset: long -> wide
biochild_long <- pivot_wider(biochild, 
                             values_from=c("currliv", "intdat", "statusk", "index", "pno", "age_of_childdeath"), 
                             names_from="wave")

# Select the important variables
biochild_long <- biochild_long[ , c("cid", "sexk", "censorcd", "number", "cohort", "dobk", "dob", "id")]

# Save the data
save(biochild_long, file="data/biochild_long.Rda")


#=====================================================
# 4. Prepare: biopart =============================
#=====================================================

# Load the biopart data
biopart <- read_dta(file.path(path_pairfam, "DATA", "Stata", "biopart.dta"))

# Keep the important variables
biopart <- biopart |> 
  select(id, demodiff, partindex, starts_with("intdat"), dob, sexp, relbeg, relend, 
         matches("b.beg"), matches("b.end"), starts_with("partcurrw"), contains(".+flag.+"), cohbeg, cohend)


# Remove, if there are no relationship episodes
biopart <- biopart[biopart$relbeg!=-3, ]

# Sort the data
biopart <- biopart[order(biopart$id, biopart$partindex), ]

# Failure indicator (i.e. event): 1=Separation/Divorce
biopart$separ <- ifelse(biopart$relend!=-99, 1, 0)

# Generate variable for the last interview
biopart$indat_max <- apply(biopart[, str_detect(colnames(biopart), "intdat")], MARGIN=c(1), max)

# Censored unions get the interveiw data as relationship end
biopart$relend <- ifelse(biopart$relend==-99, biopart$indat_max, biopart$relend)

# Drop relationships without data information (n=1274, 3.39%)
biopart <- biopart[biopart$relbeg!=-7 | biopart$relend!=-7, ]

# Create the age at union formation and separation
biopart$ageunion <- (biopart$relbeg-biopart$dob)/12
biopart$ageendunion <- (biopart$relend-biopart$dob)/12

# Drop unions with age at start of union below 10, e.g. not trustworthy (N=444, 1.12%)
biopart <- biopart[biopart$ageunion>=10, ]

# Drop unions with age at start of union below 14, e.g. not trustworthy (N=452, 1.25%)
biopart <- biopart[biopart$ageunion>=14, ]

# Remove redundant variables
biopart <- biopart[, c("id", "partindex", "relbeg", "relend", "cohbeg", "cohend", "separ")]

## 1.2 Load the Anchor-data ======================================


# Load the anchor data for waves 12 to 14
anchor12_14 <- lapply(13:14, FUN=function(nr) {
  
  anchor_files <- list.files(file.path(path_pairfam, "DATA", "Stata"), pattern=paste0("anchor", nr), full.names=T)
  anchor_files <- lapply(anchor_files, read_dta)
  anchor_files <- bind_rows(anchor_files)
  
  return(anchor_files)
  
})

# Extract the data
w14 <- anchor12_14[[2]]
w13 <- anchor12_14[[1]]

# Create a vector with the variables
basic_vars <- c("id", "wave", "inty", "intm", "age", "sex_gen")
analytical_vars <- c("nkidsbio", "frt5", "isced", "hhincnet",
                     "isei", "hlt1", "frt1", "relstat", "ethni",
                     "cob",  "sat1i1", "sex8", "sex3", "sex5", 
                     "per2i3", "frt3", "job3")


# Select the variables
w14 <- w14[, c(basic_vars, analytical_vars)]
w13 <- w13[, c(basic_vars, analytical_vars)]
rm(analytical_vars, basic_vars, remove_vars)

### Data manipulation =====================================

# Create interview data
df <- bind_rows(w14, w13)

# Create the interview date
df$int_date <- as.Date(paste(df$inty, df$intm, "01", sep="-"))



# Clean the intended fertility
df$unclear_fertility_intention <- ifelse(df$frt5==-1, 1, 0)


# Create the missing variables
df[df < 0] <- NA


## Create the predictor variable ----------------------


## Socio-economic variables --------------------

# Education
df$education <- factor(df$isced, labels = c("0 currently enrolled", "1 no degree (1b)", 
                                            "2 lower secondary education (2b)", "3 lower secondary education (2a)", 
                                            "4 upper secondary education vocational (3b)", "5 upper secondary education general (3a)",
                                            "6 post-secondary non tertiary education general (4a)", "7 first stage of tertiary education (5)",
                                            "8 second stage of tertiary education (6)"), ordered=TRUE)


# Household Income
df$hhinc_decile <- cut(df$hhincnet, quantile(df$hhincnet, probs=seq(0, 1, by=0.1), na.rm=T), labels= paste(1:10, "decile"), include.lowest = T, ordered_result = TRUE)

# Position on social ladder
df$social_ladder <- cut(df$isei, breaks=seq(0, 100, by=10), labels=1:10, include.lowest = T, ordered_result = TRUE)

# Do you have a fixed-term work contract? (missings=7k)
df$tenure_job <- create_dummy(df$job3)


## Value expressions ---------------------

# Importance of religion


# Political orientation

## Health variables ---------------------


# Self-rated health
df$health <- factor(df$hlt1, labels=c("Poor", "Not well", "Alright", "Good", "Very good"), ordered=TRUE)

# Self-rated fecundity
df$fecundity <- factor(df$frt1, labels=c("Defintely", "Likely yes", "Likely no", "Impossible"), ordered=TRUE)

# Depression
df$depression <- factor(df$per2i3, labels = c("Almost never", "Sometimes", "Often", "Almost always"), ordered=TRUE)

# Desired level of education
df$desired_education <- cut(df$sat1i1, breaks=seq(0, 10, by=2), labels=1:5, include.lowest=T, ordered_result = T)


## Demographic variables ---------------


# Relationship status
df$relationship <- NA
df$relationship[df$relstat %in% c(9, 6, 1)] <- "Single"
df$relationship[df$relstat %in% c(2, 7, 10)] <- "Dating/in a sexual relationship"
df$relationship[df$relstat %in% c(3, 8, 11)] <-"Cohabiting"
df$relationship[df$relstat %in% c(4, 5)] <-"Married"

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

# Age
hist(df$age)


# Frequency of sexual intercourse
df$freq_coitus <- df$sex8

# Conception or pregnancy
df$conception <- df$sex5


## Create the outcome variables ------------------------

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
  mutate(birth = ifelse(conception==1|lag(parity) < parity|lead(parity)>parity, 1, 0))

# Create an indicator for childless or mother
df$childless <- ifelse(df$parity==0, 1, 0)

# Estimate the interview difference
df <- df %>% 
  arrange(id, wave) %>% 
  group_by(id) %>% 
  mutate(interview_gap = time_length(max(int_date)-min(int_date), unit="months")) %>% 
  select(-inty, -intm)

### Select the important variables ======================

# Create the proportions for wave 14 for the childless
categorical_vars <- c("ethnicity", "education",  "desired_education", "foreign_born", "social_ladder", "relationship", "depression", "health", "fecundity", "hhinc_decile")
continuous_vars <- c("intended_parity", "parity", "age")
outcome_vars <- c("conception", "intend_more_children", "reached_intended_parity", "trying", "birth", "childless", "unclear_fertility_intention")
vars <- mget(ls(pattern="vars$"))

# Select the variables
df <- df[, c("id", "sex_gen", "wave", vars$categorical_vars, vars$outcome_vars, vars$continuous_vars)]


# Remove missings
df <- df[!apply(df[, c("unclear_fertility_intention", "parity")], 1, function(x) any(is.na(x))), ]

# SAve the data
save(df, file="data/analysis_data.Rda")
save(vars, file="data/variables.Rda")

### END ###################################################