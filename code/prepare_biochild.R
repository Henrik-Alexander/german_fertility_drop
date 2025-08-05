###
# Project: Explaining fertility drop in Germany
# Purpose: Data manipulation
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 2025/07/30
###




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