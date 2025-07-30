###
# Project: Explaining fertility drop in Germany
# Purpose: Descriptives
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 2025/07/30
###

rm(list = ls()); gc(TRUE)

library(tidyverse)
library(survey)

# Load the functions
source("code/functions.R")
source("code/graphics.R")

# Load the data
load("data/analysis_data.Rda")
load("data/variables.Rda")

### Save the nubmer of missing values =========

# Collect the number of missing values
missing_counts <- apply(df[df$wave==13, c(vars$categorical_vars, vars$continuous_vars, vars$outcome_vars)], 2, function(x) sum(is.na(x)))
missing_share <- apply(df[df$wave==13, c(vars$categorical_vars, vars$continuous_vars, vars$outcome_vars)], 2, function(x) round(100*mean(is.na(x)), 2))
missings <- cbind(missing_counts, missing_share)

### Data preperation ==========================

# Select the intention variable
for (sex in c(1,2)) {
  for (wave in c(13, 14)) {
   assign(paste0("w", wave, ifelse(sex==1, "m", "f")), df[df$sex_gen==sex&df$wave==wave, ])
  }
}


### Univariate statistics =====================

# Create the proportions
create_proportions <- function(variable, data) {
  x <- data[[variable]]
  counts <- tab(x)
  props <- round(100*prop.table(counts), 2)
  res <- as.data.frame(cbind(counts, props))
  rownames(res) <- NULL
  res$values <- names(counts)
  res$variable <- variable
  return(res[, c("variable", "values", "counts", "props")])
}

  # Create the mean
create_mean <- function(varname, data) {
  x <- data[[varname]]
 data.frame(variable = varname, values="mean", props = round(mean(x, na.rm=T), 2), counts=NA)
}




# Create the proportion data
props_childless <- bind_rows(lapply(vars$categorical_vars, create_proportions, data=w13f[w13f$childless==1, ]))
props_mothers <- bind_rows(lapply(vars$categorical_vars, create_proportions, data=w13f[w13f$childless==0, ]))

# Crete the means
means_childless <- bind_rows(lapply(vars$continuous_vars, create_mean, data=w13f[w13f$childless==1, ]))
means_mothers <- bind_rows(lapply(vars$continuous_vars, create_mean, data=w13f[w13f$childless==0, ]))


# Combine the data
props <- merge(props_mothers, props_childless, by=c("variable", "values"), suffixes=c("_mothers", "_childless"))
means <- merge(means_mothers, means_childless, by=c("variable", "values"), suffixes=c("_mothers", "_childless"))

# Create the final data
bind_rows(props, means)


### Correlations ==============================


### Figures ===================================

# Plot the figure
ggplot(data=w13f, aes(x=education, y=..prop.., group=childless, fill=as.factor(childless))) +
  geom_bar(position=position_dodge()) +
  coord_flip() +
  scale_x_discrete("Proportion", expand=c(0, 0)) +
  scale_y_continuous("Education", expand=c(0, 0), labels = scales::percent) +
  scale_fill_brewer("", palette="Set1", labels=c("Mother", "Childless")) +
  theme(
    axis.ticks.y=element_blank(),
    axis.line.y=element_blank(),
    legend.position.inside=c(0.8, 0.8)
  )

### END #######################################