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
library(haven)

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

# Interview timing
ggplot(df, aes(x=int_date, fill=as.factor(wave))) +
  geom_histogram() +
  scale_x_date(date_breaks="1 month") +
  facet_wrap(~wave, scales="free_x") +
  guides(fill="none")

# Plot the interview gap
ggplot(df, aes(x=interview_gap, filll=as.factor(wave))) +
  geom_histogram() +
  facet_wrap(~ wave, scales="free_x")


# Plot the age groups
ggplot(df, aes(x=age, fill=as.factor(cohort))) +
  geom_histogram() +
  facet_wrap( ~ wave, ncol=1) +
  scale_x_continuous(n.breaks=40) +
  scale_fill_viridis_d("Cohort") +
  guides(fill=guide_legend(position="bottom"))
ggsave(filename="figures/age_distribution.pdf", height=25, width=20, unit="cm")

# Plot the distribution
ggplot(df, aes(x=wave, y=age, group=interaction(cohort, wave), colour=cohort)) +
  geom_abline(slope=1, intercept=0:50, colour="lightgrey", linetype="dotted") +
  geom_jitter(size=0.1, alpha=0.1) +
  geom_boxplot(linewidth=1.3) +
  scale_x_continuous("Wave", n.breaks=12, sec.axis = sec_axis(~ ., breaks=c(3, 4, 13, 14), labels = c("2010-11", "2012-13", "2020-21", "2021-22"))) +
  scale_y_continuous("Age", n.breaks=20) +
  guides(colour=guide_legend(position="bottom"))
ggsave(filename="figures/data_structure.pdf", height=15, width=25, unit="cm")

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


## Who intends to have a child
df %>% 
  filter(wave%in%c(3, 13) & age < 44 & age>25) %>% 
  group_by(wave, age_group) %>% 
  mutate(total = n()) %>% 
  group_by(wave, age_group, intend_childbirth) %>% 
  summarise(share = n()/unique(total), count=n(), .groups="drop") %>% 
  ggplot(aes(x=intend_childbirth, y=share, group=wave, fill=factor(wave))) +
  geom_col(position=position_dodge()) +
  facet_wrap(~ age_group, ncol=1) +
  geom_text(aes(label=paste0(round(100*share, 1), "%"), y=share+0.02), position=position_dodge(width=1), colour="black", size=5, family="serif") +
  scale_y_continuous(labels=scales::percent, n.breaks=10, expand=c(0, 0), limits=c(0, 0.71)) +
  scale_x_discrete("Intend to have a child in the next 2 years") +
  scale_fill_viridis_d("Wave", option="D") +
  guides(fill=guide_legend(position="bottom")) +
  theme(
    panel.grid.major.y=element_line(colour="black", linetype="dotted")
  )
ggsave(filename="figures/plot_the_fertility_intentions.pdf", height=35, width=25, unit="cm")


### Correlations ==============================

# Relationship between intention and conception
df %>% 
  filter(wave%in%c(3, 13) & !is.na(intend_childbirth)) %>% 
  group_by(intend_childbirth) %>% 
  mutate(total=n()) %>% 
  group_by(intend_childbirth, conception) %>% 
  summarize(share = n()/unique(total)) %>% 
  filter(conception==1) %>% 
  ggplot(aes(x=intend_childbirth, y=share, fill=intend_childbirth)) +
  geom_col() +
  geom_text(aes(y=share+0.005, label=paste(100*round(share, 2), "%")), family="serif", size=8) +
  scale_x_discrete("Intend to have a child") +
  scale_y_continuous("Realized childbirth", labels=scales::percent, expand=c(0, 0), limits=c(0, 0.1), n.breaks=10) +
  scale_fill_grey() +
  guides(fill="none") +
  theme(
    panel.grid.major.y=element_line(colour="black")
  )
ggsave(filename="figures/realized_childbirth.pdf", height=15, width=25, unit="cm")


### Figures ===================================

# Plot the figure
ggplot(data=subset(df, age>30), aes(x=education, y=..prop.., group=childless, fill=as.factor(childless))) +
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
ggplot(filename="figures/childlessness.pdf", height=15, width=20, unit="cm")
### END #######################################