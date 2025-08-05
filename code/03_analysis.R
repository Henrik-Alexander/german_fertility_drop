###
# Project: Explaining fertility drop in Germany
# Purpose: Analysis
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 2025/07/30
###


rm(list = ls()); gc(TRUE)

library(tidyverse)
library(broom)
library(stargazer)

# Load the functions
source("code/functions.R")
source("code/graphics.R")

# Load the data
load("data/analysis_data.Rda")

### Make the sample selection =========================

# Filter women
# Reduce to wave 13
df <- df[df$wave%in%c(3, 13) & df$sex_gen==2 & df$age>22 & df$age<44, ]

### Intending another birth ===========================


# Plot the share of wome, 
# "Who intends to have a(nother) birth in the prime reproductive years?" by motherhood status
ggplot(data=subset(df, sex_gen==2&wave%in%c(3, 13)&age>30), aes(x=factor(reached_intended_parity, labels=c("No", "Yes")), y=..prop.., fill=factor(childless), group=childless)) +
         geom_bar(position=position_dodge()) +
  scale_fill_brewer(palette="Set1", labels=c("Mothers", "Childless Women")) +
  scale_x_discrete("Intend any children") +
  scale_y_continuous(labels=scales::percent, n.breaks=10, limits=c(0, 1), expand=c(0, 0)) +
  facet_wrap(~wave) +
  guides(fill=guide_legend(position="bottom")) +
  theme(legend.title=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major.y=element_line(colour="grey"))



# Predictors of fertility intentions  ==============

# Modelling:
# Make ordered to factor variables
df <- mutate(df, across(where(is.ordered), ~factor(.x, ordered=F)))

# Create the childbearing intentions
df$delayed_childbearing_intention <- ifelse(df$reached_intended_parity==0, 1, 0)
df$parity_truncated <- factor(ifelse(df$parity>3, 3, df$parity))

# Odds ratios from logistic regression models
# predicting delayed childbearing intentions at Wave 13 by motherhood status at Wave 13
model <- as.formula(delayed_childbearing_intention~parity_truncated+education+hhinc_quartile+social_ladder+economic_hardship+education+desired_education+fecundity+health+relationship+bula+factor(bik))
mod1 <- glm(model, data=df, family="binomial")

# Create a model summary
mod_table <- tidy(mod1, conf.int = T, exponentiate=T)
stargazer(mod_table, summary=F)

# Plot the result
mod_table$variable <- str_extract(mod_table$term, pattern="^[a-z]+")

# Plot the predictor
ggplot(data=subset(mod_table, !is.na(variable)), aes(x=fct_reorder(term, term))) +
  geom_hline(yintercept=1) +
  geom_linerange(aes(ymin=conf.low, ymax=conf.high)) +
  geom_point(aes(y=estimate)) +
  scale_y_continuous(expand=c(0, 0.1)) +
  coord_flip(ylim=c(0, 5)) +
  facet_wrap(~ variable, scales="free_y")

ggsave("figures/predictors_intend_childbirth.pdf", height=25, width=20, unit="cm")


# Predictors of intention realization =================

# Odds ratios from logistic regression models
# predicting delayed childbearing intentions at Wave 13 by motherhood status at Wave 13
model <- as.formula(intention_realization~intend_childbirth+parity_truncated+education+hhinc_quartile+social_ladder+economic_hardship+education+desired_education+fecundity+health+relationship+bula+factor(bik))
mod2 <- glm(model, data=df, family="binomial")

# Create a model summary
mod_table <- tidy(mod2, conf.int = T, exponentiate=T)
stargazer(mod_table, summary=F)

# Plot the result
mod_table$variable <- str_extract(mod_table$term, pattern="^[a-z]+")

# Plot the predictor
ggplot(data=subset(mod_table, !is.na(variable)), aes(x=fct_reorder(term, term))) +
  geom_hline(yintercept=1) +
  geom_linerange(aes(ymin=conf.low, ymax=conf.high)) +
  geom_point(aes(y=estimate)) +
  scale_y_continuous(expand=c(0, 0.1)) +
  coord_flip(ylim=c(0, 5)) +
  facet_wrap(~ variable, scales="free_y")

ggsave("figures/predictors_realization_intentions.pdf", height=25, width=20, unit="cm")


### END ###############################################
