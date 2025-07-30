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

# Reduce to wave 13
df <- df[df$wave==13&df$sex_gen==2&df$age>30, ]

### Intending another birth ===========================

# Plot the share of wome, 
# "Who intends to have a(nother) birth in the prime reproductive years?" by motherhood status
ggplot(data=subset(df, sex_gen==2&wave==13&age>30), aes(x=factor(reached_intended_parity, labels=c("No", "Yes")), y=..prop.., fill=factor(childless), group=childless)) +
         geom_bar(position=position_dodge()) +
  scale_fill_brewer(palette="Set1", labels=c("Mothers", "Childless Women")) +
  scale_x_discrete("Intend any children in Wave 13") +
  scale_y_continuous(labels=scales::percent, n.breaks=10, limits=c(0, 1), expand=c(0, 0)) +
  guides(fill=guide_legend(position="bottom")) +
  theme(legend.title=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major.y=element_line(colour="grey"))



### Delayed childbearing intentions at Wave 13 ====

#
df$delayed_childbearing_intention <- ifelse(df$reached_intended_parity==0, 1, 0)
df$parity_truncated <- factor(ifelse(df$parity>3, 3, df$parity))

# Odds ratios from logistic regression models
# predicting delayed childbearing intentions at Wave 13 by motherhood status at Wave 13
model <- as.formula(delayed_childbearing_intention~parity_truncated+education+hhinc_decile+social_ladder+desired_education+fecundity+health+depression+relationship+foreign_born+ethnicity)
mod1 <- glm(model, data=mutate(df, across(where(is.ordered), ~factor(.x, ordered=F))), family="binomial")

# Create a model summary
mod_table <- tidy(mod1, conf.int = T, exponentiate=T)
stargazer(mod_table, summary=F)

# Plot the result
mod_table$variable <- str_extract(mod_table$term, pattern="^[a-z]+")


ggplot(data=mod_table, aes(x=fct_reorder(term, term))) +
  geom_hline(yintercept=1) +
  geom_linerange(aes(ymin=conf.low, ymax=conf.high)) +
  geom_point(aes(y=estimate)) +
  scale_y_continuous(expand=c(0, 0.1)) +
  coord_flip(ylim=c(0, 5)) +
  facet_wrap(~ variable, scales="free_y") 


### END ###############################################
