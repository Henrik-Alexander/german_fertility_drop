###
# Project: Explaining fertility drop in Germany
# Purpose: Functions
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 2025/07/30
###



## Tabulate funciton =============================

tab <- function(x) {
  table(x, useNA = "always")
}


## Create dummy ===================================

create_dummy <- function(x) {
  ifelse(x==2, 0, 1)
}

### END ==========================================