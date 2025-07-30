


## Tabulate funciton =============================

tab <- function(x) {
  table(x, useNA = "always")
}


## Create dummy ===================================

create_dummy <- function(x) {
  ifelse(x==2, 0, 1)
}

### END ==========================================