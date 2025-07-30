###
# Project: Explaining fertility drop in Germany
# Purpose: Meta-file
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 2025/07/30
###



### Create the folder structure ============================

# Create a list of folders
folders <- c("raw", "data", "code", "figures", "tables")
lapply(folders, FUN=function(folder) if(!dir.exists(folder)) dir.create(folder))

### Run the Code files ====================================




### END ###################################################