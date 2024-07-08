### API REQUEST AND TRANSFORMATION OF EFF 2017 FROM STATA .dta FILES
library(dplyr)
c("rdbnomics", "tidyr", "httr") %>% sapply(library, character.only = TRUE)
c("LIBRARY/my_functions.R", "LIBRARY/widgets.R") %>% sapply(source)

source("ECV_API.last.R")
source("EFF_API.last.R")
source("EFF_API.main.R")

setwd("RESULTS_EXTRACTION")
file_list <- list.files(pattern = "*.csv", full.names = TRUE)
# Read all csv files in the folder and create a list of dataframes
ldf <- lapply(file_list, read.csv) %>% unname()

# Combine each dataframe in the list into a single dataframe
write.csv2(ldf, "CSV/final_together.csv")