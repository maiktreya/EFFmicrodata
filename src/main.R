### EFF STUDY 2023 MIGUEL GARCIA DUCH

## NEEDED LIBRARIES
library("magrittr")
library("data.table")
library("survey")
c("lib/my_functions.R", "lib/widgets.R") %>% sapply(source)

## GLOBAL VARIABLES DECLARATION
start_time <- Sys.time()
years <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020)

## STRUCTURED LOOP OVER SURVEYS WITH MEMORY CLEANING EACH ITERATION, FIRST INCLUDING ALL PEOPLE, THEN REDUCED TO THOSE WHERE VARS ARE > 0
for (i in seq_along(years)) {
    sel_year <- years[i]
    source("src/EFF-out-full.R")
    to_export <- data.table(survey_weights$variables)
    to_export %>% fwrite(paste0(".datasets/", sel_year, "-EFF.microdat.csv"))
    rm(list = setdiff(ls(), c("years", "start_time")))
}

## BENCHMARK PERFORMANCE THROUGH TOTAL TIME OF EXECUTION AND SAVE FINAL STATE
(Sys.time() - start_time) %>% print()
