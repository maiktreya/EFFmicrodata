## THIS PARTIAL SCRIPT SHOULD BE USED TOGETHER WITH EFF_API MAIN ONLY TO INCORPORATE NEW EFF RELEASES OR MODIFY VARIABLE DEFINITIONS. IT USES ORIGINAL BDE .dta FILES (SLOW)
library(data.table)
library(magrittr)

sel_year <- "2022"
path <- paste0("datasets/full/EFF_", sel_year, "/")
source("src/data_selectors.R")

##### SELECTED VARIABLES IDS ANDcolnames
full_selection <- sapply(paste0(path, selectors_eff_no_sec6), haven::read_dta)
full_selection %>% data.table()
###### DATA TRANSFORMATION
full_mean <-
    (full_selection[, paste0(path, "otras_secciones_imp1.dta")] %>% sapply(as.numeric) + # nolint
        full_selection[, paste0(path, "otras_secciones_imp2.dta")] %>% sapply(as.numeric) +
        full_selection[, paste0(path, "otras_secciones_imp3.dta")] %>% sapply(as.numeric) +
        full_selection[, paste0(path, "otras_secciones_imp4.dta")] %>% sapply(as.numeric) +
        full_selection[, paste0(path, "otras_secciones_imp5.dta")] %>% sapply(as.numeric)) / 5


full_mean %>% fwrite(paste0("datasets/", sel_year, "-EFF.microdat.csv"))
