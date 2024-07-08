library(magrittr)
library(data.table)
library(haven)

test <- read_dta(".datasets/HFCS/HFCS_UDB_2_5_STATA/d1.dta") %>% as.data.table()
