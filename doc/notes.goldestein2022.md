---
title: "NOTES GOLDSTEIN 2022"
author: "Miguel Garcia-Duch"
date: "5/22/2021"
format:
  html:
    code-fold: true
---

----------------------------------------
## THEORY OF THE PETIT RENTIER AS SOCIAL CLASS


```{r}
#| echo: true
### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
`%>%` <- magrittr::`%>%` # nolint
c("magrittr", "survey", "data.table", "stargazer") %>% sapply(library, character.only = T)
options(scipen = 99)
### PARAMETERS AND VARIABLES TO INITIALIZE
quantile_cuts <- c(.25, .5, .75, .9, .99, .999)
important_variables <- c("PB040", "HY040N", "DB040", "HY090G", "HY100N", "HY130N", "HH021", "PY035N", "PB140", "PL031", "PL040", "PL051", "PY010N", "PB110")
years <- c(2021, 2022)
final_props <- join_ecv <- data.table()
models <- models2 <- list()
sel_year <- years[1]
survey_ecv <- fread(paste0(".datasets/ecv/filtered_data", sel_year, ".csv"))
print(survey_ecv$HY040N)
```
----------------------------------------
## ECONOMETRIC METHODOLOGY




----------------------------------------
## PROPOSAL

Estimate model determinants of non.labour rents