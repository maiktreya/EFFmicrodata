## Final Revision 072024 for article "Spain, society of owners" @Miguel-Duch

library(data.table)
library(magrittr)
library(survey)

years <- c(2002, 2020)
sel_year <- years[1]

df <- fread(paste0("datasets/",sel_year ,"-EFF.microdat.csv"))

df_sv <- svydesign(ids = ~1, weights = df$facine3, data = df)

# GINI: working-class wealth of finanacial assets