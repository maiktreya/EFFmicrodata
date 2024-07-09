## Final Revision 072024 for article "Spain, society of owners" @Miguel-Duch

library(data.table)
library(magrittr)
library(survey)

years <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020)
years <- c(2011)
obj <- list()

for (i in years) {
    total_df <- fread(paste0("datasets/",sel_year ,"-EFF.microdat.csv"))

    for (j in 1:5) {{ sel_year <- i
        df <- fread(paste0("full/", sel_year, "/otras_secciones_", sel_year, "_imp", j, ".csv"))
        selection <- colnames(df)[colnames(df) %like% "p2_9b_"]
        obj[[j]] <- df[, ..selection] }}
}
