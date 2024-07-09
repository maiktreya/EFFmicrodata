## Final Revision 072024 for article "Spain, society of owners" @Miguel-Duch

library(data.table)
library(magrittr)
library(survey)

years <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020)
years <- c(2011)

for (i in years)
{
    sel_year <- i
    df <- fread(paste0("full/", sel_year, "/otras_secciones_", sel_year, "_imp1.csv"))
    selection <- colnames(df)[colnames(df) %like% "p2_9"]
    print(selection)
}
