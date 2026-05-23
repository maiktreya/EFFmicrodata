# test_survey.R
library(data.table)
library(survey)

# 1) cargar el microdato combinado
eff2002 <- fread("datasets/eff/2002-EFF.microdat.csv")
eff1005 <- fread("datasets/eff/2005-EFF.microdat.csv")
eff1008 <- fread("datasets/eff/2008-EFF.microdat.csv")
eff1011 <- fread("datasets/eff/2011-EFF.microdat.csv")
eff1014 <- fread("datasets/eff/2014-EFF.microdat.csv")
eff1017 <- fread("datasets/eff/2017-EFF.microdat.csv")
eff1020 <- fread("datasets/eff/2020-EFF.microdat.csv")
eff1022 <- fread("datasets/eff/2022-EFF.microdat.csv")

eff <- rbind(eff2002, eff1005, eff1008, eff1011, eff1014, eff1017, eff1020, eff1022, fill = TRUE)


fwrite(eff, "datasets/full_eff.gz", compress = "gzip")
