# DATA ADQUISITION FROM ENCUESTA FINANCIERA DE FAMILIAS 2020 (@miguel-garcia-duch 22-02-2024rev)
library(data.table)
library(survey)
rm(list = ls())

dt <- list()

for (i in 1:5) {
    dt[[i]] <- fread(paste0(".dataasets/full/EFF_2020/otras_secciones_imp", i, ".csv"))
    dt[[i]][, IDENPER := .I]
    dt[[i]][, IDENHOG := h_2020][, h_2020 := NULL]
    dt[is.na(dt)] <- 0

    # HAS OTHER PROPERTIES (2_42) + IS HOUSING (2_43) + GENERATES RENTS(2_43)
    # 3 first properties
    dt[[i]][, prop1 := 0][p2_42_1 == 1 & p2_35a_1 & p2_43_1 > 0, prop1 := p2_43_1]
    dt[[i]][, prop2 := 0][p2_42_2 == 1 & p2_35a_2 & p2_43_2 > 0, prop2 := p2_43_2]
    dt[[i]][, prop3 := 0][p2_42_3 == 1 & p2_35a_3 & p2_43_3 > 0, prop3 := p2_43_3]

    # remaining +3
    dt[[i]][, prop41 := 0][p2_42s1_4 == 1 & p2_35a_4 & p2_43_4 > 0, prop41 := 1]
    dt[[i]][, prop42 := 0][p2_42s2_4 == 1 & p2_35a_4 & p2_43_4 > 0, prop42 := 1]
    dt[[i]][, prop43 := 0][p2_42s3_4 == 1 & p2_35a_4 & p2_43_4 > 0, prop43 := 1]
    dt[[i]][, prop44 := 0][p2_42s4_4 == 1 & p2_35a_4 & p2_43_4 > 0, prop44 := 1]
    dt[[i]][, prop45 := 0][p2_42s5_4 == 1 & p2_35a_4 & p2_43_4 > 0, prop45 := 1]
    dt[[i]][, prop4 := prop41 + prop42 + prop43 + prop44 + prop45]
    dt[[i]][prop4 > 0, prop4 := p2_43_4]

    # total rent, maybe problematic as +3 props rents are reported jointly in p2_43_4
    dt[[i]][, RENTA_ALQ := prop1 + prop2 + prop3 + prop4]
}

# Merge implications and construct consistently remaininfg vars (avoid non integer results)
dt <- (dt[[1]] + dt[[2]] + dt[[3]] + dt[[4]] + dt[[5]]) / 5
dt[, PAR150 := 0][RENTA_ALQ > 0, PAR150 := 1]
dt[, INQUILINO2 := 0][p2_1 == 1, INQUILINO2 := 1]
dt[, PROPIETAR := 0][p2_1 == 2 & PAR150 == 0, PROPIETAR := 1]
dt[, PATINMO := PROPIETAR][, FACTORCAL := facine3][, RENTAD := renthog]


fwrite(dt, "/home/other/dev/examples/R/javi-IEF/data/EFF-2020-new.gz")
