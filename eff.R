## Final Revision 072024 for article "Spain, society of owners" @Miguel-Duch
library(data.table)
library(magrittr)
library(survey)
tipo_empleo <- c(
    "worker",
    "capitalista",
    "jubilado",
    "inactivo-parado",
    "autonomo",
    "directivo"
)

rango_edad <- c(
    "0-34",
    "35-44",
    "45-54",
    "54-65",
    "65-75",
    "+75"
)
obj_list <- list()
full_mean <- list()
nueva <- list()
df_sv <- list()
y_working_class_size <- y_working_class_ratio <- y_w_owner <- y_owner <- working_class_size <- sample_size <- working_class_ratio <- c()
years <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020, 2022) # You can expand this later if
pensions_age <- pensions_age_class <- med_pensions_age <- med_pensions_age_class <- list()
# Young people homeownership
for (p in seq_along(years)) {
    full_mean[[p]] <- fread(paste0("datasets/eff/", years[p], "-EFF.microdat.csv"))
    full_mean[[p]][, homeowner := as.factor(np2_1)]
    full_mean[[p]][, young_homeowner := 0][homeowner == 1 & bage == 1, young_homeowner := 1]
    full_mean[[p]][, young_w_homeowner := 0][young_homeowner == 1 & nsitlabdom == 1, young_w_homeowner := 1]
    #### NAMED FILTERS
    full_mean[[p]][, nsitlabdom := factor(nsitlabdom,
        levels = c(1, 2, 3, 4, 5, 6),
        labels = tipo_empleo
    )]
    full_mean[[p]][, bage := factor(bage,
        levels = c(1, 2, 3, 4, 5, 6),
        labels = rango_edad
    )]
    transf <- full_mean[[p]] %>% data.frame()
    df_sv[[p]] <- svydesign(
        ids = ~1,
        data = transf,
        weights = ~ transf$facine3
    )
    y_owner[p] <- prop.table(svytable(~young_homeowner, subset(df_sv[[p]], bage == 1)))[2]
    y_w_owner[p] <- prop.table(svytable(~young_w_homeowner, subset(df_sv[[p]], bage == 1 & nsitlabdom == 1)))[2]
    pensions_age[[p]] <- cbind(svyby(~valor, ~bage, subset(df_sv[[p]], valor > 0), svymean), years[p])
    pensions_age_class[[p]] <- cbind(svyby(~valor, ~ bage + nsitlabdom, subset(df_sv[[p]], valor > 0), svymean), years[p])
    med_pensions_age[[p]] <- cbind(svyby(~valor, ~bage, subset(df_sv[[p]], valor > 0), svyquantile, quantiles = .5), years[p])
    med_pensions_age_class[[p]] <- cbind(svyby(~valor, ~ bage + nsitlabdom, subset(df_sv[[p]], valor > 0), svyquantile, quantiles = .5), years[p])
}


# Working class subsample size
for (z in seq_along(years)) {
    working_class_size[z] <- nrow(full_mean[[z]][nsitlabdom == 1, 1])
    y_working_class_size[z] <- nrow(full_mean[[z]][nsitlabdom == 1 & bage == 1, 1])

    sample_size[z] <- nrow(full_mean[[z]][, 1])
    working_class_ratio[z] <- working_class_size[z] / sample_size[z]
    y_working_class_ratio[z] <- y_working_class_size[z] / sample_size[z]
}

#------------------EFF RESULTS-----------------------------------------------------------#

## ECV sample size
sample_result_z <- cbind(years, sample_size, working_class_size, working_class_ratio, y_working_class_size, y_working_class_ratio)
print(sample_result_z)
fwrite(sample_result_z, "sample.csv")

# young homeowners (all and workers1)
homewoner_results <- cbind(years, y_owner, y_w_owner)
print(homewoner_results)
fwrite(homewoner_results, "homeowner.csv")
fwrite(cbind(rbindlist(pensions_age), rbindlist(med_pensions_age)), "pensions_age.csv")
fwrite(cbind(rbindlist(pensions_age_class), rbindlist(med_pensions_age_class)), "pensions_age_class.csv")
