# AGE PB140 YEAR OF BIRTH
# AGE $AGE CLUSTER OF AGE
# CLASS PL031
# GENDER PB150
# REGION DB040
# HOUSEHOLD INCOME HY020

# GRADO URBANIZACIÓN DB100
# 1 - hig-population density(+50k + 500 h/km)
# 2 - mid-population density(+50k or adyacent + 100 h/km)
# 3 - low-population denstiy(else)

# TENANCY HH021
# 1. En propiedad sin hipoteca
# 2. En propiedad con hipoteca
# 3. En alquiler o realquiler a precio de mercado
# 4. En alquiler o realquiler a precio inferior al de mercado
# 5. En cesión gratuita


# FINANCIAL RENTS
# HY040N,Renta neta procedente del alquiler de una propiedad o terreno en el año anterior al de encuesta
# HY080N,Transferencias periódicas monetarias percibidas de otros hogares en el año anterior al de encuesta
# HY081N,Transferencias periódicas monetarias percibidas de otros hogares en el año anterior al de encuesta (pensiones alimenticias a hijos o compensatorias a cónyuges)
# HY090N,'Intereses, dividendos y ganancias netos de inversiones de capital en empresas no constituidas en sociedad en el año anterior al de encuesta'
# HY090G,'Intereses, dividendos y ganancias brutos de inversiones de capital en empresas no constituidas en sociedad en el año anterior al de encuesta'

# ORDEN DE FILTRADO VARIABLES
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

for (i in seq_along(years)) {
    sel_year <- years[i]
    survey_ecv <- fread(paste0(".datasets/ecv/filtered_data", sel_year, ".csv"))
    ifelse(sel_year %in% c(2021, 2022), survey_ecv[, PL040C := PL040 + PL040B], survey_ecv[, PL040C := PL040])
    # survey_ecv[PL040C %in% c(0, 4), PL040C := NA]
    ## PB040 FACTORE ELEVACIÓN, HB070 CÓDIGO DEL REPORTADOR
    survey_ecv$tenancy <- survey_ecv$HH021
    survey_ecv$rents <- survey_ecv[, HY040N + HY090N]
    survey_ecv[rents != 0, log_rents := log(rents)]
    survey_ecv[, rentsbi := 0][rents / HY020 >= 0.1, rentsbi := 1]
    # survey_ecv[, rentsbi := 0][rents >= 1000, rentsbi := 1]
    survey_ecv[, direc := 0][PL051 %in% c(1, 11, 12, 13, 14, 15, 16, 17, 18, 19), `:=`(direc = 1)]
    survey_ecv[direc == 1 & PL040C == 1, `:=`(PL040C = 4)]
    survey_ecv[, treatment := 0][PL040C == 1, treatment := 1]
    survey_ecv[, outcome := 0][PL040C == 1, outcome := rentsbi]
    survey_ecv[, period := 0][HB060 == tail(years, 1), period := 1]
    survey_ecv$AGE <- survey_ecv$AGE %>% as.factor()
    # survey_ecv[tenancy != 1, "tenancy"] <- 0
    survey_ecv[AGE == 0, AGE := NA]
    survey_ecv[PL040C == 0, PL040C := NA]
    educ_cat <- c("non-applicant", "primary", "secondary", "fp", "medium", "higher")
    isco08 <- c(
        "Armed Forces Occupations",
        "Managers", "Professionals", "Technicians and Associate Professionals",
        "Clerical Support Workers", "Service and Sales Workers",
        "Skilled Agricultural, Forestry and Fishery Workers",
        "Craft and Related Trades Workers",
        "Plant and Machine Operators, and Assemblers",
        "Elementary Occupations"
    )
    setnames(survey_ecv,
        old = c("PB190", "PL051", "PE041", "RB090", "HH021", "PL040C", "AGE", "RB290", "PB140", "HX040", "HY040N", "HY090N"),
        new = c("civil", "ocup", "educ", "sex", "tenancy", "class", "bage", "country", "birth", "members", "housrent", "profit")
    )

    survey_ecv$ocup <- factor(substring(survey_ecv$ocup, 1, 1), levels = c(0:9), labels = isco08) %>% relevel(ref = "Elementary Occupations")
    survey_ecv$educ <- factor(as.integer(survey_ecv$educ / 100), levels = c(0:5), labels = educ_cat) %>% relevel(ref = "secondary")
    survey_ecv$class <- factor(survey_ecv$class, levels = c(1, 2, 3, 4), labels = c("employer", "self-employed", "worker", "manager")) %>% relevel(ref = "worker")
    survey_ecv$bage <- factor(survey_ecv$bage, levels = c(1, 2, 3, 4, 5, 6), labels = c("0-34", "35-44", "45-54", "54-65", "65-75", "75")) %>% relevel(ref = "45-54")
    survey_ecv$sex <- factor(survey_ecv$sex, levels = c(1, 2), labels = c("Man", "Women"))
    survey_ecv$housrent <- factor(as.integer(survey_ecv$housrent > 0), levels = c(0, 1), labels = c("Norent", "Yesrent"))
    survey_ecv$profit <- factor(as.integer(survey_ecv$profit > 0), levels = c(0, 1), labels = c("Noprofit", "Yesprofit"))
    survey_ecv$country <- factor(survey_ecv$country)
    survey_ecv$tenancy <- factor(survey_ecv$tenancy)
    survey_ecv$civil <- factor(survey_ecv$civil)

    join_ecv <- rbind(join_ecv, survey_ecv, fill = T)

    survey_total <- as.svydesign2(svydesign(
        ids = ~1,
        data = survey_ecv,
        weights = ~ survey_ecv$PB040
    ))

    models[[i]] <- svyglm(rentsbi ~ bage + country + educ + members + civil + sex + class + tenancy + educ, design = survey_total, family = "quasibinomial")
    models2[[i]] <- lm(log_rents ~ bage + country + educ + members + civil + sex + class + tenancy + educ, weights = PB040, data = survey_ecv)
}

sink(paste0("output/ECV/RENTSBI", substr(years[1], 3, 4), "-", substr(years[2], 3, 4), ".txt"))
models[[1]] %>%
    stargazer(type = "text") %>%
    print()
models[[2]] %>%
    stargazer(type = "text") %>%
    print()
sink()


sink(paste0("output/ECV/LN_RENTS", substr(years[1], 3, 4), "-", substr(years[2], 3, 4), ".txt"))
models2[[1]] %>%
    stargazer(type = "text") %>%
    print()
models2[[2]] %>%
    stargazer(type = "text") %>%
    print()
sink()
