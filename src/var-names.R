#### NAMED FILTERS
tipo_empleo <- c(
    "empleado por cuenta ajena",
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

filter_cat <- c(
    age = "bage",
    class = "nsitlabdom",
    class_bin = "p6_81"
)

filters_names_new <- c(
    homeownership = "np2_1", # p2_35_1 propiedades compradas (no primera vivienda)
    n_of_props_by_age_and_class = "p2_33",
    multiprop = "main_other",
    rental_income_eff = "p7_2",
    pension_funds_assets = "valor",
    financial_assets = "riquezafin",
    financial_liquid = "riquezaliq",
    renta_hogar = "renthog",
    debt_ratio = "pagodeuda",
    total_debt = "vdeuda",
    overburden = "overburden",
    net_worth = "riquezanet",
    main_residence_value = "np2_5",
    main_residence_debt = "dvivpral",
    other_real_assets_value = "otraspr",
    other_real_assets_debt = "deuoprop",
    gross_housing_assets = "gross_hous_ass",
    net_housing_assets = "net_hous_ass",
    inc_debt = "p2_26",
    deposed = "deposed",
    deposed2 = "deposed2",
    acc_cotiz = "p4_15",
    acc_no_cotiz = "p4_24",
    renta_fija = "p4_35",
    fondos_inv = "allf"
)

filter_names_class <- filter_names_bin <- filters_names_new
filter_names_age <- filters_names_new[names(filters_names_new) != "renta_fija"]