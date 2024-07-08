## THIS PARTIAL SCRIPT SHOULD BE USED TOGETHER WITH EFF_API MAIN ONLY TO INCORPORATE NEW EFF RELEASES OR MODIFY VARIABLE DEFINITIONS. IT USES ORIGINAL BDE .dta FILES (SLOW)
path <- paste0(".datasets/full/EFF_", sel_year, "/")
source("src/data_selectors.R")
##### SELECTED VARIABLES IDS ANDcolnames
full_selection <- sapply(paste0(path, selectors_eff_no_sec6), haven::read_dta)
full_selection %>% data.table()
###### DATA TRANSFORMATION
full_mean <-
    (full_selection[, paste0(path, "otras_secciones_imp1.dta")] %>% sapply(as.numeric) + # nolint
        full_selection[, paste0(path, "otras_secciones_imp2.dta")] %>% sapply(as.numeric) +
        full_selection[, paste0(path, "otras_secciones_imp3.dta")] %>% sapply(as.numeric) +
        full_selection[, paste0(path, "otras_secciones_imp4.dta")] %>% sapply(as.numeric) +
        full_selection[, paste0(path, "otras_secciones_imp5.dta")] %>% sapply(as.numeric)) / 5
kind <- c("p6_30_1", "p6_30_2", "p6_30_3", "p6_30_4", "p6_30_5", "p6_30_6", "p6_30_7", "p6_30_8")
class <- c("p6_1c1_1", "p6_1c2_1", "p6_1c3_1", "p6_1c4_1", "p6_1c5_1", "p6_1c6_1", "p6_1c7_1", "p6_1c8_1")
valor_2002_2005 <- c("p5_7_1", "p5_7_2", "p5_7_1", "p5_7_1", "p5_7_1", "p5_7_1", "p5_7_1", "p5_7_1", "p5_7_1", "p5_7_1")
s6_1 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp1.dta"))[class]
s6_2 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp2.dta"))[class]
s6_3 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp3.dta"))[class]
s6_4 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp4.dta"))[class]
s6_5 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp5.dta"))[class]
s6_sum <- (s6_1 + s6_2 + s6_3 + s6_4 + s6_5) / 5
s6_sum1 <- s6_sum$p6_1c1_1
s6_sum2 <- s6_sum$p6_1c2_1
s6_sum3 <- s6_sum$p6_1c3_1
s6_sum4 <- s6_sum$p6_1c4_1
s6_sum5 <- s6_sum$p6_1c5_1
s6_sum6 <- s6_sum$p6_1c6_1
s6_sum7 <- s6_sum$p6_1c7_1
s6_sum8 <- s6_sum$p6_1c8_1

s637_1 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp1.dta"))[c("p6_37_1_1")]
s637_2 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp2.dta"))[c("p6_37_1_1")]
s637_3 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp3.dta"))[c("p6_37_1_1")]
s637_4 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp4.dta"))[c("p6_37_1_1")]
s637_5 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp5.dta"))[c("p6_37_1_1")]
s637_1[is.na(s637_1)] <- 0
s637_2[is.na(s637_2)] <- 0
s637_3[is.na(s637_3)] <- 0
s637_4[is.na(s637_4)] <- 0
s637_5[is.na(s637_5)] <- 0
p6_37 <- (s637_1 + s637_2 + s637_3 + s637_4 + s637_5) / 5


if (sel_year %in% c(2002, 2005)) {
    empre_1 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp1.dta"))[c("p6_30_1")]
    empre_2 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp2.dta"))[c("p6_30_1")]
    empre_3 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp3.dta"))[c("p6_30_1")]
    empre_4 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp4.dta"))[c("p6_30_1")]
    empre_5 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp5.dta"))[c("p6_30_1")]
    empre <- (empre_1 + empre_2 + empre_3 + empre_4 + empre_5) / 5
    auton_1 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp1.dta"))[c("p6_36_1_1")]
    auton_2 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp2.dta"))[c("p6_36_1_1")]
    auton_3 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp3.dta"))[c("p6_36_1_1")]
    auton_4 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp4.dta"))[c("p6_36_1_1")]
    auton_5 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp5.dta"))[c("p6_36_1_1")]
    auton <- (auton_1 + auton_2 + auton_3 + auton_4 + auton_5) / 5
} else {
    empre_1 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp1.dta"))[c("p6_1c2_1")]
    empre_2 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp2.dta"))[c("p6_1c2_1")]
    empre_3 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp3.dta"))[c("p6_1c2_1")]
    empre_4 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp4.dta"))[c("p6_1c2_1")]
    empre_5 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp5.dta"))[c("p6_1c2_1")]
    empre <- (empre_1 + empre_2 + empre_3 + empre_4 + empre_5) / 5
    # auton_alt <- (full_mean[,"p4_105_1"]+full_mean[,"p4_105_2"]+full_mean[,"p4_105_3"]+full_mean[,"p4_105_4"]+full_mean[,"p4_105_5"]+full_mean[,"p4_105_6"]) /6 # nolint
    auton <- full_mean[, "p4_105_1"]
}

s6b_dir1 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp1.dta"))[c("p6_3_1")]
s6b_dir2 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp2.dta"))[c("p6_3_1")]
s6b_dir3 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp3.dta"))[c("p6_3_1")]
s6b_dir4 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp4.dta"))[c("p6_3_1")]
s6b_dir5 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp5.dta"))[c("p6_3_1")]
s6b_dir <- (s6b_dir1 + s6b_dir2 + s6b_dir3 + s6b_dir4 + s6b_dir5) / 5
s6b_dir <- as.numeric(unlist(s6b_dir))
s6b_dir[!(s6b_dir %in% 1)] <- NA

s6b_1 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp1.dta"))[c("p6_30_1")]
s6b_2 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp2.dta"))[c("p6_30_1")]
s6b_3 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp3.dta"))[c("p6_30_1")]
s6b_4 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp4.dta"))[c("p6_30_1")]
s6b_5 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp5.dta"))[c("p6_30_1")]
s6b_sum <- (s6b_1 + s6b_2 + s6b_3 + s6b_4 + s6b_5) / 5

s681_1 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp1.dta"))[c("p6_81_1")] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
s681_2 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp2.dta"))[c("p6_81_1")] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
s681_3 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp3.dta"))[c("p6_81_1")] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
s681_4 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp4.dta"))[c("p6_81_1")] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
s681_5 <- haven::read_dta(paste0(".datasets/full/EFF_", sel_year, "/seccion6_imp5.dta"))[c("p6_81_1")] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
### IMPORTANT! HERE WE DEFINE:
# A) Autonomous workers, which didnt were computed untin 2017
s681_1[s681_1 == 21] <- 2
s681_1[s681_1 == 22] <- 3
s681_2[s681_2 == 21] <- 2
s681_2[s681_2 == 22] <- 3
s681_3[s681_3 == 21] <- 2
s681_3[s681_3 == 22] <- 3
s681_4[s681_4 == 21] <- 2
s681_4[s681_4 == 22] <- 3
s681_5[s681_5 == 21] <- 2
s681_5[s681_5 == 22] <- 3
p6_81 <- (s681_1 + s681_2 + s681_3 + s681_4 + s681_5) / 5
# B) Perform imputations for intermediate cases assigning the closest integer probability
p6_81[p6_81 == 1.4] <- 1
p6_81[p6_81 == 1.6] <- 2
p6_81[p6_81 == 1.8] <- 2
p6_81[p6_81 == 1.2] <- 1

fp_1 <- full_selection[, paste0(path, "otras_secciones_imp1.dta")][c("p5_7_1", "p5_7_2", "p5_7_3", "p5_7_4", "p5_7_5", "p5_7_6", "p5_7_7", "p5_7_8", "p5_7_9", "p5_7_10")] %>% as.data.frame()
fp_2 <- full_selection[, paste0(path, "otras_secciones_imp2.dta")][c("p5_7_1", "p5_7_2", "p5_7_3", "p5_7_4", "p5_7_5", "p5_7_6", "p5_7_7", "p5_7_8", "p5_7_9", "p5_7_10")] %>% as.data.frame()
fp_3 <- full_selection[, paste0(path, "otras_secciones_imp3.dta")][c("p5_7_1", "p5_7_2", "p5_7_3", "p5_7_4", "p5_7_5", "p5_7_6", "p5_7_7", "p5_7_8", "p5_7_9", "p5_7_10")] %>% as.data.frame()
fp_4 <- full_selection[, paste0(path, "otras_secciones_imp4.dta")][c("p5_7_1", "p5_7_2", "p5_7_3", "p5_7_4", "p5_7_5", "p5_7_6", "p5_7_7", "p5_7_8", "p5_7_9", "p5_7_10")] %>% as.data.frame()
fp_5 <- full_selection[, paste0(path, "otras_secciones_imp5.dta")][c("p5_7_1", "p5_7_2", "p5_7_3", "p5_7_4", "p5_7_5", "p5_7_6", "p5_7_7", "p5_7_8", "p5_7_9", "p5_7_10")] %>% as.data.frame()
fp_1[is.na(fp_1)] <- 0
fp_2[is.na(fp_2)] <- 0
fp_3[is.na(fp_3)] <- 0
fp_4[is.na(fp_4)] <- 0
fp_5[is.na(fp_5)] <- 0

## p2_9
f1 <- full_mean[, "p2_9_1"]
f2 <- full_mean[, "p2_9_2"]
f3 <- full_mean[, "p2_9_3"]
f4 <- full_mean[, "p2_9_4"]
f1[is.na(f1)] <- 0
f2[is.na(f2)] <- 0
f3[is.na(f3)] <- 0
f4[is.na(f4)] <- 0
f1[f1 != 7] <- 0
f2[f2 != 7] <- 0
f3[f3 != 7] <- 0
f4[f4 != 7] <- 0
f0 <- (f1 + f2 + f3 + f4) / 4
full_mean[, "p2_9_1"] <- f0

## p2_39
f1 <- full_mean[, "p2_39_1"]
f2 <- full_mean[, "p2_39_2"]
f3 <- full_mean[, "p2_39_3"]
f4 <- full_mean[, "p2_39_4"]
f1[is.na(f1)] <- 0
f2[is.na(f2)] <- 0
f3[is.na(f3)] <- 0
f4[is.na(f4)] <- 0
f0 <- (f1 + f2 + f3 + f4) / 4
full_mean[, "p2_39_1"] <- f0


## p3_2
if (sel_year %in% c(2002, 2005)) {
    p3_2_names <- c("p3_2_1", "p3_2_2", "p3_2_3", "p3_2_4")
    p3_2 <- full_mean[, p3_2_names]
    p3_2[is.na(p3_2)] <- 0
    p3_2[p3_2 != 10] <- 0
    p3_2 <- rowSums(p3_2[, c(1:4)], na.rm = TRUE)
} else {
    p3_2_names <- c("p3_2_1", "p3_2_2", "p3_2_3", "p3_2_4", "p3_2_5", "p3_2_6", "p3_2_7", "p3_2_8")
    p3_2 <- full_mean[, p3_2_names]
    p3_2[is.na(p3_2)] <- 0
    p3_2[p3_2 != 10] <- 0
    p3_2 <- rowSums(p3_2[, c(1:8)], na.rm = TRUE)
}
p3_2[p3_2 >= 10] <- 1
full_mean[, "p3_2_1"] <- p3_2


## p3_2a
if (!(sel_year %in% c(2002, 2005))) {
    p3_2a_names <- c("p3_2a_1", "p3_2a_2", "p3_2a_3", "p3_2a_4", "p3_2a_5", "p3_2a_6", "p3_2a_7", "p3_2a_8")
    p3_2a <- full_mean[, p3_2a_names]
    p3_2a[is.na(p3_2a)] <- 0
    p3_2a[p3_2a > 1] <- 0
    p3_2a <- rowSums(p3_2a[, c(1:8)], na.rm = TRUE)
    p3_2a[p3_2a > 1] <- 1
}


## p2_23
p2_23 <- full_mean[, "p2_23"]
p2_23[is.na(p2_23)] <- 0
p2_23[p2_23 > 1] <- 0
full_mean[, "p2_23"] <- p2_23

## p2_18_1 pagos prestamos viv_principal
pagopr_vivpr_names <- c("p2_18_1", "p2_18_2", "p2_18_3", "p2_18_4")
pagopr_vivpr <- full_mean[, pagopr_vivpr_names]
pagopr_vivpr[is.na(pagopr_vivpr)] <- 0
pagopr_vivpr <- rowSums(pagopr_vivpr[, c(1:4)], na.rm = TRUE)
full_mean[, "p2_18_1"] <- pagopr_vivpr

## p2_61_1_1 pagos prestamos viv_principal
pagopr_otr_names <- c("p2_61_1_1", "p2_61_1_2", "p2_61_1_3")
pagopr_otr <- full_mean[, pagopr_otr_names]
pagopr_otr[is.na(pagopr_otr)] <- 0
pagopr_otr <- rowSums(pagopr_otr[, c(1:3)], na.rm = TRUE)
full_mean[, "p2_61_1_1"] <- pagopr_otr

## p2_43
f1 <- full_mean[, "p2_43_1"]
f2 <- full_mean[, "p2_43_2"]
f3 <- full_mean[, "p2_43_3"]
f4 <- full_mean[, "p2_43_4"]
f1[is.na(f1)] <- 0
f2[is.na(f2)] <- 0
f3[is.na(f3)] <- 0
f4[is.na(f4)] <- 0
f0 <- f1 + f2 + f3 + f4
full_mean[, "p2_43_1"] <- f0

fp_sum <- (fp_1 + fp_2 + fp_3 + fp_4 + fp_5) / 5
fp_sum_total <- fp_sum[, 1] + fp_sum[, 2] + fp_sum[, 3] + fp_sum[, 4] + fp_sum[, 5]

if (sel_year %in% c(2002, 2005, 2008, 2011, 2014, 2017, 2020)) {
    aa <- full_mean[, "p2_5"]
} else {
    aa <- full_mean[, "np2_5"]
}
full_mean <- data.frame(
    full_mean[, "p1_1_1"],
    full_mean[, "p1_5_1"],
    full_mean[, "p2_2"],
    full_mean[, "p2_1"],
    full_mean[, "p2_18_1"],
    aa,
    full_mean[, "p2_8"],
    full_mean[, "p2_9_1"],
    full_mean[, "p2_33"],
    full_mean[, "p2_35_1"],
    full_mean[, "p2_35_2"],
    full_mean[, "p2_35_3"],
    full_mean[, "p2_35a_1"],
    full_mean[, "p2_35a_2"],
    full_mean[, "p2_35a_3"],
    full_mean[, "p2_35a_3"],
    full_mean[, "p2_42_1"],
    full_mean[, "p2_43_1"],
    full_mean[, "p7_2"],
    full_mean[, "p7_4a"],
    full_mean[, "p7_4b"],
    full_mean[, "p2_23"],
    full_mean[, "p2_24"],
    full_mean[, "p2_25"],
    full_mean[, "p2_26"],
    full_mean[, "p2_39_1"],
    full_mean[, "p2_61_1_1"],
    full_mean[, "p4_16"],
    full_mean[, "p4_40"],
    full_mean[, "p4_36"],
    full_mean[, "p4_25"],
    s6_sum1,
    s6_sum2,
    s6_sum3,
    s6_sum4,
    s6_sum5,
    s6_sum6,
    s6_sum7,
    s6_sum8,
    s6b_sum,
    s6b_sum,
    p6_81,
    p6_37,
    s6b_dir,
    full_mean[, "p5_1"],
    empre,
    auton
)
colnames(full_mean) <- c(
    "sex",
    "educ",
    "p2_2",
    "p2_1",
    "pagopr_vivpr",
    "np2_5",
    "p2_8",
    "p2_9",
    "p2_33",
    "p2_35_1",
    "p2_35_2",
    "p2_35_3",
    "p2_35a_1",
    "p2_35a_2",
    "p2_35a_3",
    "p2_35a_4",
    "p2_42_1",
    "p2_43",
    "rents",
    "p7_4a",
    "p7_4b",
    "p2_23",
    "p2_24",
    "p2_25",
    "p2_26",
    "p2_39",
    "pagopr_otr",
    "p4_16", ## dividendos acciones cot
    "p4_40", ##  opciones y otros
    "p4_36", ## rendimientos renta fija
    "p4_25", ## dividendos acciones no cot
    "s6_sum",
    "s6_sum2",
    "s6_sum3",
    "s6_sum4",
    "s6_sum5",
    "s6_sum6",
    "s6_sum7",
    "s6_sum8",
    "s6b_sum",
    "s6b_sum_rent",
    "p6_81",
    "tipo_auton",
    "direc",
    "p5_1",
    "empre",
    "auton"
)

full_mean[full_mean$p2_35_1 %in% c(97), "p2_35_1"] <- 0
full_mean[full_mean$p2_35_1 %in% c(1.2, 1.4), "p2_35_1"] <- 1
full_mean[full_mean$p2_35_1 %in% c(1.6, 1.8), "p2_35_1"] <- 2

##### SELECTED VARIABLES IDS AND NAMES
main_selection <- sapply(paste0(path, selectors_eff_main), haven::read_dta)
main_selection %>% data.table()
###### DATA TRANSFORMATION
main_mean <-
    (main_selection[, paste0(path, "databol1.dta")] %>% sapply(as.numeric) + # nolint
        main_selection[, paste0(path, "databol2.dta")] %>% sapply(as.numeric) +
        main_selection[, paste0(path, "databol3.dta")] %>% sapply(as.numeric) +
        main_selection[, paste0(path, "databol4.dta")] %>% sapply(as.numeric) +
        main_selection[, paste0(path, "databol5.dta")] %>% sapply(as.numeric)) / 5
main_mean <- main_mean %>% as.data.table()
main_mean <- as.data.frame(main_mean)
if (sel_year == 2002 | sel_year == 2005) {
    main_mean$np2_32 <- main_mean$p2_32 - 1
}
if (sel_year == 2002 | sel_year == 2005) {
    main_mean[, "p5_1"] <- full_mean[, "p5_1"] - 1
}
main_mean$multipr <- main_mean$np2_32
main_mean$np2_32 <- NULL
if (sel_year == 2005) {
    colnames(main_mean)[colnames(main_mean) == "renthog04_€05"] <- "renthog"
}
if (sel_year == 2008) {
    colnames(main_mean)[colnames(main_mean) == "renthog07_€09"] <- "renthog"
}
if (sel_year == 2011) {
    colnames(main_mean)[colnames(main_mean) == "renthog10_€11"] <- "renthog"
}
if (sel_year == 2014) {
    colnames(main_mean)[colnames(main_mean) == "renthog13_eur14"] <- "renthog"
}
if (sel_year == 2017) {
    colnames(main_mean)[colnames(main_mean) == "renthog16_eur17"] <- "renthog"
}
if (sel_year == 2020) {
    colnames(main_mean)[colnames(main_mean) == "renthog19_eur20"] <- "renthog"
}

main_mean <- cbind(main_mean, full_mean)
survey_weights <- as.svydesign2(svydesign(
    ids = ~1,
    data = data.frame(main_mean),
    weights = ~ main_mean[, "facine3"]
))

quantile_cuts <- c(.5, .75, .8, .9, .95, .99, .999)
set <- data.frame(survey_weights$variables)
set[is.na(set)] <- 0
set[set$s6_sum %in% 1 & set$s6b_sum %in% 1, "s6b_sum"] <- 11
set$s6_owner <- set$p2_33
set[!(set$p2_2 %in% c(2, 3)), "p2_2"] <- 0
set[set$p2_2 > 0, "p2_2"] <- 1
set[set$s6_owner > 0, "s6_owner"] <- 1
set[set$s6_sum3 %in% 1, "nsitlabdom"] <- 1
set[set$s6_sum2 %in% 1, "nsitlabdom"] <- 2
set[set$s6b_sum %in% 1, "nsitlabdom"] <- 2
set[set$p6_37 %in% 1, "nsitlabdom"] <- 3
set[set$s6_sum5 %in% 1, "nsitlabdom"] <- 4
set[set$s6_sum6 %in% 1, "nsitlabdom"] <- 4
set[set$s6_sum7 %in% 1, "nsitlabdom"] <- 4
set[set$s6_sum8 %in% 1, "nsitlabdom"] <- 4
set[set$s6_sum4 %in% 1, "nsitlabdom"] <- 5
set[set$direc %in% 1, "nsitlabdom"] <- 6
set$deuda_vivienda <- set$dvivpral + set$deuoprop
actreales <- set["np2_5"] + set["otraspr"] - set["dvivpral"] - set["deuoprop"]
transf <- full_mean["p2_35_1"]
transf[transf == 0] <- 4
transf[transf == 1] <- 0
transf[transf == 2] <- 1
transf[transf == 3] <- 1
transf[transf > 1] <- NA
main_mean["p2_35_11"] <- transf
if (sel_year == 2017) {
    riquezafin <-
        set[, "p4_15"] +
        set[, "p4_24"] +
        set[, "p4_35"] +
        set[, "allf"]
    #+ set[, "p4_43"]
    #+  set[, "odeuhog"]
    #+ set[, "salcuentas"]
} else {
    riquezafin <-
        set[, "p4_15"] +
        set[, "p4_24"] +
        set[, "p4_35"] +
        set[, "allf"]
    #+  set[, "odeuhog"]
    #+ set[, "salcuentas"]
}
main_mean <- cbind(main_mean, set$deuda_vivienda, riquezafin, actreales)
survey_weights$variables[, "riquezafin"] <- riquezafin
survey_weights$variables[, "pagodeuda"] <- set$pagodeuda
survey_weights$variables[, "pagopr_vivpr"] <- set$pagopr_vivpr
survey_weights$variables[, "actreales"] <- actreales
survey_weights$variables[, "deuda_vivienda"] <- set$deuda_vivienda
survey_weights$variables[, "s6_owner"] <- set$s6_owner
survey_weights$variables[, "s6b_sum"] <- set$s6b_sum
survey_weights$variables[, "nsitlabdom"] <- set$nsitlabdom
survey_weights$variables[, "valor"] <- fp_sum_total
main_mean[, "s6_owner"] <- set$s6_owner
main_mean[, "s6b_sum"] <- set$s6b_sum
main_mean[, "nsitlabdom"] <- set$nsitlabdom
set[set$renthog > 100000 & set$s6b_sum_rent %in% 1, "s6b_sum_rent"] <- 11
survey_weights$variables[, "s6b_sum_rent"] <- set$s6b_sum_rent
survey_weights$variables[, "p7_4a"] <- survey_weights$variables[, "p7_4a"] - survey_weights$variables[, "p7_4b"]
