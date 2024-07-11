## Final Revision 072024 for article "Spain, society of owners" @Miguel-Duch
library(data.table)
library(magrittr)
library(survey)

years <- c(2008, 2011, 2014, 2017, 2020) # You can expand this later if needed
obj_list <- list()
full_mean <- list()
ecv_mean <- list()

# for incorporating new variables from
for (i in years) {
    year_data <- list()

    for (j in 1:5) {
        df <- fread(paste0("EFFSpain/full/", i, "/otras_secciones_", i, "_imp", j, ".csv"))
        ec <- fread(paste0("EFF-ECVSpain/ECV/output/filtered_data", i, ".csv"))
        selection <- colnames(df)[colnames(df) %like% "p2_9b_"]
        year_data[[j]] <- df[, ..selection]
        ecv_mean[[j]] <- data.table(ec)
    }

    obj_list[[as.character(i)]] <- year_data
}

# to get the previously created datasets
for (p in 1:5) full_mean[[p]] <- fread(paste0("EFFSpain/datasets/", years[p], "-EFF.microdat.csv"))

#------------------CHECK LENGTH OF OBJECTS-----------------------------------------------------------#

for (x in years) {
    obj_list[[as.character(x)]][[1]][, 1] %>%
        nrow()
    # full_mean[[z]][nsitlabdom == 1, 1]
}
value <- c()
for (z in seq_along(years)) {
    value[z] <- nrow(full_mean[[z]][nsitlabdom == 1, 1]) / nrow(full_mean[[z]][, 1])
    print(value[z])
}

value2 <- c()
for (z in seq_along(years)) {
    value2[z] <- nrow(full_mean[[z]][bage == 1 & nsitlabdom == 1, 1]) / nrow(full_mean[[z]][, 1])
    print(value2[z])
}

#-----------------------------------------------------------------------------#
full_mean[[1]][, homeowner := as.factor(np2_1)]
nrow(full_mean[[1]][homeowner == 1, ]) / nrow(full_mean[[1]])

#-----------------------------------------------------------------------------#

# Working class subsample size

full_mean[[1]]$nsitlabdom %>% unique()

# Young people subsample size

# Young people hoeownership


# Share of income of working class rents income

# HY090G1, PL031
ec_sv <- list()
df_sv <- list()

for (p in 1:5) {
    transf <- ecv_mean[[p]] %>% data.frame()
    ec_sv[[p]] <- svydesign(
        ids = ~1,
        data = transf,
        weights = ~ transf$PB040
    )
}





# P29b % population
nueva <- list()
for (i in 1:5){
for(j in 1:5){
obj_list[[i]][[j]][is.na(obj_list[[i]][[j]])] <- 0
obj_list[[i]][[j]][,sum:= as.numeric(p2_9b_1) + as.numeric(p2_9b_2) + as.numeric(p2_9b_3) + as.numeric(p2_9b_4)]
}
nueva[[i]] <-  obj_list[[i]][[1]]$sum + obj_list[[i]][[1]]$sum + obj_list[[i]][[1]]$sum + obj_list[[i]][[1]]$sum + obj_list[[i]][[1]]$sum
part <- data.table(nueva[[i]])
full_mean[[i]][, p2_9b :=part]
}


for (p in 1:5) {
    transf <- full_mean[[p]] %>% data.frame()
    df_sv[[p]] <- svydesign(
        ids = ~1,
        data = transf,
        weights = ~ transf$facine3
    )
}



full_mean[[1]][, homeowner := as.factor(np2_1)]
nrow(full_mean[[1]][homeowner == 1, ]) / nrow(full_mean[[1]])



full_mean[[1]] %>% nrow()
nueva[1] %>% length()