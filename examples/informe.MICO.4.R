# analyze_combined_stats.R
library(data.table)
library(magrittr)
library(survey)
library(mitools)

# 1. Load Data
IPC <- fread("datasets/IPC/base2022.csv")
inequality_ten <- fread("out/informeMICO/1-inequality-tenancy.csv")
inequality_age <- fread("out/informeMICO/2-inequality-age.csv")

waves <- unique(inequality_age$year)

for (wave in waves){
inequality_age[year == wave, mean_renthog_nor := mean_renthog *100 /IPC[Year == wave, IPC]  ]
inequality_age[year == wave, median_renthog_nor := median_renthog *100 /IPC[  Year == wave, IPC] ]
inequality_age[year == wave, mean_riquezanet_nor := mean_riquezanet *100 /IPC[  Year == wave, IPC] ]
inequality_age[year == wave, median_riquezanet_nor := median_riquezanet *100 /IPC[  Year == wave, IPC] ]
inequality_ten[year == wave, mean_renthog_nor := mean_renthog *100 /IPC[Year == wave, IPC]  ]
inequality_ten[year == wave, median_renthog_nor := median_renthog *100 /IPC[  Year == wave, IPC] ]
inequality_ten[year == wave, mean_riquezanet_nor := mean_riquezanet *100 /IPC[  Year == wave, IPC] ]
inequality_ten[year == wave, median_riquezanet_nor := median_riquezanet *100 /IPC[  Year == wave, IPC] ]

}