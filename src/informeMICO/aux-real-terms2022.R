# .Convertir los valores medios y medianos de renta a precios constantes de 2022
library(data.table)

# 1. Load Data
ipc <- fread("datasets/IPC/base2022.csv")
inequality_ten <- fread("out/informeMICO/1-inequality-tenancy.csv")
inequality_age <- fread("out/informeMICO/2-inequality-age.csv")
inequality_qui <- fread("out/informeMICO/4-inmo-inequality-quantiles.csv")

waves <- unique(inequality_age$year)

for (wave in waves) {
    inequality_age[year == wave, mean_renthog_IPC := mean_renthog * 100 / ipc[year == wave, ipc]]
    inequality_age[year == wave, median_renthog_IPC := median_renthog * 100 / ipc[year == wave, ipc]]
    inequality_age[year == wave, mean_riquezanet_IPC := mean_riquezanet * 100 / ipc[year == wave, ipc]]
    inequality_age[year == wave, median_riquezanet_IPC := median_riquezanet * 100 / ipc[year == wave, ipc]]
    inequality_ten[year == wave, mean_renthog_IPC := mean_renthog * 100 / ipc[year == wave, ipc]]
    inequality_ten[year == wave, median_renthog_IPC := median_renthog * 100 / ipc[year == wave, ipc]]
    inequality_ten[year == wave, mean_riquezanet_IPC := mean_riquezanet * 100 / ipc[year == wave, ipc]]
    inequality_ten[year == wave, median_riquezanet_IPC := median_riquezanet * 100 / ipc[year == wave, ipc]]
    inequality_qui[year == wave, median_riquezainmo_IPC := median_riquezainmo * 100 / ipc[year == wave, ipc]]
}

# export results in 2022 real euros to csv files
fwrite(inequality_ten, "out/informeMICO/IPC_2022/1-inequality-tenancy.csv")
fwrite(inequality_age, "out/informeMICO/IPC_2022/2-inequality-age.csv")
fwrite(inequality_qui, "out/informeMICO/IPC_2022/4-inmo-inequality-quantiles.csv")
