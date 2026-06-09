library(data.table)

rm(list = ls())

# 1. Cargamos y filtramos las filas base de los cuantiles que nos interesan
dt <- fread("out/informeMICO/IPC_2022/4-inmo-inequality-quantiles.csv")
dt_base <- dt[riquezainmo %in% c("riquezainmo.0.9", "riquezainmo.0.95", "riquezainmo.0.99")]

# 2. Construimos la tabla de los cuantiles "Superiores" (Top)
# Tomamos el valor correspondiente a la antigua columna 'share_riquezainmo1' (100 - share base)
dt_top <- dt_base[, .(year, riquezainmo, share = 100 - share_riquezainmo)]

# Actualizamos las etiquetas para los cuantiles superiores
dt_top[riquezainmo == "riquezainmo.0.9",  riquezainmo := "3. 10% superior"]
dt_top[riquezainmo == "riquezainmo.0.95", riquezainmo := "2. 5% superior"]
dt_top[riquezainmo == "riquezainmo.0.99", riquezainmo := "1. 1% superior"]

# 3. Construimos la tabla del cuantil "Inferior" (90% inferior)
# Tomamos el valor de la antigua columna 'share_riquezainmo' para el corte del 0.9
dt_bottom <- dt_base[riquezainmo == "riquezainmo.0.9", 
                     .(year, riquezainmo = "4. 90% inferior", share = share_riquezainmo)]

# 4. Unimos ambas tablas (superiores e inferior) en un formato de columna única
dt_final <- rbind(dt_top, dt_bottom)

# 5. Ordenamos por año y etiqueta para organizar la salida de datos
setorder(dt_final, year, riquezainmo)

# 6. Importamos los datos de riqueza absoluta y el deflactor del IPC
total_riquezainmo <- fread("out/informeMICO/3-inequality-ratio50_1.csv")[,.(year,total_wealth_absolute)]
ipc <- fread("datasets/IPC/base2022.csv")[year %in% total_riquezainmo$year]

# 7. Calculamos la riqueza total absoluta deflactada (IPC) usando un "update join"
total_riquezainmo[ipc, total_riquezainmo_IPC := total_wealth_absolute * 100 / i.ipc, on = "year"]

# 8. Añadimos el valor deflactado a la tabla final y calculamos la riqueza por grupo
# Utilizamos otro "update join" para cruzar dt_final con total_riquezainmo usando "year".
# El prefijo i. nos asegura tomar el total correcto para multiplicarlo por la cuota (share).
dt_final[total_riquezainmo, 
         riquezainmo_total_IPC := i.total_riquezainmo_IPC * (share / 100), 
         on = "year"]

print(dt_final)