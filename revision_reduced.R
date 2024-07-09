## Final Revision 072024 for article "Spain, society of owners" @Miguel-Duch

library(data.table)
library(magrittr)
library(survey)

years <- c(2002, 2020)
sel_year <- years[1]

df <- fread(paste0("datasets/",sel_year ,"-EFF.microdat.csv"))

df_sv <- svydesign(ids = ~1, weights = df$facine3, data = df)

# GINI: working-class wealth of finanacial assets (2002 vs 2020)

# percentage of young working-class households (under 35s) living in a ceded or borrowed house (2002 vs 2020)

# RENTAL INCOME: % de los ingresos anuales representan estas cifras para los rentistas de clase trabajadora

# Financial assets , the top 10% of working-class owners with the largest portfolios hold approximately 62% of this type of wealth that is in hands of the working class. (compare with Vu et al. – in those countries where financial crisis reduced participation, less so for wealthy) 

# On average, for the cohort nearing retirement (aged 55-65) during the 2002-2020 period, the value of these pension fund savings had a mean of 28,158€ and a median of 12,673€. 

#  mean and median value of the properties belonging to working class real-estate owners has declined in real terms since 2002. On the contrary, the value of this type of patrimony for the rest of social classes has increased.