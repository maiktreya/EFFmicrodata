# Preparación de Microdatos de la EFF: Aislamiento de Riqueza y Rentas de la Vivienda

Este script de R procesa los microdatos de la **Encuesta Financiera de Familias (EFF)** del Banco de España. El objetivo principal es construir variables sintéticas armonizadas que aíslen estrictamente la **riqueza inmobiliaria residencial** y los **ingresos por alquiler de viviendas**, descartando otros activos inmobiliarios no habitacionales (como plazas de garaje independientes, trasteros, locales comerciales, oficinas o fincas rústicas).

## Objetivos del Filtro

La EFF agrupa en la misma sección todos los activos inmobiliarios anexos u otras propiedades. Para evitar una sobreestimación del patrimonio residencial real de los hogares, este script depura los datos aplicando una restricción basada en el tipo de propiedad (`p2_35a_X == 1`), garantizando que:

1. **Riqueza de la Vivienda:** Solo sume el valor de mercado de los inmuebles catalogados como "Vivienda".
2. **Rentas por Alquiler:** Solo contabilice los flujos monetarios anualizados provenientes del arrendamiento de inmuebles residenciales.

---

## Estructura del Script y Operaciones

El flujo de ejecución está dividido en cuatro fases secuenciales optimizadas con el paquete `data.table`:

### 1. Limpieza Universal y Coerción de Tipos

- Se extrae el valor de la vivienda principal (`p2_5`).
- Se identifican los bloques de segundas propiedades (del 1 al 4).
- Se transforman todas las variables críticas de la encuesta a tipo numérico y se reemplazan de forma segura los valores ausentes (`NA`) por `0`. Esto previene que las operaciones lógicas posteriores o las sumas agregadas invaliden el registro del hogar.

### 2. Estimación de Rentas por Alquiler Residencial

- Se evalúan las propiedades secundarias individuales (1 a 3) y el bloque agregado (4 o más propiedades).
- Se aísla el alquiler mensual (`p2_43_X`) **únicamente si** el tipo de propiedad es residencial (`p2_35a_X == 1`) y el ingreso declarado es estrictamente positivo (`> 0`).
- **Variables Sintéticas Generadas:**
  - `renta_alq`: Total de ingresos anualizados por alquiler residencial del hogar ($\text{Suma de alquileres mensuales} \times 12$).
  - `n_props_alq`: Número total de viviendas secundarias activamente alquiladas por el hogar.

### 3. Cálculo de la Riqueza Inmobiliaria Residencial Bruta

- Se filtra el valor actual de mercado de las propiedades secundarias (`p2_39_X`) utilizando el mismo criterio de exclusión residencial (`p2_35a_X == 1`).
- **Variable Sintética Generada:**
  - `riquezainmo`: Sumatorio del valor de la vivienda principal (`v_principal`) más el valor de mercado filtrado de las viviendas secundarias aptas.

### 4. Depuración de Memoria y Exportación

- Se eliminan todas las variables auxiliares e intermedias creadas para el cálculo (`propX_rent`, `propX_val`) con el fin de mantener el dataset final limpio y eficiente para posteriores análisis econométricos.
- El resultado se exporta comprimido en `datasets/full_eff_refined.gz`.

---

## Limitación Metodológica Conocida (Bloque 4+)

Debido al diseño del cuestionario de la EFF, a partir de la cuarta propiedad secundaria (`_4`) los activos restantes se agrupan en un único bloque colectivo. El script evalúa el tipo de inmueble basándose en la propiedad principal de dicho bloque. Si un hogar posee una cuarta vivienda y un garaje independiente en este mismo bloque, el script capturará el valor conjunto si el activo principal del bloque es residencial. Este es el estándar econométrico óptimo dadas las restricciones del microdato anonimizado.
