# EFF

## CLASS AND OTHER CATEGORICAL VARIABLES CONSTRUCTED

---

> 1. INCORPORAR A AUTÓNOMOS A LA CATEGORÍA CLASE CONSISTENTEMENTE
>
> 2. EVOLUCIÓN CONTRADICTORIA RENTA DEL ALQUILER EFF ECV
> EFF -> P.7_2:  ¿CUÁNTO RECIBIERON POR ESTE CONCEPTO DURANTE TODO EL AÑO 2016?
> ECV -> HY040G/N:  Renta bruta procedente del alquiler de una propiedad o terreno en el año anterior al de encuesta
>
> 3. INCOROPORAR P2_39
>
> 4. INCORPORAR P2_43
>
> 5. DEFINCIÓN FORMAL DE LOS FILTROS (CLASE, TIPO DE PROPIEDAD, ORIGEN DE PROPIEDAD)

---

### SET_VIA_PROPERTY

-> QUESTION:  p2_35_1,
-> ¿CÓMO OBTUVIERON LA PROPIEDAD? - en nuestro caso, tomamos en consideración solo la principal
-> LEVELS: c(1, 2, 3),
-> LABELS:     labels = c("compra", "herencia", "regalo-donacion")

---

### SET_USE_PROPERTY

-> QUESTION: factor(set$p2_42_1) -qualitative-
-> TEXT: ¿Cuál es el uso de la propiedad? - en nuestro caso, tomamos en consideración solo la principal
-> LEVELS: c(1, 2, 3, 4, 5, 6, 7),
-> LABELS: c("agricola", "vacaciones", "uso profesional", "cesion", "alquiler", "desocupada", "futura")

---

### SET_CLASS

Es la más compleja metodológicamente. Tenemos 6 categorias

 1.TRABAJADOR
 2.CAPITALISTA
 3.JUBILADO
 4.INACTIVO
 5.AUTÓNOMO
 6.DIRECTIVO

pero  partimos de la categoria NSITLABDOM definida por simplicidad por el INE que solo nos da 4.

-> QUESTION 1: NSITLABDOM -> SITUACION LABORAL DEL CABEZA DE FAMILIA;
*LABOUR MARKET SITUATION OF HOUSEHOLD HEAD;
*nsitlabdom: toma valores 1 a 4; empleado por cuenta ajena, empleado por cuenta propia, jubilado, otro tipo de inactivo o parado;

Posteriormente ampliamos  con la información de las preguntas  6.1 y 6.3

p6_30-4_105, p6_37 -> Autónomo consistente
6.30-4 -> ¿Más de un trabajo por cuenta propia?¿Ocupación principal?
-> QUESTION 2: 6.37 -> Profesional liberal, propietario de negocio único, autónomo

6.3-> QUESTION 3:  -> Tipo de ocupacion
1.Dirección de empresas y de las Administraciones Pública
2.Técnicos y profesionales científicos e intelectuales

-> QUESTION 4: 6.1. -> SI ES LA PERSONA DE REFERENCIA, ¿CUÁL ES SU SITUACIÓN LABORAL ACTUAL?
1.Empleado por cuenta ajena
2.Trabajador por cuenta propia/ gestiona un negocio del hogar 40
3.Desempleado
4.Jubilado o jubilación anticipada o prejubilado
5.Incapacitado permanente para trabajar
6.Estudiante, escolar o en formación
7.Dedicado a labores del hogar
8.Otra clase de inactividad económica

Para poder definir por lógica de conjuntos el resto de categorias pendientes o ajustar cualquier que se hubiera "colado"

```r
set[ jubilado %in% 1, "nsitlabdom"] <- inactivo
set[ incapacitado %in% 1, "nsitlabdom"] <- inactivo
set[ estudiante %in% 1, "nsitlabdom"] <- inactivo
set[ hogar %in% 1, "nsitlabdom"] <- inactivo
set[ desempleado %in% 1, "nsitlabdom"] <- trabajador
set[ empresario %in% 1, "nsitlabdom"] <- capitalista
set[ empresario_aut %in% 1, "nsitlabdom"] <- autonomo
set[ trabajador_direccon %in% 1, "nsitlabdom"] <- directivo
```

- QUE RESULTA EN :
    1.TRABAJADOR: trabajador + desempleado
    2.CAPITALISTA self-employed + employes
    3.JUBILADO retired
    4.INACTIVO inactivo (disabled, student, unactive)
    5.AUTÓNOMO liberal professional + self.employed
    6.DIRECTIVO worker within so called managerial positions

---

### DESCOMPOSICIÓN RIQUEZ FINANCIERA

riquezafin la definimos como la suma de las siguientes variables:

 "p4_15" -> VALOR DE ACCIONES COTIZADAS
 "p4_24"-> VALOR CARTERA ACCIONES NO COTIZADAS Y PARTICIPACIONES
 "p4_35" -> VALOR CARTERA RENTA FIJA
 "allf"-> VALOR CARTERA FONDOS DE INVERSIÓN
 "salcuentas" -> SALDO CUENTAS DE AHORRO SIN PODER REALZIAR PAGOWA
 "odeuhog" -> OTROS ACTIVOS FINANCIEROS

 Todas ellas están ya incorporadas con los estadísticos habituales.
