
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ehe <img src='man/figures/logo.png' align="right" height="200" style="float:right; height:200px;" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/ignaciosilvani/ehe/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ignaciosilvani/ehe/actions)

<!-- badges: end -->

# Herramientas para el procesamiento de la Encuesta de Hogar y Empleo

## Descripción

Si querés procesar datos de la **Encuesta de Hogar y Empleo (EHE)** de
la Provincia de Buenos Aires mediante el lenguaje de programación
[R](https://www.r-project.org/), el paquete `ehe` tiene por objeto
facilitar el acceso, la descarga y el procesamiento de sus microdatos.

La EHE constituye una fuente de información sobre las características
demográficas, sociales y laborales de la población de la Provincia de
Buenos Aires. El paquete `ehe` busca facilitar el acceso a sus bases de
usuarios y proporcionar herramientas para su procesamiento y análisis.

El paquete permite consultar el catálogo oficial de datos abiertos,
identificar las bases disponibles, descargar los microdatos, consultar
los diccionarios de variables, incorporar etiquetas y realizar tabulados
e indicadores del mercado laboral.

El flujo general de trabajo es:

**consultar → descargar → conocer las variables → etiquetar → analizar**

## Funciones principales

Algunas de las principales funciones del paquete son:

-   **`available_ehe()`**: Consulta las bases de EHE disponibles en el
    catálogo de datos abiertos y permite identificar las encuestas
    disponibles según municipio y año.

-   **`get_ehe()`**: Descarga y lee los microdatos de la EHE disponibles
    en el catálogo de datos abiertos.

-   **`ehe_metadata()`**: Permite consultar los metadatos asociados a
    las bases de EHE.

-   **`ehe_dictionary()`**: Permite consultar el diccionario de
    variables correspondiente a una encuesta, año y tipo de base
    determinados.

-   **`ehe_labels()`**: Incorpora etiquetas de variables y valores a una
    base EHE utilizando el diccionario correspondiente.

-   **`ehe_tabulate()`**: Permite realizar tabulados ponderados uni y
    bivariados utilizando el factor de expansión correspondiente.

-   **`ehe_laboral()`**: Calcula tasas e indicadores básicos del mercado
    laboral, incluyendo tasas de actividad, empleo, desocupación y
    subocupación.

## Fuente de los datos

El paquete utiliza el catálogo oficial de datos abiertos de la
**Dirección Provincial de Estadística de la Provincia de Buenos Aires**.

Las bases de usuarios se encuentran disponibles en:

<https://datos.estadistica.ec.gba.gov.ar/dataset?groups=bases-usuarias>

El paquete consulta el catálogo mediante su API para identificar los
recursos disponibles y acceder a los microdatos y sus metadatos.

## Instalación

La versión en desarrollo del paquete puede instalarse directamente desde
GitHub.

Si no tenés instalado `remotes`, podés instalarlo mediante:

``` r
install.packages("remotes")
```

Luego instalá `ehe` con:

``` r
remotes::install_github("ignaciosilvani/ehe")
```

Una vez instalado, cargá el paquete:

``` r
library(ehe)
```

## Modo de uso

### Consultar las bases disponibles

La función `available_ehe()` permite consultar las encuestas disponibles
en el catálogo.

``` r
available_ehe()
```

También se puede restringir la búsqueda a un municipio y año:

``` r
available_ehe(
  localidad = "Alberti",
  anio = 2024
)
```

### Descargar una base

Una vez identificada la encuesta, `get_ehe()` permite descargar y leer
los microdatos.

Por ejemplo:

``` r
base <- get_ehe(
  encuesta = "EHE-M",
  anio = 2024,
  tipo = "individual",
  localidad = "Alberti"
)
```

El resultado es un `tibble` que puede utilizarse directamente con las
herramientas de R.

### Consultar el diccionario

El diccionario de variables se obtiene a partir de la identificación de
la encuesta, el año y el tipo de base.

Por ejemplo:

``` r
dictionary <- ehe_dictionary(
  encuesta = "EHE-M",
  anio = 2024,
  tipo = "individual"
)
```

También es posible consultar simultáneamente los diccionarios
correspondientes a las bases individual y de hogar:

``` r
dictionary <- ehe_dictionary(
  encuesta = "EHE-M",
  anio = 2024,
  tipo = c("individual", "hogar")
)
```

El diccionario permite conocer las variables disponibles, sus
descripciones, etiquetas y demás información necesaria para interpretar
correctamente los microdatos.

### Etiquetar las variables

`ehe_labels()` permite incorporar a una base EHE las etiquetas de las
variables y de sus valores utilizando el diccionario correspondiente.

Una opción es obtener primero el diccionario y luego utilizarlo para
etiquetar la base:

``` r
dictionary <- ehe_dictionary(
  encuesta = "EHE-M",
  anio = 2024,
  tipo = "individual"
)

base <- ehe_labels(
  data = base,
  dictionary = dictionary
)
```

También es posible obtener automáticamente el diccionario desde
`ehe_labels()`:

``` r
base <- ehe_labels(
  data = base,
  encuesta = "EHE-M",
  anio = 2024,
  tipo = "individual"
)
```

La función agrega etiquetas descriptivas a las variables y etiquetas a
los valores categóricos cuando están disponibles en el diccionario.

Las variables numéricas con categorías definidas en el diccionario se
convierten en variables `labelled` de `haven`, conservando sus valores
originales y agregando la información de las etiquetas.

### Tabulados ponderados

El paquete incluye `ehe_tabulate()` para realizar tabulados utilizando
el factor de expansión de la encuesta.

Por ejemplo:

``` r
ehe_tabulate(
  base = base,
  x = "condact",
  weights = "ponduni"
)
```

También permite realizar tabulados bivariados:

``` r
ehe_tabulate(
  base = base,
  x = "vi4",
  y = "condact",
  weights = "ponduni"
)
```

La función permite además incorporar totales y calcular porcentajes
según las opciones disponibles.

### Indicadores del mercado laboral

La función `ehe_laboral()` permite calcular indicadores laborales a
partir de una base individual.

Por ejemplo:

``` r
ehe_laboral(
  base = base,
  weights = "ponduni"
)
```

La función calcula:

-   tasa de actividad;
-   tasa de empleo;
-   tasa de desocupación;
-   tasa de subocupación.

Los indicadores se presentan para la población de 14 años y más y
permiten desagregaciones por sexo, grupo de edad, sexo y grupo de edad y
jefatura del hogar.

## Ejemplo de flujo completo

Un flujo de trabajo habitual puede ser:

``` r
library(ehe)

# Consultar las bases disponibles
available_ehe(
  localidad = "Alberti",
  anio = 2024
)

# Descargar los microdatos
base <- get_ehe(
  encuesta = "EHE-M",
  anio = 2024,
  tipo = "individual",
  localidad = "Alberti"
)

# Consultar el diccionario
dictionary <- ehe_dictionary(
  encuesta = "EHE-M",
  anio = 2024,
  tipo = "individual"
)

# Etiquetar las variables
base <- ehe_labels(
  data = base,
  dictionary = dictionary
)

# Realizar un tabulado ponderado
ehe_tabulate(
  base = base,
  x = "condact",
  weights = "ponduni"
)

# Calcular indicadores laborales
ehe_laboral(
  base = base,
  weights = "ponduni"
)
```

## Cómo citar este paquete

Si utilizás `ehe` para obtener, procesar o analizar datos de la Encuesta
de Hogar y Empleo, se recomienda citar el paquete junto con la fuente
original de los datos.

La referencia bibliográfica del paquete se incorporará una vez definida
la publicación o versión correspondiente.

La fuente de los datos es la **Dirección Provincial de Estadística de la
Provincia de Buenos Aires**, responsable de la Encuesta de Hogar y
Empleo y de la publicación de sus bases usuarias.

## Aportes de la comunidad

El paquete `ehe` busca facilitar el acceso y procesamiento de los
microdatos de la Encuesta de Hogar y Empleo en R.

Los aportes, sugerencias, reportes de errores y propuestas de nuevas
funcionalidades son bienvenidos.

Si encontrás un problema o querés proponer una mejora, podés abrir un
*issue* en el repositorio del proyecto:

<https://github.com/ignaciosilvani/ehe>
