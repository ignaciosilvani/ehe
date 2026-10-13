#' Indicadores sociodemograficos de la EHE
#'
#' Calcula indicadores sociodemograficos para la poblacion de
#' 14 anos y mas de la Encuesta de Hogares y Empleo (EHE).
#'
#' El universo de analisis comprende a las personas de 14 anos
#' y mas. Los indicadores se calculan utilizando el factor de
#' expansion `ponduni`.
#'
#' La funcion construye internamente las variables derivadas
#' `etarios` y `sexoedad`.
#'
#' Es compatible con las distintas estructuras historicas de
#' la EHE:
#' - 2010 a 2013: `sexo`, `edad` y `parentesco`
#' - 2014 en adelante: `vi4`, `vi6` y `vi3`
#'
#' @param base Base de datos de personas de la EHE.
#'
#' @return Un tibble con los indicadores sociodemograficos.
#'
#' @examples
#' \dontrun{
#' ehe_demografico(base)
#' }
#'
#' @export
ehe_demografico <- function(base) {

  # ------------------------------------------------------------
  # Validaciones
  # ------------------------------------------------------------

  if (!is.data.frame(base)) {
    stop(
      "`base` debe ser un data.frame o tibble.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # Identificar estructura de la base
  # ------------------------------------------------------------

  if (all(c("sexo", "edad", "parentesco", "ponduni") %in% names(base))) {

    # Estructura 2010-2013
    sexo_var <- "sexo"
    edad_var <- "edad"
    parentesco_var <- "parentesco"

  } else if (all(c("vi3", "vi4", "vi6", "ponduni") %in% names(base))) {

    # Estructura 2014 en adelante
    sexo_var <- "vi4"
    edad_var <- "vi6"
    parentesco_var <- "vi3"

  } else {

    stop(
      paste0(
        "La base no contiene las variables requeridas para ninguna ",
        "de las estructuras historicas de la EHE. ",
        "Para 2010-2013 se requieren: sexo, edad, parentesco y ponduni. ",
        "Para 2014 en adelante se requieren: vi3, vi4, vi6 y ponduni."
      ),
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # Universo: personas de 14 anos y mas
  # ------------------------------------------------------------

  datos <- base |>
    dplyr::filter(
      .data[[edad_var]] > 13
    )

  # ------------------------------------------------------------
  # Variables derivadas
  # ------------------------------------------------------------

  datos <- datos |>
    dplyr::mutate(

      # Grupos de edad
      etarios = dplyr::case_when(
        .data[[edad_var]] >= 14 &
          .data[[edad_var]] <= 29 ~ 1,

        .data[[edad_var]] >= 30 &
          .data[[edad_var]] <= 64 ~ 2,

        .data[[edad_var]] >= 65 ~ 3,

        TRUE ~ NA_real_
      ),

      # Sexo + grupo de edad
      sexoedad = dplyr::case_when(

        .data[[sexo_var]] == 1 &
          etarios == 1 ~
          "Varones de 14 a 29 a\u00f1os",

        .data[[sexo_var]] == 1 &
          etarios == 2 ~
          "Varones de 30 a 64 a\u00f1os",

        .data[[sexo_var]] == 1 &
          etarios == 3 ~
          "Varones de 65 a\u00f1os y m\u00e1s",

        .data[[sexo_var]] == 2 &
          etarios == 1 ~
          "Mujeres de 14 a 29 a\u00f1os",

        .data[[sexo_var]] == 2 &
          etarios == 2 ~
          "Mujeres de 30 a 64 a\u00f1os",

        .data[[sexo_var]] == 2 &
          etarios == 3 ~
          "Mujeres de 65 a\u00f1os y m\u00e1s",

        TRUE ~ NA_character_
      )
    )

  # ------------------------------------------------------------
  # Poblacion de 14 anos y mas
  # ------------------------------------------------------------

  total_p <- datos |>
    dplyr::summarise(
      Cantidad = as.integer(round(sum(ponduni, na.rm = TRUE), 0))
    ) |>
    dplyr::mutate(
      Indicador = "Poblaci\u00f3n de 14 a\u00f1os y m\u00e1s",
      Porcentaje = 100,
      Cantidad = format(Cantidad, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    ) |>
    dplyr::select(
      Indicador,
      Cantidad,
      Porcentaje
    )

  # ------------------------------------------------------------
  # Sexo
  # ------------------------------------------------------------

  sexo_p <- datos |>
    dplyr::group_by(
      .data[[sexo_var]]
    ) |>
    dplyr::summarise(
      Cantidad = as.integer(round(sum(ponduni, na.rm = TRUE), 0)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
            Porcentaje = round(Cantidad / sum(Cantidad) * 100, 1),
      Indicador =
        dplyr::case_when(

          .data[[sexo_var]] == 1 ~
            "Varones",

          .data[[sexo_var]] == 2 ~
            "Mujeres",

          TRUE ~
            NA_character_
        ),
      Cantidad = format(Cantidad, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    ) |>
    dplyr::select(
      Indicador,
      Cantidad,
      Porcentaje
    ) |>
    dplyr::arrange(
      Indicador
    )

  # ------------------------------------------------------------
  # Grupos de edad
  # ------------------------------------------------------------

  etario_p <- datos |>
    dplyr::group_by(etarios) |>
    dplyr::summarise(
      Cantidad = as.integer(round(sum(ponduni, na.rm = TRUE), 0)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Porcentaje = round(Cantidad / sum(Cantidad) * 100, 1),
      Indicador =
        dplyr::case_when(

          etarios == 1 ~
            "De 14 a 29 a\u00f1os",

          etarios == 2 ~
            "De 30 a 64 a\u00f1os",

          etarios == 3 ~
            "65 a\u00f1os y m\u00e1s",

          TRUE ~
            NA_character_
        ),
      Cantidad = format(Cantidad, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    ) |>
    dplyr::select(
      Indicador,
      Cantidad,
      Porcentaje
    )

  # ------------------------------------------------------------
  # Sexo y grupos de edad
  # ------------------------------------------------------------

  sexoedad_p <- datos |>
    dplyr::group_by(sexoedad) |>
    dplyr::summarise(
      Cantidad = as.integer(round(sum(ponduni, na.rm = TRUE), 0)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Porcentaje = round(Cantidad / sum(Cantidad) * 100, 1),
      Indicador = sexoedad,
      Cantidad = format(Cantidad, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    ) |>
    dplyr::select(
      Indicador,
      Cantidad,
      Porcentaje
    )

  # ------------------------------------------------------------
  # Jefes de hogar
  # ------------------------------------------------------------

  jefe_p <- datos |>
    dplyr::filter(
      .data[[parentesco_var]] == 1
    ) |>
    dplyr::summarise(
      Cantidad = as.integer(round(sum(ponduni, na.rm = TRUE), 0))
    ) |>
    dplyr::mutate(
      Porcentaje = round(
        Cantidad /
          sum(datos$ponduni, na.rm = TRUE) *
          100,
        1
      ),
      Indicador =
        "Jefes del hogar",
      Cantidad = format(Cantidad, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    ) |>
    dplyr::select(
      Indicador,
      Cantidad,
      Porcentaje
    )

  # ------------------------------------------------------------
  # Tabla final
  # ------------------------------------------------------------

  dplyr::bind_rows(
    total_p,
    sexo_p,
    etario_p,
    sexoedad_p,
    jefe_p
  )
}


utils::globalVariables(
  c(
    "ponduni",
    "sexoedad",
    "etarios",
    "Cantidad",
    "Porcentaje",
    "Indicador"
  )
)
