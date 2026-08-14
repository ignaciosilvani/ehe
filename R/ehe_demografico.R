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

  variables_requeridas <- c(
    "vi3",
    "vi4",
    "vi6",
    "ponduni"
  )

  faltantes <- setdiff(variables_requeridas, names(base))

  if (length(faltantes) > 0) {
    stop(
      "La base no contiene las variables requeridas: ",
      paste(faltantes, collapse = ", "),
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # Universo: personas de 14 anos y mas
  # ------------------------------------------------------------

  datos <- base |>
    dplyr::filter(vi6 > 13)

  # ------------------------------------------------------------
  # Variables derivadas
  # ------------------------------------------------------------

  datos <- datos |>
    dplyr::mutate(

      # Grupos de edad
      etarios = dplyr::case_when(
        vi6 >= 14 & vi6 <= 29 ~ 1,
        vi6 >= 30 & vi6 <= 64 ~ 2,
        vi6 >= 65 ~ 3,
        TRUE ~ NA_real_
      ),

      # Sexo + grupo de edad
      sexoedad = dplyr::case_when(
        vi4 == 1 & etarios == 1 ~ "Varon de 14 a 29 anos",
        vi4 == 1 & etarios == 2 ~ "Varon de 30 a 64 anos",
        vi4 == 1 & etarios == 3 ~ "Varon de 65 anos y mas",

        vi4 == 2 & etarios == 1 ~ "Mujer de 14 a 29 anos",
        vi4 == 2 & etarios == 2 ~ "Mujer de 30 a 64 anos",
        vi4 == 2 & etarios == 3 ~ "Mujer de 65 anos y mas",

        TRUE ~ NA_character_
      )
    )

  # ------------------------------------------------------------
  # Poblacion de 14 anos y mas
  # ------------------------------------------------------------

  total_p <- datos |>
    dplyr::summarise(
      CANTIDAD = round(sum(ponduni, na.rm = TRUE), 0)
    ) |>
    dplyr::mutate(
      `INDICADORES SOCIODEMOGRAFICOS` = "Poblacion de 14 anos y mas",
      `%` = 100
    ) |>
    dplyr::select(
      `INDICADORES SOCIODEMOGRAFICOS`,
      CANTIDAD,
      `%`
    )

  # ------------------------------------------------------------
  # Sexo
  # ------------------------------------------------------------

  sexo_p <- datos |>
    dplyr::group_by(vi4) |>
    dplyr::summarise(
      CANTIDAD = round(sum(ponduni, na.rm = TRUE), 0),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      `%` = round(CANTIDAD / sum(CANTIDAD) * 100, 1),
      `INDICADORES SOCIODEMOGRAFICOS` = dplyr::case_when(
        vi4 == 1 ~ "Varon",
        vi4 == 2 ~ "Mujer",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(
      `INDICADORES SOCIODEMOGRAFICOS`,
      CANTIDAD,
      `%`
    ) |>
    dplyr::arrange(`INDICADORES SOCIODEMOGRAFICOS`)

  # ------------------------------------------------------------
  # Grupos de edad
  # ------------------------------------------------------------

  etario_p <- datos |>
    dplyr::group_by(etarios) |>
    dplyr::summarise(
      CANTIDAD = round(sum(ponduni, na.rm = TRUE), 0),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      `%` = round(CANTIDAD / sum(CANTIDAD) * 100, 1),
      `INDICADORES SOCIODEMOGRAFICOS` = dplyr::case_when(
        etarios == 1 ~ "De 14 a 29 anos",
        etarios == 2 ~ "De 30 a 64 anos",
        etarios == 3 ~ "65 anos y mas",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(
      `INDICADORES SOCIODEMOGRAFICOS`,
      CANTIDAD,
      `%`
    )

  # ------------------------------------------------------------
  # Sexo y grupos de edad
  # ------------------------------------------------------------

  sexoedad_p <- datos |>
    dplyr::group_by(sexoedad) |>
    dplyr::summarise(
      CANTIDAD = round(sum(ponduni, na.rm = TRUE), 0),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      `%` = round(CANTIDAD / sum(CANTIDAD) * 100, 1),
      `INDICADORES SOCIODEMOGRAFICOS` = sexoedad
    ) |>
    dplyr::select(
      `INDICADORES SOCIODEMOGRAFICOS`,
      CANTIDAD,
      `%`
    )

  # ------------------------------------------------------------
  # Jefes de hogar
  # ------------------------------------------------------------

  jefe_p <- datos |>
    dplyr::filter(vi3 == 1) |>
    dplyr::summarise(
      CANTIDAD = round(sum(ponduni, na.rm = TRUE), 0)
    ) |>
    dplyr::mutate(
      `%` = round(CANTIDAD / sum(datos$ponduni, na.rm = TRUE) * 100, 1),
      `INDICADORES SOCIODEMOGRAFICOS` = "Jefes del hogar"
    ) |>
    dplyr::select(
      `INDICADORES SOCIODEMOGRAFICOS`,
      CANTIDAD,
      `%`
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
    "vi3",
    "vi4",
    "sexoedad",
    "CANTIDAD",
    "%",
    "INDICADORES SOCIODEMOGRAFICOS"
  )
)
