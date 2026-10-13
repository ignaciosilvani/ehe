#' Tasas e indicadores del mercado laboral de EHE
#'
#' Calcula tasas de actividad, empleo, desocupacion y subocupacion
#' para la poblacion de 14 anos y mas.
#'
#' Compatible con las distintas estructuras historicas de la EHE:
#' - 2010 a 2013
#' - 2014
#' - 2015 en adelante
#'
#' @param base Base de datos EHE.
#' @param weights Variable de ponderacion. Por defecto, `"ponduni"`.
#'
#' @return Un tibble con las columnas:
#' `indicador`, `desagregacion`, `categoria` y `porcentaje`.
#'
#' @examples
#' \dontrun{
#' base <- get_ehe(
#'   encuesta = "EHE-M",
#'   anio = 2024,
#'   tipo = "individual",
#'   localidad = "Alberti"
#' )
#'
#' resultado <- ehe_laboral(base)
#'
#' resultado
#' }
#'
#' @export

ehe_laboral <- function(
  base,
  weights = "ponduni"
) {

  # ==================================================
  # VALIDACIONES
  # ==================================================

  if (!is.data.frame(base)) {
    stop(
      "`base` debe ser un data.frame o tibble.",
      call. = FALSE
    )
  }

  if (!is.character(weights) || length(weights) != 1) {
    stop(
      "`weights` debe ser el nombre de una variable.",
      call. = FALSE
    )
  }


  # ==================================================
  # IDENTIFICAR ESTRUCTURA DE LA BASE
  # ==================================================

  estructura <- dplyr::case_when(

    "edad" %in% names(base) &&
      "sexo" %in% names(base) &&
      "p1" %in% names(base) &&
      "p12_1" %in% names(base) &&
      "p12_2" %in% names(base) ~
      "2010_2013",

    "i1a" %in% names(base) &&
      "i1c" %in% names(base) &&
      "ii2h" %in% names(base) &&
      "iii3g" %in% names(base) ~
      "2014",

    TRUE ~
      "actual"
  )


  # ==================================================
  # BASES 2010-2013
  # ==================================================

  if (estructura == "2010_2013") {

    variables_requeridas <- c(
      "edad",
      "sexo",
      "p1",
      "p2",
      "p3",
      "p4",
      "p5",
      "p7",
      "p8",
      "p9",
      "p10",
      "p12_1",
      "p12_2",
      "p13",
      weights
    )

    faltantes <- setdiff(
      variables_requeridas,
      names(base)
    )

    if (length(faltantes) > 0) {
      stop(
        paste0(
          "Para las bases 2010-2013 faltan las siguientes variables: ",
          paste(faltantes, collapse = ", ")
        ),
        call. = FALSE
      )
    }


    # ----------------------------------------------
    # Edad y sexo
    # ----------------------------------------------

    base <- base |>
      dplyr::mutate(
        vi6 = edad,
        vi4 = sexo
      )


    # ----------------------------------------------
    # Parentesco
    # ----------------------------------------------

    if (!"vi3" %in% names(base)) {

      if ("parentesco" %in% names(base)) {

        base <- base |>
          dplyr::mutate(
            vi3 = parentesco
          )

      } else {

        stop(
          "Las bases 2010-2013 necesitan `parentesco` para calcular los indicadores de jefatura.",
          call. = FALSE
        )
      }
    }


    # ----------------------------------------------
    # Condicion de actividad
    # ----------------------------------------------

    base <- base |>
      dplyr::mutate(

        condact = dplyr::case_when(

          (
            (p1 == 1) |

              ((p1 == 2) &
                 (p2 == 1) &
                 (p3 == 1)) |

              ((p1 == 2) &
                 (p2 == 1) &
                 (p3 == 2)) |

              ((p1 == 2) &
                 (p4 == 1) &
                 (p5 == 2) &
                 (p7 == 1)) |

              ((p1 == 2) &
                 (p4 == 1) &
                 (p5 == 4)) |

              ((p1 == 2) &
                 (p4 == 1) &
                 (p5 == 5)) |

              ((p1 == 2) &
                 (p4 == 1) &
                 (p5 == 6)) |

              ((p1 == 2) &
                 (p4 == 1) &
                 (p5 == 7))
          ) ~ 1,

          (
            (p8 == 1) |

              ((p8 == 2) &
                 (p9 == 3)) |

              ((p8 == 2) &
                 (p9 == 4))
          ) ~ 2,

          (
            p10 == 1 |
              p10 == 2 |
              p10 == 3 |
              p10 == 4 |
              p10 == 5 |
              p10 == 6 |
              p10 == 99
          ) ~ 3,

          p1 == 0 ~ 9,

          TRUE ~ 9
        )
      )


    # ----------------------------------------------
    # Horas trabajadas
    # ----------------------------------------------

    base <- base |>
      dplyr::mutate(

        menos9 = dplyr::case_when(

          p12_1 == -9 |
            p12_2 == -9 ~ -9,

          TRUE ~ NA_real_
        ),

        horas = dplyr::case_when(

          p12_1 != -9 &
            p12_2 != -9 ~
            p12_1 + p12_2,

          p12_1 == -9 &
            p12_2 != -9 ~
            p12_2,

          p12_1 != -9 &
            p12_2 == -9 ~
            p12_1,

          TRUE ~ 0
        ),

        horas_recod = dplyr::case_when(

          horas < 35 ~ 1,

          horas > 34 &
            horas < 47 ~ 2,

          horas > 46 ~ 3,

          menos9 == -9 ~ 99,

          TRUE ~ NA_real_
        ),

        iii3g = dplyr::case_when(

          p13 == 1 ~ 1,

          !is.na(p13) ~ 0,

          TRUE ~ NA_real_
        )
      )
  }


  # ==================================================
  # BASE 2014
  # ==================================================

  if (estructura == "2014") {

    variables_requeridas <- c(
      "vi3",
      "vi4",
      "vi6",
      "i1a",
      "i1b",
      "i1c",
      "i1d",
      "i1e",
      "i1f",
      "i1g",
      "i1h",
      "ii2a",
      "ii2b",
      "ii2d",
      "ii2e",
      "ii2f",
      "ii2g",
      "ii2h",
      "ii2i",
      "iii3g",
      "iii3etotal_hs",
      "iii3ftotal_hs",
      weights
    )

    faltantes <- setdiff(
      variables_requeridas,
      names(base)
    )

    if (length(faltantes) > 0) {
      stop(
        paste0(
          "Para la base 2014 faltan las siguientes variables: ",
          paste(faltantes, collapse = ", ")
        ),
        call. = FALSE
      )
    }


    # ----------------------------------------------
    # Condicion de actividad 2014
    # ----------------------------------------------

    base <- base |>
      dplyr::mutate(

        condact = dplyr::case_when(

          (
            (i1a == 1 & i1c == 1) |

              (i1a == 1 &
                 i1c == 2 &
                 (i1d == 1 | i1d == 2)) |

              (i1a == 2 &
                 i1b == 1 &
                 i1c == 2 &
                 (i1d == 1 | i1d == 2)) |

              (i1a == 2 &
                 i1b == 1 &
                 i1c == 1) |

              (i1a == 2 &
                 i1b == 2 &
                 i1e == 5 &
                 (i1f == 1 | i1f == 2 | i1f == 3)) |

              (i1a == 2 &
                 i1b == 2 &
                 i1e == 5 &
                 i1f == 4 &
                 i1g == 1) |

              (i1a == 2 &
                 i1b == 2 &
                 i1e == 5 &
                 i1f == 5 &
                 i1h == 1)
          ) ~ 1,

          (
            (i1a == 2) &
              (i1b == 2) &
              (i1e == 1 | i1e == 2) &
              (ii2f == 1) &
              (ii2g == 1)

            |

              (i1a == 2) &
              (i1b == 2) &
              (i1e == 3 | i1e == 4) &
              (ii2a == 1 | ii2a == 2) &
              (
                ii2b == 1 |
                  ii2b == 2 & ii2d == 1 |
                  ii2b == 2 & ii2d == 2 &
                  (ii2e == 1 | ii2e == 2)
              )

            |

              (i1a == 2) &
              (i1b == 2) &
              (i1e == 5) &
              (i1f == 4) &
              (i1g == 2 | i1g == 99) &
              (ii2a == 1 | ii2a == 2) &
              (
                ii2b == 1 |
                  ii2b == 2 & ii2d == 1 |
                  ii2b == 2 & ii2d == 2 &
                  (ii2e == 1 | ii2e == 2)
              )

            |

              (i1a == 2) &
              (i1b == 2) &
              (i1e == 5) &
              (i1f == 5) &
              (i1h == 2 | i1h == 99) &
              (ii2a == 1 | ii2a == 2) &
              (
                ii2b == 1 |
                  ii2b == 2 & ii2d == 1 |
                  ii2b == 2 & ii2d == 2 &
                  (ii2e == 1 | ii2e == 2)
              )
          ) ~ 2,

          (
            ii2h == 1 |
              ii2h == 2 |
              ii2i == 1 |
              ii2i == 2
          ) ~ 3,

          TRUE ~ 9
        )
      )


    # ----------------------------------------------
    # Horas trabajadas 2014
    # ----------------------------------------------

    base <- base |>
      dplyr::mutate(

        menos9 = dplyr::case_when(

          iii3etotal_hs == -9 |
            iii3ftotal_hs == -9 ~ -9,

          TRUE ~ NA_real_
        ),

        horas = dplyr::case_when(

          iii3etotal_hs != -9 &
            iii3ftotal_hs != -9 ~
            iii3etotal_hs + iii3ftotal_hs,

          iii3etotal_hs == -9 &
            iii3ftotal_hs != -9 ~
            iii3ftotal_hs,

          iii3etotal_hs != -9 &
            iii3ftotal_hs == -9 ~
            iii3etotal_hs,

          TRUE ~ 0
        ),

        horas_recod = dplyr::case_when(

          horas < 35 ~ 1,

          horas > 34 &
            horas < 47 ~ 2,

          horas > 46 ~ 3,

          menos9 == -9 ~ 99,

          TRUE ~ NA_real_
        )
      )
  }


  # ==================================================
  # 2015 EN ADELANTE
  # ==================================================

  if (estructura == "actual") {

    variables_requeridas <- c(
      "vi3",
      "vi4",
      "vi6",
      "condact",
      "iii3g",
      weights
    )

    faltantes <- setdiff(
      variables_requeridas,
      names(base)
    )

    if (length(faltantes) > 0) {
      stop(
        paste0(
          "Faltan las siguientes variables en `base`: ",
          paste(faltantes, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }


  # ==================================================
  # VARIABLES COMUNES
  # ==================================================

  datos <- base |>
    dplyr::mutate(

      # ----------------------------------------------
      # Grupo de edad
      #
      # 65+ se mantiene aca.
      # Se elimina solamente al final del resultado.
      # ----------------------------------------------

      etarios = dplyr::case_when(

        vi6 > 13 &
          vi6 < 30 ~ 1,

        vi6 > 29 &
          vi6 < 65 ~ 2,

        vi6 > 64 ~ 3,

        TRUE ~ NA_real_
      ),

      etarios = haven::labelled(
        etarios,
        labels = c(
          "De 14 a 29 a\u00f1os" = 1,
          "De 30 a 64 a\u00f1os" = 2,
          "65 a\u00f1os y m\u00e1s" = 3
        )
      ),


      # ----------------------------------------------
      # Sexo + grupo de edad
      # ----------------------------------------------

      sexoedad = dplyr::case_when(

        vi4 == 1 &
          etarios == 1 ~
          "Varones de 14 a 29 a\u00f1os",

        vi4 == 1 &
          etarios == 2 ~
          "Varones de 30 a 64 a\u00f1os",

        vi4 == 1 &
          etarios == 3 ~
          "Varones de 65 a\u00f1os y m\u00e1s",

        vi4 == 2 &
          etarios == 1 ~
          "Mujeres de 14 a 29 a\u00f1os",

        vi4 == 2 &
          etarios == 2 ~
          "Mujeres de 30 a 64 a\u00f1os",

        vi4 == 2 &
          etarios == 3 ~
          "Mujeres de 65 a\u00f1os y m\u00e1s",

        TRUE ~ NA_character_
      ),


      # ----------------------------------------------
      # PEA
      # ----------------------------------------------

      pea = dplyr::case_when(

        condact %in% c(1, 2) ~ 1,

        condact == 3 ~ 0,

        TRUE ~ NA_real_
      ),


      # ----------------------------------------------
      # Empleo
      # ----------------------------------------------

      empleo = dplyr::case_when(

        condact == 1 ~ 1,

        condact %in% c(2, 3) ~ 0,

        TRUE ~ NA_real_
      ),


      # ----------------------------------------------
      # Desocupacion
      # ----------------------------------------------

      desocup = dplyr::case_when(

        condact == 2 ~ 1,

        condact %in% c(1, 3) ~ 0,

        TRUE ~ NA_real_
      )
    )


  # ==================================================
  # SUBOCUPACION
  # ==================================================

  if (estructura == "2010_2013") {

    datos <- datos |>
      dplyr::mutate(

        subocup = dplyr::case_when(

          pea == 1 &
            horas_recod == 1 &
            iii3g == 1 ~ 1,

          pea == 1 ~ 0,

          TRUE ~ NA_real_
        )
      )

  } else if (estructura == "2014") {

    datos <- datos |>
      dplyr::mutate(

        subocup = dplyr::case_when(

          pea == 1 &
            horas_recod == 1 &
            iii3g == 1 ~ 1,

          pea == 1 ~ 0,

          TRUE ~ NA_real_
        )
      )

  } else {

    datos <- datos |>
      dplyr::mutate(

        subocup = dplyr::case_when(

          pea == 1 &
            iii3g == 1 ~ 1,

          pea == 1 &
            iii3g != 1 ~ 0,

          TRUE ~ NA_real_
        )
      )
  }


  # ==================================================
  # FUNCION PARA CALCULAR TASAS
  # ==================================================

  calcular_tasa <- function(
    data,
    variable,
    universo
  ) {

    data |>
      dplyr::filter(

        vi6 >= 14,

        !is.na(.data[[weights]]),

        !is.na(.data[[variable]]),

        !is.na(.data[[universo]])
      ) |>
      dplyr::group_by(
        .data[[universo]]
      ) |>
      dplyr::summarise(

        total = sum(
          .data[[weights]],
          na.rm = TRUE
        ),

        numerador = sum(
          .data[[variable]] *
            .data[[weights]],
          na.rm = TRUE
        ),

        .groups = "drop"
      ) |>
      dplyr::mutate(

        porcentaje =
          numerador /
          total *
          100
      )
  }


  # ==================================================
  # FUNCION AUXILIAR PARA AGREGAR TASA
  # ==================================================

  agregar_tasa <- function(
    resultado,
    data,
    variable,
    universo,
    Indicador,
    Desagregacion,
    categorias = NULL,
    excluir_65 = FALSE
  ) {

    datos_tasa <- data

    # ----------------------------------------------
    # Exclusion de 65+ solamente cuando corresponde
    # ----------------------------------------------

    if (excluir_65) {

      datos_tasa <- datos_tasa |>
        dplyr::filter(
          etarios %in% c(1, 2)
        )
    }


    tasa <- calcular_tasa(
      data = datos_tasa,
      variable = variable,
      universo = universo
    )


    if (!is.null(categorias)) {

      tasa <- tasa |>
        dplyr::mutate(

          Categoria =
            categorias[
              as.character(
                .data[[universo]]
              )
            ]
        )

    } else {

      tasa <- tasa |>
        dplyr::mutate(

          Categoria =
            as.character(
              .data[[universo]]
            )
        )
    }


    tasa <- tasa |>
      dplyr::transmute(

        Indicador = Indicador,

        Desagregacion = Desagregacion,

        Categoria = Categoria,

        Porcentaje = round(
          porcentaje,
          1
        )
      )


    dplyr::bind_rows(
      resultado,
      tasa
    )
  }


  # ==================================================
  # RESULTADO
  # ==================================================

  resultado <- tibble::tibble(

    Indicador = character(),

    Desagregacion = character(),

    Categoria = character(),

    Porcentaje = numeric()
  )


  # ==================================================
  # TASA DE ACTIVIDAD
  # ==================================================

  datos <- datos |>
    dplyr::mutate(
      universo_general = 1
    )


  # ----------------------------------------------
  # General
  # ----------------------------------------------

  resultado <- agregar_tasa(

    resultado,
    datos,

    variable = "pea",

    universo = "universo_general",

    Indicador = "Tasa de actividad",

    Desagregacion = "General",

    categorias = c(
      `1` = "14 a\u00f1os y m\u00e1s"
    )
  )


  # ----------------------------------------------
  # Sexo
  # ----------------------------------------------

  datos <- datos |>
    dplyr::mutate(

      sexo = dplyr::case_when(

        vi4 == 1 ~ "Varones",

        vi4 == 2 ~ "Mujeres",

        TRUE ~ NA_character_
      )
    )


  resultado <- agregar_tasa(

    resultado,
    datos,

    variable = "pea",

    universo = "sexo",

    Indicador = "Tasa de actividad",

    Desagregacion = "Sexo"
  )


  # ----------------------------------------------
  # Grupo de edad
  #
  # Aca se calcula tambien 65+.
  # ----------------------------------------------

  resultado <- agregar_tasa(

    resultado,
    datos,

    variable = "pea",

    universo = "etarios",

    Indicador = "Tasa de actividad",

    Desagregacion = "Grupo de edad",

    categorias = c(

      `1` = "14 a 29 a\u00f1os",

      `2` = "30 a 64 a\u00f1os",

      `3` = "65 a\u00f1os y m\u00e1s"
    )
  )


  # ----------------------------------------------
  # Sexo y grupo de edad
  #
  # Aca se excluye 65+.
  # ----------------------------------------------

  resultado <- agregar_tasa(

    resultado,
    datos,

    variable = "pea",

    universo = "sexoedad",

    Indicador = "Tasa de actividad",

    Desagregacion = "Sexo y grupo de edad",

    excluir_65 = TRUE
  )


  # ----------------------------------------------
  # Jefatura
  # ----------------------------------------------

  datos <- datos |>
    dplyr::mutate(

      jefe = dplyr::case_when(

        vi3 == 1 ~ 1,

        !is.na(vi3) ~ 0,

        TRUE ~ NA_real_
      )
    )


  datos_jefe <- datos |>
    dplyr::filter(
      jefe == 1
    ) |>
    dplyr::mutate(
      universo_jefe = 1
    )


  resultado <- agregar_tasa(

    resultado,

    datos_jefe,

    variable = "pea",

    universo = "universo_jefe",

    Indicador = "Tasa de actividad",

    Desagregacion = "Jefatura",

    categorias = c(
      `1` = "Jefes del hogar"
    )
  )


  # ==================================================
  # TASA DE EMPLEO
  # ==================================================

  resultado <- agregar_tasa(

    resultado,
    datos,

    variable = "empleo",

    universo = "universo_general",

    Indicador = "Tasa de empleo",

    Desagregacion = "General",

    categorias = c(
      `1` = "14 a\u00f1os y m\u00e1s"
    )
  )


  resultado <- agregar_tasa(

    resultado,
    datos,

    variable = "empleo",

    universo = "sexo",

    Indicador = "Tasa de empleo",

    Desagregacion = "Sexo"
  )


  resultado <- agregar_tasa(

    resultado,
    datos,

    variable = "empleo",

    universo = "etarios",

    Indicador = "Tasa de empleo",

    Desagregacion = "Grupo de edad",

    categorias = c(

      `1` = "14 a 29 a\u00f1os",

      `2` = "30 a 64 a\u00f1os",

      `3` = "65 a\u00f1os y m\u00e1s"
    )
  )


  resultado <- agregar_tasa(

    resultado,
    datos,

    variable = "empleo",

    universo = "sexoedad",

    Indicador = "Tasa de empleo",

    Desagregacion = "Sexo y grupo de edad",

    excluir_65 = TRUE
  )


  resultado <- agregar_tasa(

    resultado,

    datos_jefe,

    variable = "empleo",

    universo = "universo_jefe",

    Indicador = "Tasa de empleo",

    Desagregacion = "Jefatura",

    categorias = c(
      `1` = "Jefes del hogar"
    )
  )


  # ==================================================
  # TASA DE DESOCUPACION
  # ==================================================

  datos_pea <- datos |>
    dplyr::filter(
      pea == 1
    )


  # ----------------------------------------------
  # General
  # ----------------------------------------------

  resultado <- agregar_tasa(

    resultado,

    datos_pea,

    variable = "desocup",

    universo = "universo_general",

    Indicador = "Tasa de desocupaci\u00f3n",

    Desagregacion = "General",

    categorias = c(
      `1` = "PEA"
    )
  )


  # ----------------------------------------------
  # Sexo
  # ----------------------------------------------

  resultado <- agregar_tasa(

    resultado,

    datos_pea,

    variable = "desocup",

    universo = "sexo",

    Indicador = "Tasa de desocupaci\u00f3n",

    Desagregacion = "Sexo"
  )


  # ----------------------------------------------
  # Grupo de edad
  # ----------------------------------------------

  resultado <- agregar_tasa(

    resultado,

    datos_pea,

    variable = "desocup",

    universo = "etarios",

    Indicador = "Tasa de desocupaci\u00f3n",

    Desagregacion = "Grupo de edad",

    categorias = c(

      `1` = "14 a 29 a\u00f1os",

      `2` = "30 a 64 a\u00f1os",

      `3` = "65 a\u00f1os y m\u00e1s"
    )
  )


  # ----------------------------------------------
  # Sexo y edad
  # ----------------------------------------------

  resultado <- agregar_tasa(

    resultado,

    datos_pea,

    variable = "desocup",

    universo = "sexoedad",

    Indicador = "Tasa de desocupaci\u00f3n",

    Desagregacion = "Sexo y grupo de edad",

    excluir_65 = TRUE
  )


  # ----------------------------------------------
  # Jefatura
  # ----------------------------------------------

  datos_pea_jefe <- datos_jefe |>
    dplyr::filter(
      pea == 1
    )


  resultado <- agregar_tasa(

    resultado,

    datos_pea_jefe,

    variable = "desocup",

    universo = "universo_jefe",

    Indicador = "Tasa de desocupaci\u00f3n",

    Desagregacion = "Jefatura",

    categorias = c(
      `1` = "Jefes del hogar"
    )
  )


  # ==================================================
  # TASA DE SUBOCUPACION
  # ==================================================

  # ----------------------------------------------
  # General
  # ----------------------------------------------

  resultado <- agregar_tasa(

    resultado,

    datos_pea,

    variable = "subocup",

    universo = "universo_general",

    Indicador = "Tasa de subocupaci\u00f3n",

    Desagregacion = "General",

    categorias = c(
      `1` = "PEA"
    )
  )


  # ----------------------------------------------
  # Sexo
  # ----------------------------------------------

  resultado <- agregar_tasa(

    resultado,

    datos_pea,

    variable = "subocup",

    universo = "sexo",

    Indicador = "Tasa de subocupaci\u00f3n",

    Desagregacion = "Sexo"
  )


  # ----------------------------------------------
  # Grupo de edad
  # ----------------------------------------------

  resultado <- agregar_tasa(

    resultado,

    datos_pea,

    variable = "subocup",

    universo = "etarios",

    Indicador = "Tasa de subocupaci\u00f3n",

    Desagregacion = "Grupo de edad",

    categorias = c(

      `1` = "14 a 29 a\u00f1os",

      `2` = "30 a 64 a\u00f1os",

      `3` = "65 a\u00f1os y m\u00e1s"
    )
  )


  # ----------------------------------------------
  # Sexo y edad
  # ----------------------------------------------

  resultado <- agregar_tasa(

    resultado,

    datos_pea,

    variable = "subocup",

    universo = "sexoedad",

    Indicador = "Tasa de subocupaci\u00f3n",

    Desagregacion = "Sexo y grupo de edad",

    excluir_65 = TRUE
  )


  # ----------------------------------------------
  # Jefatura
  # ----------------------------------------------

  resultado <- agregar_tasa(

    resultado,

    datos_pea_jefe,

    variable = "subocup",

    universo = "universo_jefe",

    Indicador = "Tasa de subocupaci\u00f3n",

    Desagregacion = "Jefatura",

    categorias = c(
      `1` = "Jefes del hogar"
    )
  )


  # ==================================================
  # CORRECCION:
  #
  # 65+ se calcula internamente para "Grupo de edad",
  # pero no se muestra en el resultado final.
  #
  # En "Sexo y grupo de edad" ya fue excluido
  # durante el calculo mediante excluir_65 = TRUE.
  # ==================================================

  resultado <- resultado |>
    dplyr::filter(
      !(
        Desagregacion == "Grupo de edad" &
          Categoria == "65 a\u00f1os y m\u00e1s"
      )
    )


  # ==================================================
  # ORDEN FINAL
  # ==================================================

  resultado |>
    dplyr::mutate(

      Indicador = factor(

        Indicador,

        levels = c(

          "Tasa de actividad",

          "Tasa de empleo",

          "Tasa de desocupaci\u00f3n",

          "Tasa de subocupaci\u00f3n"
        )
      ),

      Desagregacion = factor(

        Desagregacion,

        levels = c(

          "General",

          "Sexo",

          "Grupo de edad",

          "Sexo y grupo de edad",

          "Jefatura"
        )
      )
    ) |>
    dplyr::arrange(

      Indicador,

      Desagregacion
    ) |>
    dplyr::mutate(

      Indicador =
        as.character(Indicador),

      Desagregacion =
        as.character(Desagregacion)
    )
}


utils::globalVariables(
  c(
    "ponduni",
    "vi3",
    "vi4",
    "vi6",
    "edad",
    "sexo",
    "parentesco",
    "sexoedad",
    "etarios",
    "Cantidad",
    "Porcentaje",
    "Indicador"
  )
)
