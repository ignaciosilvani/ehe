#' Tasas e indicadores del mercado laboral de EHE
#'
#' Calcula tasas de actividad, empleo, desocupación y subocupación
#' para la población de 14 años y más, utilizando el factor de
#' expansión correspondiente.
#'
#' Las tasas se presentan para:
#' - población de 14 años y más;
#' - sexo;
#' - grupos de edad;
#' - sexo y grupo de edad;
#' - jefes/as de hogar.
#'
#' @param base Base de datos EHE.
#' @param weights Variable de ponderación. Por defecto, `"ponduni"`.
#'
#' @return Un tibble con las columnas:
#' `indicador`, `desagregacion`, `categoria` y `porcentaje`.
#'
#' @export
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
#' ehe_laboral(base)
#' }
#'
ehe_laboral <- function(
  base,
  weights = "ponduni"
) {
  
  # --------------------------------------------------
  # Validaciones
  # --------------------------------------------------
  
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
  
  variables_requeridas <- c(
    "vi3",
    "vi4",
    "vi6",
    "condact",
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
        "Faltan las siguientes variables en `base`: ",
        paste(faltantes, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  
  # --------------------------------------------------
  # Variables auxiliares
  # --------------------------------------------------
  
  datos <- base |>
    dplyr::mutate(
      
      # ----------------------------------------------
      # Grupos de edad
      # ----------------------------------------------
      
      etarios = dplyr::case_when(
        vi6 > 13 & vi6 < 30 ~ 1,
        vi6 > 29 & vi6 < 65 ~ 2,
        vi6 > 64 ~ 3,
        TRUE ~ NA_real_
      ),
      
      etarios = haven::labelled(
        etarios,
        labels = c(
          "De 14 a 29 años" = 1,
          "De 30 a 64 años" = 2,
          "65 años y más" = 3
        )
      ),
      
      
      # ----------------------------------------------
      # Sexo y grupo de edad
      # ----------------------------------------------
      
      sexoedad = dplyr::case_when(
        vi4 == 1 & etarios == 1 ~
          "Varones de 14 a 29 años",
        
        vi4 == 1 & etarios == 2 ~
          "Varones de 30 a 64 años",
        
        vi4 == 1 & etarios == 3 ~
          "Varones de 65 años y más",
        
        vi4 == 2 & etarios == 1 ~
          "Mujeres de 14 a 29 años",
        
        vi4 == 2 & etarios == 2 ~
          "Mujeres de 30 a 64 años",
        
        vi4 == 2 & etarios == 3 ~
          "Mujeres de 65 años y más",
        
        TRUE ~ NA_character_
      ),
      
      
      # ----------------------------------------------
      # Población económicamente activa
      #
      # 1 = ocupado
      # 2 = desocupado
      # ----------------------------------------------
      
      pea = dplyr::case_when(
        condact %in% c(1, 2) ~ 1,
        condact == 3 ~ 0,
        TRUE ~ NA_real_
      ),
      
      
      # ----------------------------------------------
      # Ocupados
      # ----------------------------------------------
      
      empleo = dplyr::case_when(
        condact == 1 ~ 1,
        condact %in% c(2, 3) ~ 0,
        TRUE ~ NA_real_
      ),
      
      
      # ----------------------------------------------
      # Desocupados
      # ----------------------------------------------
      
      desocup = dplyr::case_when(
        condact == 2 ~ 1,
        condact %in% c(1, 3) ~ 0,
        TRUE ~ NA_real_
      ),
      
      
      # ----------------------------------------------
      # Horas trabajadas
      # ----------------------------------------------
      
      horas = dplyr::case_when(
        iii3etotal_hs != 99 | iii3ftotal_hs != 99 ~
          iii3etotal_hs + iii3ftotal_hs,
        TRUE ~ 0
      ),
      
      
      # ----------------------------------------------
      # Agrupación de horas trabajadas
      # ----------------------------------------------
      
      horas_recod = dplyr::case_when(
        horas < 35 ~ 1,
        horas > 34 & horas < 47 ~ 2,
        horas > 46 ~ 3
      ),
      
      
      # ----------------------------------------------
      # Subocupados
      #
      # Se mantiene exactamente la definición
      # utilizada en el informe.
      # ----------------------------------------------
      
      subocup = dplyr::case_when(
        horas_recod == 1 & iii3g == 1 ~ 1,
        TRUE ~ 0
      )
    )
  
  
  # --------------------------------------------------
  # Función auxiliar para calcular una tasa
  # --------------------------------------------------
  
  calcular_tasa <- function(data, variable, universo) {
    
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
        porcentaje = numerador / total * 100
      )
  }
  
  
  # --------------------------------------------------
  # Función auxiliar para agregar una tasa
  # --------------------------------------------------
  
  agregar_tasa <- function(
    resultado,
    data,
    variable,
    universo,
    indicador,
    desagregacion,
    categorias = NULL,
    excluir_65 = FALSE
  ) {
    
    datos_tasa <- data
    
    # ----------------------------------------------
    # Excluir mayores de 65 años cuando corresponde
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
          categoria = categorias[
            as.character(.data[[universo]])
          ]
        )
      
    } else {
      
      tasa <- tasa |>
        dplyr::mutate(
          categoria = as.character(
            .data[[universo]]
          )
        )
    }
    
    tasa <- tasa |>
      dplyr::transmute(
        indicador = indicador,
        desagregacion = desagregacion,
        categoria = categoria,
        porcentaje = round(
          porcentaje,
          1
        )
      )
    
    dplyr::bind_rows(
      resultado,
      tasa
    )
  }
  
  
  # --------------------------------------------------
  # Resultado
  # --------------------------------------------------
  
  resultado <- tibble::tibble(
    indicador = character(),
    desagregacion = character(),
    categoria = character(),
    porcentaje = numeric()
  )
  
  
  # ==================================================
  # TASA DE ACTIVIDAD
  # ==================================================
  
  # ----------------------------------------------
  # General
  # ----------------------------------------------
  
  datos <- datos |>
    dplyr::mutate(
      universo_general = 1
    )
  
  resultado <- agregar_tasa(
    resultado,
    datos,
    variable = "pea",
    universo = "universo_general",
    indicador = "Tasa de actividad",
    desagregacion = "General",
    categorias = c(
      `1` = "14 años y más"
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
    indicador = "Tasa de actividad",
    desagregacion = "Sexo"
  )
  
  
  # ----------------------------------------------
  # Grupo de edad
  #
  # SOLO 14-29 y 30-64
  # ----------------------------------------------
  
  resultado <- agregar_tasa(
    resultado,
    datos,
    variable = "pea",
    universo = "etarios",
    indicador = "Tasa de actividad",
    desagregacion = "Grupo de edad",
    categorias = c(
      `1` = "14 a 29 años",
      `2` = "30 a 64 años"
    ),
    excluir_65 = TRUE
  )
  
  
  # ----------------------------------------------
  # Sexo y edad
  #
  # SOLO 14-29 y 30-64
  # ----------------------------------------------
  
  resultado <- agregar_tasa(
    resultado,
    datos,
    variable = "pea",
    universo = "sexoedad",
    indicador = "Tasa de actividad",
    desagregacion = "Sexo y grupo de edad",
    excluir_65 = TRUE
  )
  
  
  # ----------------------------------------------
  # Jefes/as de hogar
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
    indicador = "Tasa de actividad",
    desagregacion = "Jefatura",
    categorias = c(
      `1` = "Jefes del hogar"
    )
  )
  
  
  # ==================================================
  # TASA DE EMPLEO
  # ==================================================
  
  # General
  
  resultado <- agregar_tasa(
    resultado,
    datos,
    variable = "empleo",
    universo = "universo_general",
    indicador = "Tasa de empleo",
    desagregacion = "General",
    categorias = c(
      `1` = "14 años y más"
    )
  )
  
  
  # Sexo
  
  resultado <- agregar_tasa(
    resultado,
    datos,
    variable = "empleo",
    universo = "sexo",
    indicador = "Tasa de empleo",
    desagregacion = "Sexo"
  )
  
  
  # Grupo de edad: solamente 14-29 y 30-64
  
  resultado <- agregar_tasa(
    resultado,
    datos,
    variable = "empleo",
    universo = "etarios",
    indicador = "Tasa de empleo",
    desagregacion = "Grupo de edad",
    categorias = c(
      `1` = "14 a 29 años",
      `2` = "30 a 64 años"
    ),
    excluir_65 = TRUE
  )
  
  
  # Sexo y edad: solamente 14-29 y 30-64
  
  resultado <- agregar_tasa(
    resultado,
    datos,
    variable = "empleo",
    universo = "sexoedad",
    indicador = "Tasa de empleo",
    desagregacion = "Sexo y grupo de edad",
    excluir_65 = TRUE
  )
  
  
  # Jefes/as de hogar
  
  resultado <- agregar_tasa(
    resultado,
    datos_jefe,
    variable = "empleo",
    universo = "universo_jefe",
    indicador = "Tasa de empleo",
    desagregacion = "Jefatura",
    categorias = c(
      `1` = "Jefes del hogar"
    )
  )
  
  
  # ==================================================
  # TASA DE DESOCUPACIÓN
  # Universo: PEA
  # ==================================================
  
  datos_pea <- datos |>
    dplyr::filter(
      pea == 1
    )
  
  
  # General
  
  resultado <- agregar_tasa(
    resultado,
    datos_pea,
    variable = "desocup",
    universo = "universo_general",
    indicador = "Tasa de desocupación",
    desagregacion = "General",
    categorias = c(
      `1` = "PEA"
    )
  )
  
  
  # Sexo
  
  resultado <- agregar_tasa(
    resultado,
    datos_pea,
    variable = "desocup",
    universo = "sexo",
    indicador = "Tasa de desocupación",
    desagregacion = "Sexo"
  )
  
  
  # Grupo de edad: solamente 14-29 y 30-64
  
  resultado <- agregar_tasa(
    resultado,
    datos_pea,
    variable = "desocup",
    universo = "etarios",
    indicador = "Tasa de desocupación",
    desagregacion = "Grupo de edad",
    categorias = c(
      `1` = "14 a 29 años",
      `2` = "30 a 64 años"
    ),
    excluir_65 = TRUE
  )
  
  
  # Sexo y edad: solamente 14-29 y 30-64
  
  resultado <- agregar_tasa(
    resultado,
    datos_pea,
    variable = "desocup",
    universo = "sexoedad",
    indicador = "Tasa de desocupación",
    desagregacion = "Sexo y grupo de edad",
    excluir_65 = TRUE
  )
  
  
  # Jefes/as de hogar
  
  datos_pea_jefe <- datos_jefe |>
    dplyr::filter(
      pea == 1
    )
  
  resultado <- agregar_tasa(
    resultado,
    datos_pea_jefe,
    variable = "desocup",
    universo = "universo_jefe",
    indicador = "Tasa de desocupación",
    desagregacion = "Jefatura",
    categorias = c(
      `1` = "Jefes del hogar"
    )
  )
  
  
  # ==================================================
  # TASA DE SUBOCUPACIÓN
  # Universo: PEA
  # ==================================================
  
  # General
  
  resultado <- agregar_tasa(
    resultado,
    datos_pea,
    variable = "subocup",
    universo = "universo_general",
    indicador = "Tasa de subocupación",
    desagregacion = "General",
    categorias = c(
      `1` = "PEA"
    )
  )
  
  
  # Sexo
  
  resultado <- agregar_tasa(
    resultado,
    datos_pea,
    variable = "subocup",
    universo = "sexo",
    indicador = "Tasa de subocupación",
    desagregacion = "Sexo"
  )
  
  
  # Grupo de edad: solamente 14-29 y 30-64
  
  resultado <- agregar_tasa(
    resultado,
    datos_pea,
    variable = "subocup",
    universo = "etarios",
    indicador = "Tasa de subocupación",
    desagregacion = "Grupo de edad",
    categorias = c(
      `1` = "14 a 29 años",
      `2` = "30 a 64 años"
    ),
    excluir_65 = TRUE
  )
  
  
  # Sexo y edad: solamente 14-29 y 30-64
  
  resultado <- agregar_tasa(
    resultado,
    datos_pea,
    variable = "subocup",
    universo = "sexoedad",
    indicador = "Tasa de subocupación",
    desagregacion = "Sexo y grupo de edad",
    excluir_65 = TRUE
  )
  
  
  # Jefes/as de hogar
  
  datos_pea_jefe <- datos_jefe |>
    dplyr::filter(
      pea == 1
    )
  
  resultado <- agregar_tasa(
    resultado,
    datos_pea_jefe,
    variable = "subocup",
    universo = "universo_jefe",
    indicador = "Tasa de subocupación",
    desagregacion = "Jefatura",
    categorias = c(
      `1` = "Jefes del hogar"
    )
  )
  
  
  # ==================================================
  # Orden final
  # ==================================================
  
  resultado |>
    dplyr::mutate(
      indicador = factor(
        indicador,
        levels = c(
          "Tasa de actividad",
          "Tasa de empleo",
          "Tasa de desocupación",
          "Tasa de subocupación"
        )
      ),
      desagregacion = factor(
        desagregacion,
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
      indicador,
      desagregacion
    ) |>
    dplyr::mutate(
      indicador = as.character(indicador),
      desagregacion = as.character(desagregacion)
    )
}
