#' Etiqueta una base EHE
#'
#' Aplica etiquetas de variables y de valores a una base EHE
#' utilizando el diseno de registro correspondiente.
#'
#' @param data Base de datos EHE.
#' @param dictionary Diccionario obtenido con [ehe_dictionary()].
#' Si es `NULL`, se obtiene automaticamente.
#' @param encuesta Encuesta EHE.
#' @param anio Ano del diseno de registro.
#' @param tipo Tipo de base: `"individual"` o `"hogar"`.
#'
#' @return La base EHE con etiquetas de variables y valores.
#'
#' @export

ehe_labels <- function(
  data,
  dictionary = NULL,
  encuesta = "EHE-M",
  anio = NULL,
  tipo = c("individual", "hogar")
) {

  tipo <- match.arg(tipo)


  # --------------------------------------------------
  # Validaciones
  # --------------------------------------------------

  if (!is.data.frame(data)) {
    stop(
      "`data` debe ser un data.frame o tibble.",
      call. = FALSE
    )
  }


  if (is.null(dictionary)) {

    if (is.null(anio)) {
      stop(
        "Debe indicar `anio` cuando no se proporciona `dictionary`.",
        call. = FALSE
      )
    }

    dictionary <- ehe_dictionary(
      encuesta = encuesta,
      anio = anio,
      tipo = tipo
    )

  }


  # --------------------------------------------------
  # Variables presentes en base y diccionario
  # --------------------------------------------------

  variables <- intersect(
    unique(dictionary$variable),
    names(data)
  )


  # --------------------------------------------------
  # Procesa cada variable
  # --------------------------------------------------

  for (variable in variables) {

    dic_variable <- dictionary |>
      dplyr::filter(
        .data$variable == .env$variable
      )


    # ------------------------------------------------
    # Etiqueta de la variable
    # ------------------------------------------------

    descripcion <- dic_variable$descripcion[
      !is.na(dic_variable$descripcion) &
        dic_variable$descripcion != ""
    ]


    etiqueta <- dic_variable$etiqueta[
      !is.na(dic_variable$etiqueta) &
        dic_variable$etiqueta != ""
    ]


    label_variable <- if (length(descripcion) > 0) {
      descripcion[1]
    } else if (length(etiqueta) > 0) {
      etiqueta[1]
    } else {
      NULL
    }


    if (!is.null(label_variable)) {

      attr(
        data[[variable]],
        "label"
      ) <- label_variable

    }


    # ------------------------------------------------
    # Etiquetas de valores
    # ------------------------------------------------

    categorias <- dic_variable |>
      dplyr::filter(
        .data$tipo == "categoria",
        !is.na(.data$codigo),
        !is.na(.data$etiqueta),
        .data$etiqueta != ""
      )


    # Si no tiene categorias, continua
    if (nrow(categorias) == 0) {
      next
    }


    # No se recodifican variables logical o character.
    # ehe_labels() solo agrega etiquetas a variables
    # numericas que poseen categorias en el diccionario.
    if (!is.numeric(data[[variable]])) {
      next
    }


    labels <- stats::setNames(
      categorias$codigo,
      categorias$etiqueta
    )


    data[[variable]] <- haven::labelled(
      data[[variable]],
      labels = labels,
      label = label_variable
    )

  }


  data
}
