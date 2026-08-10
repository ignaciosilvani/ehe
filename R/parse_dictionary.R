#' Procesa un diseño de registro EHE
#'
#' Función interna que transforma el diseño de registro
#' leído desde Excel en un diccionario de variables y categorías.
#'
#' @param x Diseño de registro leído con readxl.
#'
#' @return Un tibble con variables, descripciones, códigos,
#' etiquetas y tipo de registro.
#'
#' @keywords internal

.parse_dictionary <- function(x) {
  
  x <- x |>
    dplyr::transmute(
      variable = .data$...1,
      descripcion = .data$...2,
      codigo = .data$...3,
      etiqueta = .data$...4
    )
  
  
  x <- x |>
    dplyr::mutate(
      nueva_variable = grepl(
        "^[a-zA-Z_][a-zA-Z0-9_]*$",
        dplyr::coalesce(.data$variable, "")
      )
    )
  
  
  x <- x |>
    dplyr::mutate(
      nueva_variable = .data$nueva_variable &
        !.data$variable %in% c("CAMPO")
    )
  
  
  x <- x |>
    dplyr::mutate(
      variable = dplyr::if_else(
        .data$nueva_variable,
        .data$variable,
        NA_character_
      ),
      descripcion = dplyr::if_else(
        .data$nueva_variable,
        .data$descripcion,
        NA_character_
      )
    ) |>
    tidyr::fill(
      variable,
      descripcion
    )
  
  
  x |>
    dplyr::filter(
      .data$nueva_variable |
        !is.na(.data$codigo)
    ) |>
    dplyr::mutate(
      tipo = dplyr::if_else(
        is.na(.data$codigo),
        "variable",
        "categoria"
      )
    ) |>
    dplyr::select(
      variable,
      descripcion,
      codigo,
      etiqueta,
      tipo
    )
}
