#' Busca diseño de registro EHE
#'
#' Función interna.
#'
#' @keywords internal

.find_dictionary <- function(encuesta,
                             anio_busqueda) {
  
  
  catalogo <- .catalog()
  
  
  resultado <- catalogo |>
    dplyr::filter(
      .data$encuesta == encuesta,
      .data$anio == anio_busqueda,
      grepl(
        "Diseño de registro",
        .data$resource_name
      )
    )
  
  
  resultado
  
}
