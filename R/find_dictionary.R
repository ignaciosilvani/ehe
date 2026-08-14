#' Busca diseno de registro EHE
#'
#' Funcion interna.
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
        "Dise\u00f1o de registro",
        .data$resource_name
      )
    )


  resultado

}
