#' Busca un recurso en el catalogo
#'
#' @param encuesta Tipo de encuesta ("EHE-M" o "EHE-P").
#' @param anio Ano de la encuesta.
#' @param tipo Tipo de base ("individual" o "hogar").
#' @param municipio Municipio (solo para EHE-M).
#'
#' @return Una fila del catalogo.
#'
#' @keywords internal
.find_resource <- function(encuesta,
                           anio,
                           tipo,
                           municipio = NULL) {

  catalogo <- .catalog()

  recurso <- catalogo |>
    dplyr::filter(
      encuesta == !!encuesta,
      anio == !!anio,
      tipo == !!tipo
    )

  if (encuesta == "EHE-M") {
    recurso <- recurso |>
      dplyr::filter(municipio == !!municipio)
  }

  if (nrow(recurso) == 0) {
    stop("No se encontro un recurso con esos parametros.", call. = FALSE)
  }

  if (nrow(recurso) > 1) {
    stop("La busqueda devolvio mas de un recurso.", call. = FALSE)
  }

  recurso
}
