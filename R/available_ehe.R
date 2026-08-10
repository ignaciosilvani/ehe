#' Consulta las bases EHE disponibles
#'
#' Devuelve un catálogo de las encuestas EHE disponibles
#' por localidad, año y tipo de base.
#'
#' @param encuesta Tipo de encuesta.
#' @param localidad Localidad o municipio.
#' @param anio Año.
#'
#' @return Un tibble con las encuestas disponibles.
#'
#' @export
available_ehe <- function(encuesta = NULL,
                          localidad = NULL,
                          anio = NULL) {


  catalogo <- .catalog()


  if (!is.null(encuesta)) {

    catalogo <- catalogo |>
      dplyr::filter(.data$encuesta %in% encuesta)

  }


  if (!is.null(localidad)) {

    catalogo <- catalogo |>
      dplyr::filter(.data$municipio %in% localidad)

  }


  if (!is.null(anio)) {

    catalogo <- catalogo |>
      dplyr::filter(.data$anio %in% anio)

  }


  catalogo |>
    dplyr::select(
      encuesta,
      municipio,
      anio,
      tipo,
      resource_name
    ) |>
    dplyr::arrange(
      encuesta,
      municipio,
      anio,
      tipo
    )

}

#devtools::load_all()
