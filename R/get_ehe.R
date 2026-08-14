#' Descarga una base EHE
#'
#' Descarga una base usuaria de la Encuesta de Hogar y Empleo.
#'
#' @param encuesta Tipo de encuesta. Por defecto "EHE-M".
#' @param anio Ano de la encuesta.
#' @param tipo Tipo de base: "individual" o "hogar".
#' @param localidad Localidad de la encuesta.
#'
#' @return Un tibble con la base descargada.
#'
#' @export
get_ehe <- function(encuesta = "EHE-M",
                    anio,
                    tipo = c("individual", "hogar"),
                    localidad = NULL) {


  tipo <- match.arg(tipo)


  recurso <- .find_resource(
    encuesta = encuesta,
    municipio = localidad,
    anio = anio,
    tipo = tipo
  )


  if (nrow(recurso) == 0) {

    stop(
      paste0(
        "No existe una base EHE disponible para:\n",
        encuesta, " - ",
        localidad, " - ",
        anio, " - ",
        tipo
      ),
      call. = FALSE
    )

  }


  if (nrow(recurso) > 1) {

    stop(
      "La busqueda devuelve mas de un recurso. Revise los parametros.",
      call. = FALSE
    )

  }


  message(
    "Descargando: ",
    recurso$resource_name
  )


  archivo <- tempfile(
    fileext = ".csv"
  )


  .download_ckan(
    url = recurso$url,
    destfile = archivo
  )


  datos <- readr::read_csv(
    archivo,
    guess_max = Inf,
    show_col_types = FALSE
  )


  datos <- .as_ehe(
    datos = datos,
    encuesta = encuesta,
    localidad = localidad,
    anio = anio,
    tipo = tipo,
    recurso = recurso
  )


  attr(datos, "encuesta") <- encuesta
  attr(datos, "localidad") <- localidad
  attr(datos, "anio") <- anio
  attr(datos, "tipo") <- tipo
  attr(datos, "resource") <- recurso$resource_name


  datos

}
