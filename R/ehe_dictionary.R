#' Obtiene el diseno de registro EHE
#'
#' Descarga, lee y procesa el diseno de registro
#' de una encuesta EHE.
#'
#' @param encuesta Encuesta EHE.
#' @param anio Ano del diseno de registro.
#' @param tipo Tipo de base: `"hogar"` o `"individual"`.
#'
#' @return Un tibble con variables, descripciones,
#' codigos y etiquetas.
#'
#' @export

ehe_dictionary <- function(
  encuesta = "EHE-M",
  anio,
  tipo = c("individual", "hogar")
) {

  tipo <- match.arg(tipo)


  recurso <- .find_dictionary(
    encuesta = encuesta,
    anio_busqueda = anio
  )


  if (nrow(recurso) == 0) {
    stop(
      "No existe diseno de registro para esa encuesta y ano.",
      call. = FALSE
    )
  }


  archivo <- tempfile(
    fileext = ".xlsx"
  )


  .download_ckan(
    url = recurso$url,
    destfile = archivo
  )


  hoja <- if (tipo == "hogar") {
    "Hogar"
  } else {
    "Personas"
  }


  diccionario <- readxl::read_excel(
    archivo,
    sheet = hoja,
    col_names = FALSE
  )


  .parse_dictionary(
    diccionario
  )

}
