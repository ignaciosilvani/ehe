# URL base de la API CKAN
CKAN_URL <- "http://datos.estadistica.ec.gba.gov.ar/api/3/action"

#' Consulta la API de CKAN
#'
#' Función interna para realizar consultas a la API CKAN.
#'
#' @param action Endpoint de la API.
#' @param ... Parámetros enviados a la API.
#'
#' @return Una lista con el contenido de `result`.
#'
#' @keywords internal
.ckan_request <- function(action, ...) {

  url <- paste0(CKAN_URL, "/", action)

  parametros <- list(...)

  if (length(parametros) > 0) {

    query <- paste(
      names(parametros),
      utils::URLencode(unlist(parametros), reserved = TRUE),
      sep = "=",
      collapse = "&"
    )

    url <- paste0(url, "?", query)

  }

  json <- jsonlite::fromJSON(
    url,
    simplifyVector = FALSE
  )

  if (!isTRUE(json$success)) {
    stop("La API de CKAN devolvió un error.", call. = FALSE)
  }

  json$result

}

#' Obtiene el catálogo de la EHE desde CKAN
#'
#' @return Lista con los resultados de la consulta.
#'
#' @keywords internal
.ckan_search <- function() {

  .ckan_request(
    action = "package_search",
    fq = "groups:bases-usuarias AND tags:Laboral",
    rows = 1000
  )

}
