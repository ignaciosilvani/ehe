#' Información de una base EHE
#'
#' Muestra los metadatos asociados a una base EHE descargada.
#'
#' @param x Objeto de clase ehe.
#'
#' @return Un tibble con información de la encuesta.
#'
#' @export

ehe_metadata <- function(x) {
  
  if (!inherits(x, "ehe")) {
    stop(
      "El objeto no pertenece a la clase ehe.",
      call. = FALSE
    )
  }
  
  
  tibble::tibble(
    
    encuesta = attr(x, "encuesta"),
    
    localidad = attr(x, "localidad"),
    
    anio = attr(x, "anio"),
    
    tipo = attr(x, "tipo"),
    
    recurso = attr(x, "resource_name"),
    
    resource_id = attr(x, "resource_id"),
    
    url = attr(x, "url"),
    
    casos = nrow(x)
    
  )
  
}
