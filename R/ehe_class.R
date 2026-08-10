#' Agrega metadatos a una base EHE
#'
#' Función interna.
#'
#' @keywords internal

.as_ehe <- function(datos,
                    encuesta,
                    localidad,
                    anio,
                    tipo,
                    recurso) {
  
  
  attr(datos, "encuesta") <- encuesta
  
  attr(datos, "localidad") <- localidad
  
  attr(datos, "anio") <- anio
  
  attr(datos, "tipo") <- tipo
  
  attr(datos, "resource_name") <- recurso$resource_name
  
  attr(datos, "resource_id") <- recurso$resource_id
  
  attr(datos, "url") <- recurso$url
  
  class(datos) <- c(
    "ehe",
    class(datos)
  )
  
  datos
  
}
