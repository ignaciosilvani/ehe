#' Construye el catálogo de la EHE
#'
#' Obtiene el catálogo desde CKAN y agrega los metadatos derivados.
#'
#' @return Un tibble con una fila por recurso.
#'
#' @keywords internal
.catalog <- function() {
  
  res <- .ckan_search()
  
  catalogo <- .parse_catalog(res)
  
  catalogo <- .parse_metadata(catalogo)
  
  catalogo
  
}
