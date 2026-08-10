#' Descarga un archivo desde el portal CKAN
#'
#' Función interna para descargar recursos.
#'
#' @param url URL del recurso.
#' @param destfile Archivo destino.
#'
#' @keywords internal
.download_ckan <- function(url, destfile) {
  
  url <- sub(
    "^https://",
    "http://",
    url
  )
  
  utils::download.file(
    url = url,
    destfile = destfile,
    mode = "wb",
    quiet = TRUE
  )
  
}
