#' Convierte un recurso de CKAN en un tibble
#'
#' @keywords internal
.parse_resource <- function(resource, dataset) {
  
  tibble::tibble(
    
    dataset_id = dataset$id,
    dataset_slug = dataset$name,
    dataset      = dataset$title,
    
    resource_id = resource$id,
    resource_name = resource$name,
    
    description = resource$description,
    
    format = resource$format,
    
    size = resource$size,
    
    url = resource$url,
    
    created = resource$created,
    
    modified = resource$modified,
    
    package_id = resource$package_id,
    mimetype   = resource$mimetype,
    position   = resource$position,
    
  )
  
}

#' Convierte un dataset de CKAN en un tibble
#'
#' @keywords internal
.parse_dataset <- function(dataset) {
  
  purrr::map_dfr(
    dataset$resources,
    .parse_resource,
    dataset = dataset
  )
  
}

#' Convierte el catálogo de CKAN en un tibble
#'
#' @keywords internal
.parse_catalog <- function(catalog) {
  
  purrr::map_dfr(
    catalog$results,
    .parse_dataset
  )
  
}
