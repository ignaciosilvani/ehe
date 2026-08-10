#' Agrega metadatos derivados al catálogo
#'
#' @keywords internal
.parse_metadata <- function(catalogo) {
  
  catalogo |>
    dplyr::mutate(
      
      encuesta = dplyr::case_when(
        stringr::str_detect(resource_name, "EHE-M") ~ "EHE-M",
        stringr::str_detect(resource_name, "EHE-P") ~ "EHE-P",
        TRUE ~ NA_character_
      ),
      
      tipo = dplyr::case_when(
        stringr::str_detect(resource_name, "Individual") ~ "individual",
        stringr::str_detect(resource_name, "Hogar") ~ "hogar",
        stringr::str_detect(resource_name, "Diseño de registro") ~ "diseno",
        TRUE ~ NA_character_
      ),
      
      anio = as.integer(
        stringr::str_extract(resource_name, "\\d{4}$")
      ),
      
      municipio = dplyr::case_when(
        
        encuesta == "EHE-M" &
          tipo %in% c("hogar", "individual") ~
          
          stringr::str_remove(
            resource_name,
            "^Base Usuaria EHE-M (Hogar|Individual) "
          ) |>
          
          stringr::str_remove(" \\d{4}$"),
        
        TRUE ~ NA_character_
        
      ),
      recurso = paste(encuesta, tipo, municipio, anio, sep = "_")
      
    )
  
}
