#' Tabulados ponderados de EHE
#'
#' Calcula tabulados uni y bivariados utilizando el factor
#' de expansión correspondiente.
#'
#' @param base Base de datos EHE.
#' @param x Variable para el tabulado.
#' @param y Variable opcional para un tabulado bivariado.
#' @param weights Variable de ponderación.
#' @param add.totals Indica si se agrega el total.
#' Puede ser `"row"`, `"col"`, `"both"` o `NULL`.
#'
#' @return Un tibble con el tabulado.
#'
#' @export

ehe_tabulate <- function(
  base,
  x,
  y = NULL,
  weights = NULL,
  add.totals = NULL
) {
  
  # --------------------------------------------------
  # Validaciones
  # --------------------------------------------------
  
  if (!is.data.frame(base)) {
    stop(
      "`base` debe ser un data.frame o tibble.",
      call. = FALSE
    )
  }
  
  if (!is.character(x) || length(x) != 1) {
    stop(
      "`x` debe ser el nombre de una variable.",
      call. = FALSE
    )
  }
  
  if (!x %in% names(base)) {
    stop(
      paste0(
        "La variable `", x,
        "` no está presente en la base."
      ),
      call. = FALSE
    )
  }
  
  if (!is.null(y)) {
    
    if (!is.character(y) || length(y) != 1) {
      stop(
        "`y` debe ser el nombre de una variable.",
        call. = FALSE
      )
    }
    
    if (!y %in% names(base)) {
      stop(
        paste0(
          "La variable `", y,
          "` no está presente en la base."
        ),
        call. = FALSE
      )
    }
  }
  
  if (!is.null(weights)) {
    
    if (!weights %in% names(base)) {
      stop(
        paste0(
          "La variable de ponderación `", weights,
          "` no está presente en la base."
        ),
        call. = FALSE
      )
    }
  }
  
  if (!is.null(add.totals)) {
    
    add.totals <- match.arg(
      add.totals,
      c("row", "col", "both")
    )
    
    if (is.null(y) &&
        add.totals %in% c("col", "both")) {
      
      stop(
        "`add.totals = 'col'` o `'both'` requiere un tabulado bivariado.",
        call. = FALSE
      )
    }
  }
  
  # --------------------------------------------------
  # Tabulado univariado
  # --------------------------------------------------
  
  if (is.null(y)) {
    
    if (is.null(weights)) {
      
      resultado <- base |>
        dplyr::count(
          .data[[x]],
          name = "frecuencia"
        ) |>
        dplyr::mutate(
          porcentaje = .data$frecuencia /
            sum(.data$frecuencia) * 100
        )
      
    } else {
      
      resultado <- base |>
        dplyr::filter(
          !is.na(.data[[x]]),
          !is.na(.data[[weights]])
        ) |>
        dplyr::group_by(
          .data[[x]]
        ) |>
        dplyr::summarise(
          frecuencia = sum(
            .data[[weights]]
          ),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          porcentaje = .data$frecuencia /
            sum(.data$frecuencia) * 100
        )
    }
    
    # ------------------------------------------------
    # Total para univariado
    # ------------------------------------------------
    
    if (!is.null(add.totals) &&
        add.totals %in% c("row", "both")) {
      
      total <- tibble::tibble(
        frecuencia = sum(
          resultado$frecuencia,
          na.rm = TRUE
        ),
        porcentaje = sum(
          resultado$porcentaje,
          na.rm = TRUE
        )
      )
      
      total[[x]] <- "Total"
      
      resultado <- dplyr::bind_rows(
        resultado,
        total
      ) |>
        dplyr::relocate(
          dplyr::all_of(x)
        )
    }
    
    return(resultado)
  }
  
  # --------------------------------------------------
  # Tabulado bivariado
  # --------------------------------------------------
  
  if (is.null(weights)) {
    
    resultado <- base |>
      dplyr::filter(
        !is.na(.data[[x]]),
        !is.na(.data[[y]])
      ) |>
      dplyr::count(
        .data[[x]],
        .data[[y]],
        name = "frecuencia"
      )
    
  } else {
    
    resultado <- base |>
      dplyr::filter(
        !is.na(.data[[x]]),
        !is.na(.data[[y]]),
        !is.na(.data[[weights]])
      ) |>
      dplyr::group_by(
        .data[[x]],
        .data[[y]]
      ) |>
      dplyr::summarise(
        frecuencia = sum(
          .data[[weights]]
        ),
        .groups = "drop"
      )
  }
  
  # --------------------------------------------------
  # Pasar bivariado a formato ancho
  # --------------------------------------------------
  
  resultado <- resultado |>
    tidyr::pivot_wider(
      names_from = dplyr::all_of(y),
      values_from = frecuencia,
      values_fill = 0
    )
  
  # --------------------------------------------------
  # Totales del tabulado bivariado
  # --------------------------------------------------
  
  if (!is.null(add.totals)) {
    
    # ------------------------------------------------
    # Total por fila
    #
    # "col" y "both"
    # agregan una columna Total
    # ------------------------------------------------
    
    if (add.totals %in% c("col", "both")) {
      
      resultado <- resultado |>
        dplyr::mutate(
          Total = rowSums(
            dplyr::across(
              -dplyr::all_of(x)
            ),
            na.rm = TRUE
          )
        )
    }
    
    # ------------------------------------------------
    # Total por columna
    #
    # "row" y "both"
    # agregan una fila Total
    # ------------------------------------------------
    
    if (add.totals %in% c("row", "both")) {
      
      # Convertir x a character para poder
      # incorporar la categoría "Total"
      
      resultado <- resultado |>
        dplyr::mutate(
          !!x := as.character(
            .data[[x]]
          )
        )
      
      # Calcular el total de cada columna
      
      total <- resultado |>
        dplyr::summarise(
          dplyr::across(
            -dplyr::all_of(x),
            ~ sum(.x, na.rm = TRUE)
          )
        ) |>
        dplyr::mutate(
          !!x := "Total"
        ) |>
        dplyr::relocate(
          dplyr::all_of(x)
        )
      
      # Agregar fila Total
      
      resultado <- dplyr::bind_rows(
        resultado,
        total
      )
    }
  }
  
  # --------------------------------------------------
  # Resultado final
  # --------------------------------------------------
  
  resultado
}
