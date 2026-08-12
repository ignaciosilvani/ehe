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
#' @param percentage Indica si se calculan porcentajes para
#' tabulados bivariados. Puede ser `"row"`, `"col"` o `NULL`.
#'
#' @return Un tibble con el tabulado.
#'
#' @examples
#' \dontrun{
#' # --------------------------------------------------
#' # Tabulado bivariado ponderado
#' # --------------------------------------------------
#'
#' tab <- ehe_tabulate(
#'   base = base_lab,
#'   x = "nivel_ed",
#'   y = "realizacion_entrevista",
#'   weights = "ponduni"
#' )
#'
#' # --------------------------------------------------
#' # Agregar total por fila
#' # --------------------------------------------------
#'
#' tab_row <- ehe_tabulate(
#'   base = base_lab,
#'   x = "nivel_ed",
#'   y = "realizacion_entrevista",
#'   weights = "ponduni",
#'   add.totals = "row"
#' )
#'
#' # --------------------------------------------------
#' # Agregar total por columna
#' # --------------------------------------------------
#'
#' tab_col <- ehe_tabulate(
#'   base = base_lab,
#'   x = "nivel_ed",
#'   y = "realizacion_entrevista",
#'   weights = "ponduni",
#'   add.totals = "col"
#' )
#'
#' # --------------------------------------------------
#' # Agregar totales por fila y columna
#' # --------------------------------------------------
#'
#' tab_both <- ehe_tabulate(
#'   base = base_lab,
#'   x = "nivel_ed",
#'   y = "realizacion_entrevista",
#'   weights = "ponduni",
#'   add.totals = "both"
#' )
#'
#' # --------------------------------------------------
#' # Porcentajes por fila
#' # --------------------------------------------------
#'
#' pct_row <- ehe_tabulate(
#'   base = base_lab,
#'   x = "nivel_ed",
#'   y = "realizacion_entrevista",
#'   weights = "ponduni",
#'   percentage = "row"
#' )
#'
#' # --------------------------------------------------
#' # Porcentajes por columna
#' # --------------------------------------------------
#'
#' pct_col <- ehe_tabulate(
#'   base = base_lab,
#'   x = "nivel_ed",
#'   y = "realizacion_entrevista",
#'   weights = "ponduni",
#'   percentage = "col"
#' )
#'
#' # --------------------------------------------------
#' # Porcentajes por fila con totales
#' # --------------------------------------------------
#'
#' pct_row_both <- ehe_tabulate(
#'   base = base_lab,
#'   x = "nivel_ed",
#'   y = "realizacion_entrevista",
#'   weights = "ponduni",
#'   add.totals = "both",
#'   percentage = "row"
#' )
#'
#' # --------------------------------------------------
#' # Porcentajes por columna con totales
#' # --------------------------------------------------
#'
#' pct_col_both <- ehe_tabulate(
#'   base = base_lab,
#'   x = "nivel_ed",
#'   y = "realizacion_entrevista",
#'   weights = "ponduni",
#'   add.totals = "both",
#'   percentage = "col"
#' )
#'
#' # --------------------------------------------------
#' # Tabulado univariado ponderado
#' # --------------------------------------------------
#'
#' tab_uni <- ehe_tabulate(
#'   base = base_lab,
#'   x = "nivel_ed",
#'   weights = "ponduni"
#' )
#'
#' # --------------------------------------------------
#' # Tabulado univariado con total
#' # --------------------------------------------------
#'
#' tab_uni_total <- ehe_tabulate(
#'   base = base_lab,
#'   x = "nivel_ed",
#'   weights = "ponduni",
#'   add.totals = "row"
#' )
#' }
#'
#' @export
ehe_tabulate <- function(
  base,
  x,
  y = NULL,
  weights = NULL,
  add.totals = NULL,
  percentage = NULL
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

    if (!is.character(weights) || length(weights) != 1) {
      stop(
        "`weights` debe ser el nombre de una variable.",
        call. = FALSE
      )
    }

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

    if (
      is.null(y) &&
      add.totals %in% c("col", "both")
    ) {
      stop(
        "`add.totals = 'col'` o `'both'` requiere un tabulado bivariado.",
        call. = FALSE
      )
    }
  }

  if (!is.null(percentage)) {

    percentage <- match.arg(
      percentage,
      c("row", "col")
    )

    if (is.null(y)) {
      stop(
        "`percentage` requiere un tabulado bivariado.",
        call. = FALSE
      )
    }
  }


  # ==================================================
  # TABULADO UNIVARIADO
  # ==================================================

  if (is.null(y)) {

    if (is.null(weights)) {

      resultado <- base |>
        dplyr::count(
          .data[[x]],
          name = "frecuencia"
        ) |>
        dplyr::mutate(
          porcentaje =
            .data$frecuencia /
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
            .data[[weights]],
            na.rm = TRUE
          ),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          porcentaje =
            .data$frecuencia /
            sum(.data$frecuencia) * 100
        )
    }


    # -----------------------------------------------
    # Total univariado
    # -----------------------------------------------

    if (
      !is.null(add.totals) &&
      add.totals %in% c("row", "both")
    ) {

      resultado[[x]] <- as.character(
        resultado[[x]]
      )

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


  # ==================================================
  # TABULADO BIVARIADO
  # ==================================================

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
          .data[[weights]],
          na.rm = TRUE
        ),
        .groups = "drop"
      )
  }


  # --------------------------------------------------
  # Pasar a formato ancho
  # --------------------------------------------------

  resultado <- resultado |>
    tidyr::pivot_wider(
      names_from = dplyr::all_of(y),
      values_from = frecuencia,
      values_fill = 0
    )


  # ==================================================
  # GUARDAR NOMBRES DE COLUMNAS DE FRECUENCIA
  # ==================================================

  # `x` puede ser haven_labelled y, por lo tanto,
  # internamente numérico.
  #
  # Identificamos las columnas que contienen las
  # categorías de `y` antes de agregar los totales.

  columnas_frecuencia <- setdiff(
    names(resultado),
    x
  )


  # ==================================================
  # TOTALES
  #
  # Los totales se agregan sobre las frecuencias,
  # antes de calcular porcentajes.
  # ==================================================

  if (!is.null(add.totals)) {

    # -----------------------------------------------
    # Para poder agregar "Total" a x, solamente
    # cuando se solicitan totales, convertimos x
    # a character.
    #
    # Si add.totals = NULL, x queda intacto como
    # haven_labelled.
    # -----------------------------------------------

    resultado[[x]] <- as.character(
      resultado[[x]]
    )


    # -----------------------------------------------
    # Total por fila
    # -----------------------------------------------

    if (
      add.totals %in% c("row", "both")
    ) {

      resultado$Total <- rowSums(
        resultado[
          columnas_frecuencia
        ],
        na.rm = TRUE
      )

      columnas_frecuencia <- c(
        columnas_frecuencia,
        "Total"
      )
    }


    # -----------------------------------------------
    # Total por columna
    # -----------------------------------------------

    if (
      add.totals %in% c("col", "both")
    ) {

      total_columna <- resultado |>
        dplyr::summarise(
          dplyr::across(
            dplyr::all_of(columnas_frecuencia),
            ~ sum(.x, na.rm = TRUE)
          )
        )

      total_columna[[x]] <- "Total"

      resultado <- dplyr::bind_rows(
        resultado,
        total_columna
      ) |>
        dplyr::relocate(
          dplyr::all_of(x)
        )
    }
  }


  # ==================================================
  # PORCENTAJES
  # ==================================================

  if (!is.null(percentage)) {

    # ------------------------------------------------
    # Las únicas columnas que participan del cálculo
    # son las columnas de frecuencias.
    #
    # x queda completamente excluida del cálculo,
    # aunque sea haven_labelled y numérica.
    # ------------------------------------------------

    columnas_pct <- columnas_frecuencia


    # -----------------------------------------------
    # Porcentaje por fila
    # -----------------------------------------------

    if (percentage == "row") {

      totales_fila <- rowSums(
        resultado[
          columnas_pct
        ],
        na.rm = TRUE
      )

      resultado[
        columnas_pct
      ] <- sweep(
        resultado[
          columnas_pct
        ],
        1,
        totales_fila,
        "/"
      ) * 100

      # Evitamos NaN cuando una fila suma cero.

      resultado[
        columnas_pct
      ] <- dplyr::mutate(
        resultado[
          columnas_pct
        ],
        dplyr::across(
          dplyr::everything(),
          ~ ifelse(
            is.finite(.x),
            .x,
            0
          )
        )
      )
    }


    # -----------------------------------------------
    # Porcentaje por columna
    # -----------------------------------------------

    if (percentage == "col") {

      totales_columna <- colSums(
        resultado[
          columnas_pct
        ],
        na.rm = TRUE
      )

      resultado[
        columnas_pct
      ] <- sweep(
        resultado[
          columnas_pct
        ],
        2,
        totales_columna,
        "/"
      ) * 100

      # Evitamos NaN cuando una columna suma cero.

      resultado[
        columnas_pct
      ] <- dplyr::mutate(
        resultado[
          columnas_pct
        ],
        dplyr::across(
          dplyr::everything(),
          ~ ifelse(
            is.finite(.x),
            .x,
            0
          )
        )
      )
    }
  }


  # ==================================================
  # RESULTADO
  # ==================================================

  resultado
}

