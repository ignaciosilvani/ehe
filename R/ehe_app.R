#' Shiny application to explore EHE labor indicators
#'
#' Opens a Shiny application that allows users to select a municipality
#' and a year and consult the labor indicators calculated by
#' [ehe_laboral()].
#'
#' @return Does not return an object. Runs a Shiny application.
#'
#' @examples
#' \dontrun{
#' ehe_app()
#' }
#'
#' @export
ehe_app <- function() {

  # ------------------------------------------------------------
  # Bases disponibles
  # ------------------------------------------------------------

  disponibles <- available_ehe() |>
    dplyr::filter(
      tipo == "individual"
    ) |>
    dplyr::distinct(
      municipio,
      anio
    ) |>
    dplyr::arrange(
      municipio,
      anio
    )

  # ------------------------------------------------------------
  # Interfaz
  # ------------------------------------------------------------

  ui <- shiny::fluidPage(

    shiny::titlePanel(
      "Encuesta de Hogar y Empleo \u2014 Indicadores laborales"
    ),

    shiny::sidebarLayout(

      shiny::sidebarPanel(

        shiny::selectInput(
          inputId = "municipio",
          label = "Municipio",
          choices = sort(unique(disponibles$municipio)),
          selected = sort(unique(disponibles$municipio))[1]
        ),

        shiny::selectInput(
          inputId = "anio",
          label = "A\u00f1o",
          choices = NULL
        ),

        shiny::actionButton(
          inputId = "actualizar",
          label = "Consultar",
          class = "btn-primary"
        )
      ),

      shiny::mainPanel(

        shiny::h3("Indicadores laborales"),

        shiny::tableOutput(
          outputId = "indicadores"
        )
      )
    )
  )

  # ------------------------------------------------------------
  # Servidor
  # ------------------------------------------------------------

  server <- function(input, output, session) {

    # ----------------------------------------------------------
    # Actualizar a\u00f1os seg\u00fan municipio
    # ----------------------------------------------------------

    shiny::observeEvent(
      input$municipio,
      {

        anios <- disponibles |>
          dplyr::filter(
            municipio == input$municipio
          ) |>
          dplyr::pull(anio) |>
          sort()

        shiny::updateSelectInput(
          session,
          "anio",
          choices = anios,
          selected = max(anios, na.rm = TRUE)
        )
      },
      ignoreInit = FALSE
    )

    # ----------------------------------------------------------
    # Calcular indicadores
    # ----------------------------------------------------------

    indicadores <- shiny::eventReactive(
      input$actualizar,
      {

        shiny::validate(
          shiny::need(
            input$municipio != "",
            "Seleccion\u00e1 un municipio."
          ),
          shiny::need(
            input$anio != "",
            "Seleccion\u00e1 un a\u00f1o."
          )
        )

        base <- get_ehe(
          encuesta = "EHE-M",
          anio = as.numeric(input$anio),
          tipo = "individual",
          localidad = input$municipio
        )

        ehe_laboral(
          base = base,
          weights = "ponduni"
        )
      }
    )

    # ----------------------------------------------------------
    # Tabla
    # ----------------------------------------------------------

    output$indicadores <- shiny::renderTable(
      {

        indicadores()

      },
      striped = TRUE,
      bordered = TRUE,
      hover = TRUE
    )
  }

  # ------------------------------------------------------------
  # Ejecutar aplicaci\u00f3n
  # ------------------------------------------------------------

  shiny::shinyApp(
    ui = ui,
    server = server
  )
}
