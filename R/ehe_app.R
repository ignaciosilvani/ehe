#' Aplicacion Shiny para explorar indicadores laborales de la EHE
#'
#' Abre una aplicacion Shiny que permite seleccionar un municipio
#' y un anio y consultar los indicadores laborales calculados
#' mediante [ehe_laboral()].
#'
#' @return No devuelve un objeto. Ejecuta una aplicacion Shiny.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ehe_app()
#' }
ehe_app <- function() {

  # Consultar bases disponibles
  disponibles <- available_ehe()

  # Ordenar y limpiar municipios
  municipios <- disponibles |>
    dplyr::distinct(municipio) |>
    dplyr::filter(!is.na(municipio)) |>
    dplyr::arrange(municipio)

  shiny::shinyApp(

    ui = shiny::fluidPage(

      shiny::titlePanel(
        shiny::div(
          shiny::h2(
            "Encuesta de Hogar y Empleo - Indicadores del mercado laboral"
          ),
          shiny::h5(
            "Municipios de la Provincia de Buenos Aires"
          )
        )
      ),

      shiny::sidebarLayout(

        shiny::sidebarPanel(

          shiny::selectInput(
            inputId = "municipio",
            label = "Municipio",
            choices = municipios$municipio,
            selected = municipios$municipio[1]
          ),

          shiny::selectInput(
            inputId = "anio",
            label = "Anio",
            choices = NULL
          ),

          shiny::tags$hr(),

          shiny::tags$p(
            "Desarrollado con el paquete ",
            shiny::tags$strong("{ehe}"),
            style = paste(
              "text-align: center;",
              "color: #666;",
              "font-size: 12px;"
            )
          )
        ),

        shiny::mainPanel(

          shiny::h3(
            shiny::textOutput("titulo_resultado")
          ),

          shiny::tableOutput("indicadores"),

          shiny::tags$p(
            shiny::textOutput("fuente"),
            style = paste(
              "color: #666;",
              "font-size: 12px;",
              "margin-top: 10px;"
            )
          )
        )
      )
    ),

    server = function(input, output, session) {

      # Actualizar anios segun municipio
      shiny::observeEvent(
        input$municipio,
        {

          anios <- disponibles |>
            dplyr::filter(municipio == input$municipio) |>
            dplyr::distinct(anio) |>
            dplyr::filter(!is.na(anio)) |>
            dplyr::arrange(anio)

          shiny::updateSelectInput(
            session = session,
            inputId = "anio",
            choices = anios$anio,
            selected = max(anios$anio)
          )
        },
        ignoreInit = FALSE
      )

      # Base seleccionada
      base_seleccionada <- shiny::reactive({

        shiny::req(
          input$municipio,
          input$anio
        )

        recurso <- disponibles |>
          dplyr::filter(
            municipio == input$municipio,
            anio == input$anio,
            tipo == "individual"
          )

        shiny::validate(
          shiny::need(
            nrow(recurso) > 0,
            "No se encontro una base individual para la seleccion."
          )
        )

        get_ehe(
          encuesta = recurso$encuesta[1],
          anio = recurso$anio[1],
          tipo = "individual",
          localidad = recurso$municipio[1]
        )
      })

      # Indicadores laborales
      indicadores <- shiny::reactive({

        base <- base_seleccionada()

        ehe_laboral(
          base = base,
          weights = "ponduni"
        )
      })

      # Titulo de los resultados
      output$titulo_resultado <- shiny::renderText({

        shiny::req(
          input$municipio,
          input$anio
        )

        paste(
          input$municipio,
          "-",
          input$anio
        )
      })

      # Mostrar indicadores
      output$indicadores <- shiny::renderTable({

        datos <- indicadores()

        datos |>
          dplyr::mutate(
            porcentaje = paste0(
              format(
                round(porcentaje, 1),
                decimal.mark = ","
              ),
              "%"
            )
          )
      })

      # Fuente de los datos
      output$fuente <- shiny::renderText({

        shiny::req(input$anio)

        paste(
          "Fuente: EHE",
          input$anio,
          "- Direccion Provincial de Estadistica"
        )
      })
    }
  )
}
