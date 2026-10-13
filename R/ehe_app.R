#' Aplicacion Shiny para explorar indicadores de la EHE
#'
#' Abre una aplicacion Shiny que permite seleccionar un municipio,
#' un anio y el tipo de indicadores a consultar.
#'
#' Los indicadores laborales se calculan mediante [ehe_laboral()]
#' y los indicadores sociodemograficos mediante [ehe_demografico()].
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

  # ------------------------------------------------------------
  # Recursos de la EHE
  # ------------------------------------------------------------

  shiny::addResourcePath(
    "ehe-www",
    system.file(
      "www",
      package = "ehe"
    )
  )

  # ------------------------------------------------------------
  # Consultar bases disponibles
  # ------------------------------------------------------------

  disponibles <- available_ehe()

  # ------------------------------------------------------------
  # Ordenar y limpiar municipios
  # ------------------------------------------------------------

  municipios <- disponibles |>
    dplyr::distinct(municipio) |>
    dplyr::filter(!is.na(municipio)) |>
    dplyr::arrange(municipio)

  # ------------------------------------------------------------
  # Aplicacion
  # ------------------------------------------------------------

  shiny::shinyApp(

    # ==========================================================
    # UI
    # ==========================================================

    ui = shiny::fluidPage(

      # --------------------------------------------------------
      # CSS
      # --------------------------------------------------------

      shiny::tags$head(

        shiny::tags$style(
          shiny::HTML("

          /* --------------------------------------------------
             ESTILO GENERAL
          -------------------------------------------------- */

          body {
            background-color: #f4f5f6;
            color: #333333;
            font-family: Arial, Helvetica, sans-serif;
            margin: 0;
          }

          .container-fluid {
            padding-left: 0;
            padding-right: 0;
          }


          /* --------------------------------------------------
             ENCABEZADO
          -------------------------------------------------- */

          .ehe-header {
            position: relative;
            width: 100%;
            height: 150px;
            overflow: hidden;
          }

          .ehe-banner {
            position: absolute;
            inset: 0;
            width: 100%;
            height: 100%;
            object-fit: fill;
          }

          .ehe-header-main {
            position: relative;
            z-index: 2;
            height: 100%;
            padding: 25px 45px;
            box-sizing: border-box;

            display: flex;
            flex-direction: column;
            justify-content: center;
          }

          .ehe-header-title {
            margin: 0;
            padding-right: 200px;
            font-size: 30px;
            font-weight: 600;
            color: #ffffff;
            line-height: 1.2;
            text-shadow: 0 1px 2px rgba(0,0,0,0.15);
          }

          .ehe-header-subtitle {
            margin-top: 8px;
            padding-right: 200px;
            font-size: 16px;
            color: #ffffff;
            text-shadow: 0 1px 2px rgba(0,0,0,0.15);
          }

          .ehe-logo {
            position: absolute;
            top: 25px;
            right: 45px;
            width: 140px;
            height: auto;
          }


          /* --------------------------------------------------
             CONTENEDOR PRINCIPAL
          -------------------------------------------------- */

          .ehe-content {
            max-width: 1250px;
            margin: 0 auto;
            padding: 30px 30px 40px 30px;
          }


          /* --------------------------------------------------
             TITULOS DE SECCION
          -------------------------------------------------- */

          .ehe-section-title {
            font-size: 13px;
            font-weight: 700;
            text-transform: uppercase;
            letter-spacing: 0.6px;
            color: #168eb6;
            margin-top: 0;
            margin-bottom: 18px;
          }


          /* --------------------------------------------------
             PANEL DE SELECCION
          -------------------------------------------------- */

          .ehe-selection {
            background-color: #ffffff;
            border: 1px solid #dddddd;
            border-left: 5px solid #168eb6;
            border-radius: 3px;
            padding: 22px 25px 12px 25px;
            margin-bottom: 25px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.05);
          }

          .ehe-selection .form-group {
            margin-bottom: 15px;
          }

          .ehe-selection label {
            font-size: 13px;
            font-weight: 600;
            color: #444444;
            margin-bottom: 6px;
          }

          .ehe-selection .form-control {
            border: 1px solid #cfcfcf;
            border-radius: 3px;
            height: 38px;
            font-size: 14px;
            color: #333333;
            box-shadow: none;
          }

          .ehe-selection .form-control:focus {
            border-color: #168eb6;
            box-shadow: 0 0 0 1px #168eb6;
          }


          /* --------------------------------------------------
             RESULTADOS
          -------------------------------------------------- */

          .ehe-results {
            background-color: #ffffff;
            border: 1px solid #dddddd;
            border-radius: 3px;
            padding: 25px 30px 28px 30px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.05);
          }

          .ehe-result-title {
            font-size: 22px;
            font-weight: 600;
            color: #333333;
            margin-top: 0;
            margin-bottom: 22px;
            padding-bottom: 12px;
            border-bottom: 2px solid #168eb6;
          }


          /* --------------------------------------------------
             TABLA
          -------------------------------------------------- */

          .ehe-results .table {
            margin-bottom: 10px;
            font-size: 14px;
          }

          .ehe-results .table > thead > tr > th {
            background-color: #e9ecef;
            color: #333333;
            font-weight: 600;
            border-bottom: 2px solid #168eb6;
            vertical-align: middle;
          }

          .ehe-results .table > tbody > tr > td {
            vertical-align: middle;
            border-top: 1px solid #e5e5e5;
          }

          .ehe-results .table-striped > tbody > tr:nth-of-type(odd) {
            background-color: #fafafa;
          }


          /* --------------------------------------------------
             FUENTE
          -------------------------------------------------- */

          .ehe-source {
            margin-top: 18px;
            padding-top: 12px;
            border-top: 1px solid #dddddd;
            color: #666666;
            font-size: 12px;
          }


          /* --------------------------------------------------
             PIE
          -------------------------------------------------- */

          .ehe-footer {
            max-width: 1250px;
            margin: 0 auto;
            padding: 18px 30px 30px 30px;
            color: #777777;
            font-size: 12px;
            border-top: 1px solid #dddddd;
          }

          .ehe-footer strong {
            color: #555555;
          }


          /* --------------------------------------------------
             RESPONSIVE
          -------------------------------------------------- */

          @media (max-width: 768px) {

            .ehe-header {
              height: 140px;
            }

            .ehe-header-main {
              padding: 20px;
            }

            .ehe-header-title {
              font-size: 23px;
              padding-right: 160px;
            }

            .ehe-header-subtitle {
              font-size: 14px;
              padding-right: 160px;
            }

            .ehe-logo {
              right: 20px;
              top: 20px;
              width: 120px;
            }

            .ehe-content {
              padding: 20px 15px 30px 15px;
            }

            .ehe-results {
              padding: 20px 15px;
            }

            .ehe-result-title {
              font-size: 19px;
            }
          }

          ")
        )
      ),


      # ========================================================
      # ENCABEZADO
      # ========================================================

      shiny::div(
        class = "ehe-header",

        # Banner como fondo del encabezado
        shiny::img(
          src = "ehe-www/banner_ehe.png",
          class = "ehe-banner"
        ),

        # Contenido superpuesto al banner
        shiny::div(
          class = "ehe-header-main",

          shiny::h1(
            class = "ehe-header-title",
            "Encuesta de Hogares y Empleo"
          ),

          shiny::div(
            class = "ehe-header-subtitle",
            "Indicadores de los municipios de la Provincia de Buenos Aires"
          ),

          shiny::img(
            src = "ehe-www/logo_ehe.png",
            class = "ehe-logo"
          )
        )
      ),


      # ========================================================
      # CONTENIDO
      # ========================================================

      shiny::div(
        class = "ehe-content",

        # ------------------------------------------------------
        # SELECCION
        # ------------------------------------------------------

        shiny::div(
          class = "ehe-selection",

          shiny::div(
            class = "ehe-section-title",
            "Selecci\u00f3n de indicadores"
          ),

          shiny::fluidRow(

            # Municipio
            shiny::column(
              width = 4,

              shiny::selectInput(
                inputId = "municipio",
                label = "Municipio",
                choices = municipios$municipio,
                selected = municipios$municipio[1]
              )
            ),

            # Anio
            shiny::column(
              width = 2,

              shiny::selectInput(
                inputId = "anio",
                label = "A\u00f1o",
                choices = NULL
              )
            ),

            # Tipo de indicador
            shiny::column(
              width = 3,

              shiny::selectInput(
                inputId = "tipo_indicador",
                label = "Indicadores",
                choices = c(
                  "Sociodemogr\u00e1ficos" = "demografico",
                  "Mercado laboral" = "laboral"
                ),
                selected = "demografico"
              )
            ),

            # Indicador laboral
            shiny::column(
              width = 3,

              shiny::conditionalPanel(
                condition = "input.tipo_indicador == 'laboral'",

                shiny::selectInput(
                  inputId = "indicador_laboral",
                  label = "Indicador laboral",
                  choices = c(
                    "Tasa de actividad" = "Tasa de actividad",
                    "Tasa de empleo" = "Tasa de empleo",
                    "Tasa de desocupaci\u00f3n" = "Tasa de desocupaci\u00f3n",
                    "Tasa de subocupaci\u00f3n" = "Tasa de subocupaci\u00f3n"
                  ),
                  selected = "Tasa de actividad"
                )
              )
            )
          )
        ),


        # ------------------------------------------------------
        # RESULTADOS
        # ------------------------------------------------------

        shiny::div(
          class = "ehe-results",

          shiny::h2(
            class = "ehe-result-title",
            shiny::textOutput("titulo_resultado")
          ),

          shiny::tableOutput(
            "indicadores"
          ),

          shiny::div(
            class = "ehe-source",
            shiny::textOutput("fuente")
          )
        )
      ),


      # ========================================================
      # PIE
      # ========================================================

      shiny::div(
        class = "ehe-footer",

        shiny::HTML(
          "<strong>Encuesta de Hogares y Empleo</strong> \u00b7
            Direcci\u00f3n Provincial de Estad\u00edstica \u00b7
           Provincia de Buenos Aires"
        )
      )
    ),


    # ==========================================================
    # SERVER
    # ==========================================================

    server = function(input, output, session) {

      # --------------------------------------------------------
      # Actualizar anios segun municipio
      # --------------------------------------------------------

      shiny::observeEvent(
        input$municipio,
        {

          anios <- disponibles |>
            dplyr::filter(
              municipio == input$municipio
            ) |>
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


      # --------------------------------------------------------
      # Base seleccionada
      # --------------------------------------------------------

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
            "No se encontro una base individual para la selecci\u00f3n."
          )
        )

        get_ehe(
          encuesta = recurso$encuesta[1],
          anio = recurso$anio[1],
          tipo = "individual",
          localidad = recurso$municipio[1]
        )
      })


      # --------------------------------------------------------
      # Indicadores
      # --------------------------------------------------------

      indicadores <- shiny::reactive({

        base <- base_seleccionada()

        shiny::req(
          input$tipo_indicador
        )

        # ------------------------------------------------------
        # Mercado laboral
        # ------------------------------------------------------

        if (input$tipo_indicador == "laboral") {

          shiny::req(
            input$indicador_laboral
          )

          ehe_laboral(
            base = base,
            weights = "ponduni"
          ) |>
            dplyr::filter(
              Indicador == input$indicador_laboral
            )

          # ------------------------------------------------------
          # Sociodemograficos
          # ------------------------------------------------------

        } else {

          ehe_demografico(
            base = base
          )
        }
      })


      # --------------------------------------------------------
      # Titulo de los resultados
      # --------------------------------------------------------

      output$titulo_resultado <- shiny::renderText({

        shiny::req(
          input$municipio,
          input$anio,
          input$tipo_indicador
        )

        if (input$tipo_indicador == "laboral") {

          paste(
            input$indicador_laboral,
            "-",
            input$municipio,
            "-",
            input$anio
          )

        } else {

          paste(
            "Indicadores sociodemogr\u00e1ficos",
            "-",
            input$municipio,
            "-",
            input$anio
          )
        }
      })


      # --------------------------------------------------------
      # Mostrar indicadores
      # --------------------------------------------------------

      output$indicadores <- shiny::renderTable({

        datos <- indicadores()

        # ------------------------------------------------------
        # Indicadores laborales
        # ------------------------------------------------------

        if (input$tipo_indicador == "laboral") {

          datos |>
            dplyr::mutate(
              Porcentaje = paste0(
                format(
                  round(
                    Porcentaje,
                    1
                  ),
                  decimal.mark = ","
                ),
                "%"
              )
            )

          # ------------------------------------------------------
          # Indicadores sociodemograficos
          # ------------------------------------------------------

        } else {

          datos |>
            dplyr::mutate(
              Porcentaje = paste0(
                format(
                  round(
                    Porcentaje,
                    1
                  ),
                  decimal.mark = ","
                ),
                "%"
              )
            )
        }
      })


      # --------------------------------------------------------
      # Fuente de los datos
      # --------------------------------------------------------

      output$fuente <- shiny::renderText({

        shiny::req(
          input$anio
        )

        paste(
          "Fuente: EHE",
          input$anio,
          "- Direcci\u00f3n Provincial de Estad\u00edstica"
        )
      })
    }
  )
}
