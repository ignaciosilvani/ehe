
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
  # RECURSOS DE LA EHE
  # ------------------------------------------------------------

  shiny::addResourcePath(
    "ehe-www",
    system.file("www", package = "ehe")
  )

  # ------------------------------------------------------------
  # BASES DISPONIBLES
  # ------------------------------------------------------------

  disponibles <- available_ehe()

  municipios <- disponibles |>
    dplyr::distinct(municipio) |>
    dplyr::filter(!is.na(municipio)) |>
    dplyr::arrange(municipio)

  # ------------------------------------------------------------
  # PREPARAR MAPA DE MUNICIPIOS
  # ------------------------------------------------------------

  normalizar_municipio <- function(x) {
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    toupper(trimws(x))
  }

  ruta_mapa <- system.file(
    "mapas",
    "Provincia.shp",
    package = "ehe"
  )

  # Alternativa para desarrollo local
  if (ruta_mapa == "") {
    ruta_mapa <- file.path(
      "inst",
      "mapas",
      "Provincia.shp"
    )
  }

  if (!file.exists(ruta_mapa)) {
    stop(
      paste0(
        "No se encontro el shapefile en: ",
        ruta_mapa,
        ". Verifica que Provincia.shp y sus archivos asociados ",
        "esten dentro de inst/mapas/."
      )
    )
  }

  municipios_shp <- sf::st_read(
    ruta_mapa,
    quiet = TRUE
  )

  municipios_shp$municipio_clave <-
    normalizar_municipio(municipios_shp$PARTIDO)

  # ------------------------------------------------------------
  # APLICACION
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
             PANEL DE RESULTADOS
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
             DISTRIBUCION: TABLA IZQUIERDA, MAPA DERECHA
          -------------------------------------------------- */

          .ehe-results-layout {
            display: flex;
            align-items: flex-start;
            gap: 25px;
            width: 100%;
            box-sizing: border-box;
          }

          .ehe-table {
            flex: 1 1 0;
            min-width: 0;
          }

          .ehe-map {
            flex: 0 0 36%;
            min-width: 0;
            margin-top: 0;
          }

          .ehe-map .shiny-plot-output {
            width: 100%;
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

          .ehe-table .table-responsive {
            overflow-x: auto;
          }


          /* --------------------------------------------------
             FUENTE
          -------------------------------------------------- */

          .ehe-source {
            margin-top: 18px;
            padding-top: 0;
            border-top: none;
            color: #666666;
            font-size: 12px;
          }


          /* --------------------------------------------------
             PIE
          -------------------------------------------------- */

          .ehe-footer {
            text-align: center;
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

            .ehe-results-layout {
              flex-direction: column;
            }

            .ehe-table,
            .ehe-map {
              width: 100%;
              flex: 1 1 auto;
            }

            .ehe-map {
              margin-top: 15px;
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

        shiny::img(
          src = "ehe-www/banner_ehe.png",
          class = "ehe-banner"
        ),

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

            shiny::column(
              width = 4,

              shiny::selectInput(
                inputId = "municipio",
                label = "Municipio",
                choices = municipios$municipio,
                selected = municipios$municipio[1]
              )
            ),

            shiny::column(
              width = 2,

              shiny::selectInput(
                inputId = "anio",
                label = "A\u00f1o",
                choices = NULL
              )
            ),

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

          # Titulo: ocupa todo el ancho del panel
          shiny::h2(
            class = "ehe-result-title",
            shiny::textOutput("titulo_resultado")
          ),

          # Tabla a la izquierda y mapa a la derecha
          shiny::div(
            class = "ehe-results-layout",

            # Tabla y fuente
            shiny::div(
              class = "ehe-table",

              shiny::tableOutput("indicadores"),

              shiny::div(
                class = "ehe-source",
                shiny::textOutput("fuente")
              )
            ),

            # Mapa a la derecha
            shiny::div(
              class = "ehe-map",

              shiny::plotOutput(
                "mapa_municipio",
                height = "430px",
                width = "100%"
              )
            )
          )
        )
      ),


      # ========================================================
      # PIE
      # ========================================================

      shiny::div(
        class = "ehe-footer",

        shiny::HTML(
          "Direcci\u00f3n Provincial de Estad\u00edstica. Subsecretar\u00eda de Coordinaci\u00f3n Econ\u00f3mica y Estad\u00edstica. Ministerio de Econom\u00eda.Provincia de Buenos Aires"
        )
      )
    ),


    # ==========================================================
    # SERVER
    # ==========================================================

    server = function(input, output, session) {

      # --------------------------------------------------------
      # ACTUALIZAR A\u00f1OS SEGUN MUNICIPIO
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
      # BASE SELECCIONADA
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
            "No se encontr\u00f3 una base individual para la selecci\u00f3n."
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
      # INDICADORES
      # --------------------------------------------------------

      indicadores <- shiny::reactive({

        base <- base_seleccionada()

        shiny::req(
          input$tipo_indicador
        )

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

        } else {

          ehe_demografico(
            base = base
          )
        }
      })


      # --------------------------------------------------------
      # TITULO DE LOS RESULTADOS
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
      # MOSTRAR INDICADORES
      # --------------------------------------------------------

      output$indicadores <- shiny::renderTable({

        datos <- indicadores()

        datos |>
          dplyr::mutate(
            Porcentaje = paste0(
              format(
                round(Porcentaje, 1),
                decimal.mark = ",",
                nsmall = 1
              ),
              "%"
            )
          )
      })


      # --------------------------------------------------------
      # MAPA REACTIVO DEL MUNICIPIO SELECCIONADO
      # --------------------------------------------------------

      output$mapa_municipio <- shiny::renderPlot({

        shiny::req(input$municipio)

        clave <- normalizar_municipio(input$municipio)

        # Equivalencias entre EHE y el shapefile
        if (clave == "SAN MARTIN") {
          clave <- "GRAL. SAN MARTIN"
        }

        if (clave == "VIAMONTE") {
          clave <- "GRAL. VIAMONTE"
        }

        mapa <- municipios_shp |>
          dplyr::mutate(
            seleccionado = municipio_clave == clave
          )

        ggplot2::ggplot(mapa) +

          ggplot2::geom_sf(
            ggplot2::aes(fill = seleccionado),
            color = "white",
            linewidth = 0.2
          ) +

          ggplot2::scale_fill_manual(
            values = c(
              "FALSE" = "#e2e6e9",
              "TRUE" = "#168eb6"
            ),
            guide = "none"
          ) +

          ggplot2::theme_void() +

          ggplot2::theme(
            plot.background = ggplot2::element_rect(
              fill = "transparent",
              color = NA
            ),
            panel.background = ggplot2::element_rect(
              fill = "transparent",
              color = NA
            ),
            plot.margin = ggplot2::margin(0, 0, 0, 0)
          )

      }, bg = "transparent")


      # --------------------------------------------------------
      # FUENTE DE LOS DATOS
      # --------------------------------------------------------

      output$fuente <- shiny::renderText({
        "Fuente: Departamento de An\u00e1lisis de las Estad\u00edsticas Sociales a partir de la Encuesta de Hogares y Empleo Municipal (EHE-M)."
      })
    }
  )
}
