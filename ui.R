library(shiny)
library(shinythemes)

shinyUI(
  navbarPage(
    "Comparación de precios",
    theme = shinytheme("flatly"),
    tabPanel("Objetivo",
             column(5,
                    wellPanel(h1('En esta aplicación se hace un comparativo 
                                 de los precios de categorías selectas en Coppel, Famsa y Elektra.'),
                              align="justify"),
                              style = "background-color: white;"
                    ) # End Column
    ), # End TabPanel
    navbarMenu("Open Price",
               tabPanel("Tablas",
                        fluidPage(
                          column(3,
                                 textOutput("texto"),
                                 br(),
                                 selectInput("levantamiento", 
                                             "Elige el levantamiento",
                                             choices = c("Levantamiento 1", "Levantamiento 2"),
                                             selected = "Levantamiento 2"
                                 ), # End SelectInput
                                 wellPanel(style = "background-color: #2c3e50;",
                                           h3('¿Qué significa?',
                                              align="center",
                                              style = "color:#C2D1E0"),
                                           br(),
                                           h4("La tabla muestra el precio de apertura para los productos de cada una de las categorías.
                         Se puede elegir el producto que se quiere ver, y la fecha en la cual se obtuvieron los datos.",
                                              style = "color:#C2D1E0"),
                                           h4("Además, se tiene la opción de descargar los datos de cualquier fecha, producto y tienda.",
                                              style = "color:#C2D1E0")
                                 ), # End wellPanel
                                 wellPanel(
                                   h3("Selecciona las opciones para descargar los datos:"),
                                   checkboxGroupInput("opciones_descarga_tienda",
                                                      "Tiendas",
                                                      choices = list("Coppel",
                                                                     "Elektra",
                                                                     "Famsa"),
                                                      selected = list("Coppel",
                                                                      "Elektra",
                                                                      "Famsa"),
                                                      inline = T
                                   ),
                                   checkboxGroupInput("opciones_descarga_levantamiento",
                                                      "Levantamiento",
                                                      choices = list("Levantamiento 1",
                                                                     "Levantamiento 2"),
                                                      selected = "Levantamiento 2",
                                                      inline = T
                                   ),
                                   downloadButton('download_sel', 'Descargar selección')
                                 ) # end wellPanel
                          ), # End column
                          column(8,
                                 radioButtons('filtroProducto', 
                                              label = '',
                                              choices = list("Lavadoras" = "LAVADORA",
                                                             "Refrigeradores" = "REFRIGERADOR",
                                                             "Pantallas" = "PANTALLA",
                                                             "Estufas" = "ESTUFA",
                                                             "Colchones" = "COLCHON"),
                                              inline = T
                                 ),
                                 mainPanel(
                                   dataTableOutput("tabla_openprice")
                                 ) # End mainPanel
                          ) # End column
                        ) #end fluidPage
               ), #end TabPanel
               tabPanel("Gráficas",
                        fluidPage(
                          column(3,
                                 textOutput("texto2"),
                                 br(),
                                 selectInput("levantamiento_grafs", 
                                             "Elige el levantamiento",
                                             choices = c("Levantamiento 1", "Levantamiento 2"),
                                             selected = "Levantamiento 2"
                                 ), # End SelectInput
                                 wellPanel(style = "background-color: #2c3e50;",
                                           h3('¿Qué significa?',
                                              align="center",
                                              style = "color:#C2D1E0"),
                                           br(),
                                           h4("La tabla muestra el precio de apertura para los productos de cada una de las categorías.
                                              Se puede elegir el producto que se quiere ver, y la fecha en la cual se obtuvieron los datos.",
                                              style = "color:#C2D1E0"),
                                           h4("Además, se tiene la opción de descargar los datos de cualquier fecha, producto y tienda.",
                                              style = "color:#C2D1E0")
                                 ) # End wellPanel
                                 ), # End column
                          column(8,
                                 radioButtons('filtroProducto_grafs', 
                                              label = '',
                                              choices = list("Lavadoras" = "LAVADORA",
                                                             "Refrigeradores" = "REFRIGERADOR",
                                                             "Pantallas" = "PANTALLA",
                                                             "Estufas" = "ESTUFA",
                                                             "Colchones" = "COLCHON"),
                                              inline = T
                                 ),
                                 mainPanel(
                                   plotlyOutput("grafica_openprice",
                                                width = "1400px",
                                                height = "750px"
                                                  )
                                 ) # End mainPanel
                          ) # End column
                        ) #end fluidPage
               ) #end TabPanel
    ) # end navBarmenu
  ) # end navBarPage
) # end ShinyUI


