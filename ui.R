library(shiny)
library(shinythemes)

shinyUI(
  navbarPage(
    "Comparación de precios",
    theme = shinytheme("flatly"),
    tabPanel("Objetivo",
             column(1),
             column(5,
                    imageOutput('imagen1')
             ), # End Column
             column(5,
                    wellPanel(h1('La aplicación tiene como propósito compilar, 
                                 procesar y presentar la información más reciente de la oferta 
                                 de productos online para ayudar a los tomadores de decisiones 
                                 con la información más reciente y los informes más concretos posibles. 
                                 Se consideran las principales cadenas y la información se actualiza de forma 
                                 semanal.',
                                 align="justify"),
                              style = "background-color: white;")
             ), # End Column
             column(1)
    ), # End TabPanel
    navbarMenu("Comparativo de precios",
               tabPanel("Open Price",
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
               tabPanel("Distribución de la oferta",
                        fluidPage(
                          column(3,
                                 textOutput("texto2"),
                                 br(),
                                 selectInput("levantamiento_grafs", 
                                             "Elige el levantamiento",
                                             choices = c("Levantamiento 1", "Levantamiento 2"),
                                             selected = "Levantamiento 2"
                                 ), # End SelectInput
                                 sliderInput("rango_precios_graf",
                                             "Elige el rango de precios",
                                             value = c(0, 200000),
                                             min = 0,
                                             max = 200000,
                                             ticks = F
                                 ), # End sliderInput
                                 wellPanel(style = "background-color: #2c3e50;",
                                           h3('¿Qué significa?',
                                              align="center",
                                              style = "color:#C2D1E0"),
                                           br(),
                                           h4("La gráfica muestra el precio de cada producto en las distintas tiendas.
                                              Al poner el cursor en un punto, se muestra el detalle de ese producto.
                                              Las líneas horizontales representan la mediana de cada tienda.",
                                              style = "color:#C2D1E0")
                                 ) # End wellPanel
                          ), # End column
                          column(8,
                                 radioButtons('filtroProducto_grafs', 
                                              label = 'Elige el producto que quieres ver',
                                              choices = list("Lavadoras" = "LAVADORA",
                                                             "Refrigeradores" = "REFRIGERADOR",
                                                             "Pantallas" = "PANTALLA",
                                                             "Estufas" = "ESTUFA",
                                                             "Colchones" = "COLCHON"),
                                              inline = T
                                 ),
                                 checkboxGroupInput("filtro_tamano_grafs",
                                                    "Elige los tamaños de producto que quieres ver",
                                                    # Las choices se actualizan solas dependiendo del producto que se
                                                    # seleccione. El código está en server.R
                                                    choices = c("1"),
                                                    inline = T
                                 ), # End checkboxGroupInput
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


