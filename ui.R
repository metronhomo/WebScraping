library(shiny)
library(shinythemes)

shinyUI(
  navbarPage(
    "Comparación de precios",
    theme = shinytheme("flatly"),
    tabPanel("Open Price",
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
                      h4("La tabla muestra el precio de apertura más bajo para cada tienda y para cada clasificación de producto.
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
                      dataTableOutput("openprice")
                      
                    ) # End mainPanel
             ) # End column
    )
    
  )
)