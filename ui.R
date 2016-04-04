library(shiny)
library(shinythemes)

shinyUI(
  navbarPage(
    "Comparación de precios",
    theme = shinytheme("flatly"),
    tabPanel("Open Price",
             textOutput("texto"),
             radioButtons('filtroProducto', 
                     label = '',
                     choices = unique(productos$Producto),
                     selected = unique(productos$Producto)[1],
                     inline = T
                     ),
             mainPanel(
               dataTableOutput("openprice"),
               downloadButton('download_EKT', 'Descargar datos de Elektra'),
               downloadButton('download_Coppel', 'Descargar datos de Coppel'),
               downloadButton('download_Famsa', 'Descargar datos de Famsa')
             )
    )
    
  )
)