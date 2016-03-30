library(shiny)
library(shinythemes)

shinyUI(
  navbarPage(
    "Comparación de precios",
    theme = shinytheme("flatly"),
    tabPanel("Open Price",
             
             radioButtons('filtroProducto', 
                     label = '',
                     choices = unique(productos$Producto),
                     selected = unique(productos$Producto)[1],
                     inline = T
                     ),
             mainPanel(
               htmlOutput("openprice")
             )
    )
    
  )
)