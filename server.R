library(googleVis)
library(shiny)

shinyServer(function(input, output) {
  output$texto <- renderText({"Precios actualizados el 28 de marzo de 2016"})
  data_openprice <- reactive({
    prod <- input$filtroProducto
    productos_filter <- productos %>% 
      filter(Producto == prod)
    a <- productos_filter %>% 
      group_by(Tienda, Tamanio) %>% 
      slice(which.min(Precio)) %>% 
      ungroup() %>% 
      mutate(Precio_Marca = paste0(Marca, ": $", Precio)) %>% 
      select(Tienda, Tamanio, Precio_Marca) %>% 
      spread(Tienda, Precio_Marca, fill = '-') %>% 
      mutate(Tamanio2 = as.numeric(gsub("[^0-9]+", "", Tamanio))) %>% 
      arrange(Tamanio2) %>% 
      select(-Tamanio2)
    cat(prod, "\n\n")
    return(a)
  })
  
#   output$openprice <- renderGvis({
#     cat("Va a imprimir\n")
#     gvisTable(data_openprice())
#   })
  
  output$openprice <- renderTable({
    cat("Va a imprimir\n")
    data_openprice()
  })
})