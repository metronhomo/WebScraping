library(googleVis)
library(shiny)

shinyServer(function(input, output) {
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
    return(a)
  })
  
  output$openprice <- renderGvis({
    gvisTable(data_openprice())         
  })
})