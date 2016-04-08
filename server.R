library(googleVis)
library(shiny)

shinyServer(function(input, output) {
  output$texto <- renderText({"Última ejecución del robot: 28 de marzo de 2016"})
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
  
  #   output$openprice <- renderGvis({
  #     gvisTable(data_openprice())
  #   })
  
  #   output$openprice <- renderTable({
  #     data_openprice()
  #   })
  
  output$openprice <- renderDataTable(
    data_openprice(),
    options = list(pageLength = 150)
  )
  
  dataset_Input <- reactive({
    descarga_tienda <- unlist(input$opciones_descarga_tienda)
    descarga_lev <- unlist(input$opciones_descarga_levantamiento)
    return(list(tienda = descarga_tienda,
                lev = descarga_lev))
  })
  
  output$download_sel <- downloadHandler(
    filename = function() {
      paste('precios-', Sys.Date(), '.zip', sep='')
    },
    content = function(fname) {
      descarga_tienda <- dataset_Input()[[1]]
      descarga_lev <- dataset_Input()[[2]]
      
      setwd(tempdir())
      
      if(sum(grepl("Elektra", descarga_tienda)) > 0){
        productos_ekt = productos %>% 
          filter(Tienda == "Elektra") %>% 
          select(-Tienda)
        write.csv(productos_ekt, file = "Elektra.csv")
      }
      
      if(sum(grepl("Famsa", descarga_tienda)) > 0){
        productos_famsa = productos %>% 
          filter(Tienda == "Famsa") %>% 
          select(-Tienda)
        write.csv(productos_famsa, file = "Famsa.csv")
      }
      
      if(sum(grepl("Coppel", descarga_tienda)) > 0){
        productos_coppel = productos %>% 
          filter(Tienda == "Coppel") %>% 
          select(-Tienda)
        write.csv(productos_coppel, file = "Coppel.csv")
      }
      
      zip(zipfile = fname, files = paste0(descarga_tienda, ".csv"))
      if(file.exists(paste0(fname, ".zip"))) file.rename(paste0(fname, ".zip"), fname)
    },
    contentType = "application/zip"
  )
  
  output$download_EKT <- downloadHandler(
    filename = function() {
      paste('EKT-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      productos = productos %>% 
        filter(Tienda == "Elektra") %>% 
        select(-Tienda)
      write.csv(productos, con)
    }
  )
  
  output$download_Famsa <- downloadHandler(
    filename = function() {
      paste('Famsa-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      productos = productos %>% 
        filter(Tienda == "Famsa") %>% 
        select(-Tienda)
      write.csv(productos, con)
    }
  )
  
  output$download_Coppel <- downloadHandler(
    filename = function() {
      paste('Coppel-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      productos = productos %>% 
        filter(Tienda == "Coppel") %>% 
        select(-Tienda)
      write.csv(productos, con)
    }
  )
  
})




