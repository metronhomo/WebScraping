library(googleVis)
library(shiny)

shinyServer(function(input, output) {
  
  fecha_lev <- reactive({
    lev <- as.numeric(unlist(stri_extract_all(input$levantamiento, regex = "[0-9]+")))
    filtrada <- productos %>% 
      filter(Levantamiento == lev) %>% 
      select(Fecha_Levantamiento) %>% 
      unique() %>% 
      sort()
    fecha <- as.character(filtrada[1,1])
    return(fecha)
  })
  
  output$texto <- renderText({
    paste("Fecha de ejecución del robot:", fecha_lev())})
  
  data_openprice <- reactive({
    prod <- input$filtroProducto
    lev <- as.numeric(unlist(stri_extract_all(input$levantamiento, regex = "[0-9]+")))
    productos_filter <- productos %>% 
      filter(Producto == prod &
               Levantamiento == lev)
    a <- productos_filter %>% 
      group_by(Tienda, Tamaño) %>% 
      slice(which.min(Precio)) %>% 
      ungroup() %>% 
      mutate(Precio_Marca = paste0(Marca, ": $", Precio)) %>% 
      select(Tienda, Tamaño, Precio_Marca) %>% 
      spread(Tienda, Precio_Marca, fill = '-') %>% 
      mutate(Tamaño2 = as.numeric(gsub("[^0-9]+", "", Tamaño))) %>% 
      arrange(Tamaño2) %>% 
      select(-Tamaño2)
    return(a)
  })
  

  output$openprice <- renderDataTable(
    data_openprice(),
    options = list(pageLength = 150)
  )
  
  dataset_Input <- reactive({
    descarga_tienda <- unlist(input$opciones_descarga_tienda)
    descarga_lev <- as.numeric(unlist(stri_extract_all(input$opciones_descarga_levantamiento, regex = "[0-9]+")))
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
          filter(Tienda == "Elektra" &
                   Levantamiento %in% descarga_lev) %>% 
          select(-Tienda)
        write.csv(productos_ekt, file = "Elektra.csv")
      }
      
      if(sum(grepl("Famsa", descarga_tienda)) > 0){
        productos_famsa = productos %>% 
          filter(Tienda == "Famsa" &
                   Levantamiento %in% descarga_lev) %>% 
          select(-Tienda)
        write.csv(productos_famsa, file = "Famsa.csv")
      }
      
      if(sum(grepl("Coppel", descarga_tienda)) > 0){
        productos_coppel = productos %>% 
          filter(Tienda == "Coppel" &
                   Levantamiento %in% descarga_lev) %>% 
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




