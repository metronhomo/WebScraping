library(googleVis)
library(shiny)

shinyServer(function(input, output) {
  
  #################################################################################
  ######### Tabla
  #################################################################################
  
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
  
  data_tabla_openprice <- reactive({
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
  
  
  output$tabla_openprice <- renderDataTable(
    data_tabla_openprice(),
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
  
  #################################################################################
  ######### Gráfica
  #################################################################################
  
  data_graf_openprice <- reactive({
    prod <- input$filtroProducto_grafs
    lev <- as.numeric(unlist(stri_extract_all(input$levantamiento_grafs, regex = "[0-9]+")))
    if(prod == "COLCHON"){
      productos_filter <- productos %>% 
        filter(Producto == prod &
                 Levantamiento == lev) %>% 
        mutate(Precio = as.numeric(Precio))
    } else {
      productos_filter <- productos %>% 
        filter(Producto == prod &
                 Levantamiento == lev) %>% 
        mutate(Tam = as.numeric(stri_extract(regex = "[0-9]+\\.?[0-9]*", str = Tamaño)),
               Precio = as.numeric(Precio))
    }
    return(list(prod, productos_filter))
  })  
  
  output$grafica_openprice <- renderPlot({
    prod <- data_graf_openprice()[[1]]
    df <- data_graf_openprice()[[2]]
    if(prod == "COLCHON") {
      gg <-  df %>% 
        ggplot(aes(x = Tienda, y = Precio, color = Tienda)) 
    } else {
      gg <- df %>% 
        ggplot(aes(x = Tienda, y = Precio, color = Tienda, size = Tam))
    }
    GG <- gg+ 
      geom_jitter() +
      scale_size_continuous(range = c(1, 4))
    return(GG)
  })
  
})




