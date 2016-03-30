library(dplyr)
library(rvest)
library(stringi)
library(stringr)
library(gsubfn)

productos_ekt <- readRDS("../data/productos_ekt.RDS") %>% 
  select(SKU, Descripcion)

scrapea_modelo_ekt <- function(sku, verbose = T){
  pag_url <- paste("http://www.elektra.com.mx/Items?sku=",sku,sep="")
  if(verbose) cat("Leyendo ", pag_url, "\n")
  pag <- try(read_html(pag_url, encoding = "UTF-8"), silent=T)
  if(class(pag)[1] != "try-error"){
    if(verbose) cat("Se pudo leer ", pag_url, "\n")
    tabla1 <- pag %>%
      html_nodes(xpath="/html/body/script[18]")%>%
      html_text()
    if(length(tabla1) != 0){
      tabla2 <- stri_replace_all(regex = '\n|\r',
                                 replacement ='',str = tabla1)
      tabla3 <- stri_replace_all(regex = '(.*?)("ListPrice\\":)([0-9]+)(.*?"Name\\":)(.*?,)(.*?"Price\\":)([0-9]+)(.*?"Price2\\":)([0-9]+)(.*?")(SerialNumber\\":)(.*?\\",)(.*?$)',
                                 replacement ='$3|$5|$7|$9|$12',str = tabla2)
      #para saber si existe la parte de precios de credito
      credito <- grepl(pattern = '\"TimelyPaymentPrice\"',tabla2)
      if(credito){
        tabla_cre <- stri_replace_all(regex = '(.*?"TimelyPaymentPrice\\":)([0-9]+)(.*?")(.*?"TimelyPaymentPrice\\":)([0-9]+)(.*?")(.*?"TimelyPaymentPrice\\":)([0-9]+)(.*?")(.*?"TimelyPaymentPrice\\":)([0-9]+)(.*?")(.*?$)',
                                      replacement ='$2|$5|$8|$11',str = tabla2) 
        tabla_cre2 <- gsub(pattern =',|"', replacement="",x=tabla_cre,perl=T)
        tabla_cre3 <- unlist(strsplit(tabla_cre,split = "|",fixed=T))
      }
      else{ 
        if(verbose) cat("La página no existe\n")
        tabla_cre3 <- c("Error","Error","Error","Error")
      }
      
      tabla4 <- gsub(pattern ='([0-9]+)(\\\\\")', 
                    replacement = "\\1 pulgadas", 
                    x = tabla3, 
                    perl = T)
      tabla5 <- gsub(pattern ='("|,|  +)', replacement="", x = tabla4, perl = T)
      
      columnas <- c(sku, unlist(strsplit(tabla5, split = "|",fixed = T)))
      columnas2 <- c(columnas, tabla_cre3)
      if(verbose) cat("Terminó ", pag_url, "\n")
    }else{
      columnas2 <- c(sku,"Error","Error","Error","Error","Error","Error","Error")
    }
    
  }else{
    if(verbose) cat("No se pudo leer ", pag_url, "\n")
    columnas2 <- c(sku,"Error","Error","Error","Error","Error","Error","Error")
  }
  return(columnas2)
}

x <- data.frame(t(sapply(1:nrow(productos_ekt), 
                            function(i){
                              cat("\n", i, " ")
                              scrapea_modelo_ekt(productos_ekt$SKU[i])
                              })
                     )
                   )

productos_ekt_detalle <- x %>% 
  select(SKU =X1,
         Nombre = X3,
         Modelo = X6,
         Precio_list = X2,
         Precio_rebaja = X4,
         Precio2 = X5,
         cre_52_sem = X7,
         cre_65_sem = X8,
         cre_78_sem = X9,
         cre_102_sem = X10) %>% 
  mutate(SKU = as.numeric(as.character(SKU))) %>% 
  left_join(productos_ekt)

saveRDS(productos_ekt_detalle, "../data/productos_ekt_detalle.RDS")


