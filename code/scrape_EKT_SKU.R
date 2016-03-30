library(rvest)
library(stringi)
library(stringr)
library(dplyr)
library(gsubfn)

unwanted_array <- list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                       'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                       'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                       'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                       'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )



scrapea <- function(pag_url){
  cat("\n", pag_url)
  error_pagina <- T
  i <- 0
  while(error_pagina && i < 100) {
    i = i + 1
    pag <- try(read_html(pag_url, encoding = "UTF-8"), silent = T)
    error_pagina = (class(pag) == "try-error" | grepl("Departamento desconocido", pag, fixed = T))[1]
    cat("\nIntento ", i, "\n")
  }
  
  if(!error_pagina){
    cat("Se descargó la página", pag_url, "con éxito")
    
    # Extrae texto del objeto descargado por rvest
    tabla1 <- pag %>%
      html_nodes(xpath="/html/body/script[18]")%>%
      html_text()
    
    # Obtiene Descripción, nombre, precio y SKU
    tabla2 <- stri_replace_all(regex = '(.*?)("Description\\":\\")(.*?\\",)(.*?)("Name\\":\\")(.*?\\",)(.*?)(\"Price\\\":)([0-9]+)(.*?)(\"Sku\\\":)([0-9]+)',
                               replacement ='$3|$6|$9|$12\n',str = tabla1)
    
    # Convierte en vector el caracter "tabla2"
    aux <- unlist(strsplit(tabla2,split = "\n"))
    
    # Filtra las entradas que corresponden a productos
    aux2 <- aux[grepl(pattern = '(.*)?\\|(.*)?\\|[0-9]+\\|[0-9]+',
                      x = aux,
                      perl = T)]
    
    # Hace split a | para convertirlo en lista
    aux3 <- strsplit(aux2, split = '\\|')
    
    # Convierte la lista a dataframe
    tabla3 <- data.frame(matrix(unlist(aux3), ncol = 4, byrow=T), stringsAsFactors = F)
    
    # Da formato y quita duplicados
    df <- tabla3 %>% 
      select(SKU = X4,
             Nombre = X2,
             Precio = X3,
             Descripcion = X1) %>% 
      mutate(SKU = as.numeric(str_replace_all(SKU, "[^0-9]", "")), #Quita caracteres que no son número en el SKU
             Nombre1 = str_replace_all(Nombre, '([0-9]+)(\\\\\")', "\\1 pulgadas"), #Sustituye las comillas dobles por la pañabra pulgadas
             Nombre2 = str_replace_all(Nombre1,"  +", " "), # Quita espacios dobles
             Nombre3 = str_replace_all(Nombre2, "[^a-zA-Z0-9\\s]|^ | $",""), # Se queda solo son caracteres alfanuméricos y espacios
             Nombre4 = str_replace_all(Nombre3, "^ | $", ""), # Quita espacios al inicio y al final
             Precio = as.numeric(as.character(Precio)),
             Descripcion1 = iconv(Descripcion, to='ASCII//TRANSLIT'),
             Descripcion2 = str_replace_all(Descripcion1, "( )( +)", " "),
             Descripcion3 = str_replace_all(Descripcion2, "[^a-zA-Z\\s]", ""),
             Descripcion4 = str_replace_all(Descripcion3, "^ | $", "")) %>% 
      select(SKU,
             Nombre = Nombre4,
             Precio,
             Descripcion = Descripcion4) %>% 
      filter(!duplicated(SKU))
    cat("\nTerminó la página ", pag_url)
    
  } else {
    cat("\nHubo un error con la página")
    df <- data.frame(SKU = 'Error', Nombre = "Error", Precio = "Error", Descripcion = "Error")
  }
  return(df)
}


# URLs de pantallas, refrigeradores y lavadoras
URLS <- c("http://www.elektra.com.mx/Departments?fldc=11&dc=110108",
          "http://www.elektra.com.mx/Departments?fldc=13&dc=1301",
          "http://www.elektra.com.mx/Departments?fldc=13&dc=1304")

df_productos <- lapply(URLS, scrapea) %>% 
  rbind_all()

saveRDS(df_productos, "productos_ekt.RDS")




