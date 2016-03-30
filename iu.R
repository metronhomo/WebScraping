



# library(stringi)
# stri_enc_mark(base_Famsa$Nombre_producto)
# all(stri_enc_isutf8(base_Famsa$Nombre_producto))
# 
# # cambiar el encoding de las bases
# base_Famsa$Caracteristicas <- stri_replace_all_fixed(base_Famsa$Caracteristicas, pattern = "Ã¡", replacement = "á") %>% 
#                 stri_replace_all_fixed(., pattern = "Ã©", replacement = "é") %>%
#                 stri_replace_all_fixed(., pattern = "Ãƒ¡", replacement = "á") %>%
#                 stri_replace_all_fixed(., pattern = "Ãƒ³", replacement = "ó") %>%
#                 stri_replace_all_fixed(., pattern = "Ãƒ±", replacement = "ñ") %>%
#                 stri_replace_all_fixed(., pattern = "Ãƒ©", replacement = "é") %>%
#                 stri_replace_all_fixed(., pattern = "Ã³", replacement = "ó") %>% 
#                 stri_replace_all_fixed(., pattern = "Ã±", replacement = "ñ") %>%
#                 stri_replace_all_fixed(., pattern = "Ãº", replacement = "ú")%>%
#                 stri_replace_all_fixed(., pattern = "Â", replacement = "")
#                 # stri_replace_all_fixed(., pattern = "Ã", replacement = "í")
# 
# base_Famsa$Nombre_producto <- stri_replace_all_fixed(base_Famsa$Nombre_producto, pattern = "Ã", replacement = "í")
# base_Famsa$Caracteristicas <- stri_replace_all_fixed(base_Famsa$Caracteristicas, pattern = "Ã", replacement = "í")
# 
# 
# 
# base_Famsa$Caracteristicas <- stri_replace_all_fixed(base_Famsa$Caracteristicas, pattern = "(íƒ¡)", replacement = "k") %>% 
#   stri_replace_all_fixed(., pattern = "Ãƒ³", replacement = "ó") %>%
#   stri_replace_all_fixed(., pattern = "Ãƒ±", replacement = "ñ") %>%
#   stri_replace_all_fixed(., pattern = "Ãƒ©", replacement = "é")
#   # stri_replace_all_fixed(., pattern = "Â", replacement = "")



base_coppel <- readRDS("./base_coppel_1.RDS")
base_Famsa <- readRDS("./base_Famsa_1.RDS")
base_ekt <- readRDS("./base_ekt_1.RDS")




###############quitar todos los casos que no sirven
base_coppel <- base_coppel[base_coppel$Nombre != "Error",]
base_ekt <- base_ekt[base_ekt$Precio_list != "Error",]
base_Famsa <- base_Famsa[base_Famsa$Nombre_producto != "Error",]







###########para poder filtrar por los articulos que solo queremos que son Refriferadores, pantallas
# y lavadoras





producto<-which(str_locate(base_Famsa$Nombre_producto,'Pantalla')[,1]==1)
producto2<-which(str_locate(base_Famsa$Nombre_producto,'Refrigerador')[,1]==1)
producto3<-which(str_locate(base_Famsa$Nombre_producto,'Lavadora')[,1]==1)
producto4<-which(str_locate(base_coppel$Nombre,'Pantalla')[,1]==1)
producto5<-which(str_locate(base_coppel$Nombre,'Refrigerador')[,1]==1)
producto6<-which(str_locate(base_coppel$Nombre,'Lavadora')[,1]==1)



#bases filtradas por articulos

baseSub <- base_coppel[c(producto,producto2,producto3),]
baseSub_coppel<-base_coppel[c(producto4,producto5,producto6),]

extraePulgadas<-function(cadena){
  require(stringr)
  a<-tail(strsplit(cadena,split=" ")[[1]],1)
  b<-gsub('([0-9])([[:alpha:]])', '\\1 \\2', a)
  c<-gsub('([[:alpha:]])([0-9])', '\\1 \\2', b)
  d<-str_extract_all(c,"\\(?[0-9,.]+\\)?")[[1]][1]
  return(d)
}



#funcion para extraer tamaño de producto

extrae_tamano <- function(cadena){
  
  hu <- grep("Pantalla",cadena, ignore.case = T)
  ho <- grep("Refrigerador",cadena,ignore.case = T)
  he <- grep("Lavadora",cadena, ignore.case = T)
  
  if (length(hu) !=0){
    ju <- as.character(unlist(stri_extract_all(cadena, regex = '([0-9]+")'))) %>% 
      stri_extract_all(regex = "[0-9]+")
    ja <- ifelse(is.na(ju), NA, paste(ju, "pulgadas"))
  }
  
  if (length(ho) !=0){
    
    ju <- stri_extract_all(regex = '\\d+( ?)p', 
                           str = cadena, 
                           case_insensitive = TRUE) %>% 
      stri_extract_all(regex = "[0-9]+")
    ja <- ifelse(is.na(ju), NA, paste(ju, "pies"))
  }
  
  if (length(he) !=0){
    ju <- stri_extract_all(regex = '\\d+( ?)kg', 
                           str = cadena, 
                           case_insensitive = TRUE) %>% 
      stri_extract_all(regex = "[0-9]+")
    ja <- ifelse(is.na(ju), NA, paste(ju, "Kg"))
  }
  
  return(ja)
}




extrae_tamano(art)
#para extraer tipo de producto

tipo_producto <- function(art){
  hu <- grep("Pantalla",art, fixed = T)
  ho <- grep("Refrigerador",art, fixed = T)
  he <- grep("Lavadora",art, fixed = T)
  
  if(length(hu) != 0){
    pr <- "Pantalla"
  } 
  if(length(ho) != 0){
    pr <- "Refrigerador"
  }
  if(length(he) != 0){
    pr <- "Lavadora"
  }

  return(pr)
}


tipo_producto(art)



# para extraer marca 




marcas <- list("Acer","Acros","Advio","Amana", "White-Westinghouse","Quasar",
           "Aoc","Apple","Armada","Aspix","Asus","SEIKI",
           "Atvio","Black_&_Decker","Blackberry","Blue_Point","Bosch", "TCL",
           "Cinsa","Coby","Compaq","Continental", "Crolls","Daewoo",
           "Delher","Dell","Easy","EKT","Electrolux","E-Machines",
           "Emerson","Extranjero","Flamineta","Fraga", "Frigidaire",
           "Game_Boy","Gate_Way", "Ge","Goldstar", "Hisense",
           "Hitachi","Hoover","Hp","Ibm","Iem","Joinet","Kelvinator","Kenmore",
           "Kenwood","Koblenz","Lanix","Lenovo","Lg","Mabe","Majestic","Maytag",
           "México","Mitsui","Mitzu","Nintendo","Nintendo Revolution / Wii",
           "Panasonic","Philco","Philips", "Phillips","Polaroid","Pioneer","RCA","Samsung","Sanyo",
           "Sharp","Singer","Sony","Supermatic","Techpad","Toshiba", "Westing House",
           "Whirlpool","X-Box")

marcas2 <- toupper(marcas)




#extraer marca 

extrae_marca <- function(cadena,marcas){
  
  kl <- as.list(strsplit(cadena,  " ")[[1]]) %>% 
    toupper()
  m <- gsub("[^A-Za-z0-9ñÑ-]", "", kl)
  vb <- intersect(marcas, m)
  if(length(vb) == 0) vb = NA  
    return(vb)

}

extrae_marca(art, marca)




#para el data frame de coppel
baseSub_coppel$marca <- sapply(baseSub_coppel$Nombre,
                               function(cadena) extrae_marca(cadena, marcas2))
baseSub_coppel$Producto <- sapply(baseSub_coppel$Nombre,
                               function(art) tipo_producto(art))
baseSub_coppel$tamano <- sapply(baseSub_coppel$Nombre,
                                  function(cadena) extrae_tamano(cadena))



#para el data frame de Famsa
baseSub$marca <- sapply(baseSub$Nombre_producto,
                               function(cadena) extrae_marca(cadena, marcas2))
baseSub$Producto <- sapply(baseSub$Nombre_producto,
                                  function(art) tipo_producto(art))
baseSub$tamano <- sapply(baseSub$Nombre_producto,
                                function(cadena) extrae_tamano(cadena))





# write.csv(base_Famsa,"base_Famsa.csv")
# write.csv(base_coppel,"base_Coppel.csv")
# write.csv(base_ekt,"base_ekt.csv")
