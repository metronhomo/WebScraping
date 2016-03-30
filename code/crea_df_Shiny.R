library(stringi)
library(stringr)
library(dplyr)

source("utils.R")

df_coppel <- readRDS("./productos_coppel_detalle.RDS") %>% 
  filter(Nombre != "Error")
df_famsa <- readRDS("./productos_famsa_detalle.RDS") %>% 
  filter(Nombre_producto != "Error")
df_ekt <- readRDS("./productos_ekt_detalle.RDS")


producto <- which(str_locate(df_famsa$Nombre_producto,'Pantalla')[,1]==1)
producto2 <- which(str_locate(df_famsa$Nombre_producto,'Refrigerador')[,1]==1)
producto3 <- which(str_locate(df_famsa$Nombre_producto,'Lavadora')[,1]==1)
producto4 <- which(str_locate(df_coppel$Nombre,'Pantalla')[,1]==1)
producto5 <- which(str_locate(df_coppel$Nombre,'Refrigerador')[,1]==1)
producto6 <- which(str_locate(df_coppel$Nombre,'Lavadora')[,1]==1)

#df's filtrados por articulos
df_Sub_famsa <- df_famsa[c(producto,producto2,producto3),]
df_Sub_coppel <- df_coppel[c(producto4,producto5,producto6),]

#para el data frame de coppel
df_Sub_coppel$Marca <- sapply(df_Sub_coppel$Nombre,
                              function(cadena) extrae_marca(cadena, marcas))
df_Sub_coppel$Producto <- sapply(df_Sub_coppel$Nombre,
                                 function(art) tipo_producto(art))
df_Sub_coppel$Tamanio <- sapply(df_Sub_coppel$Nombre,
                               function(cadena) extrae_tamano(cadena))

#para el data frame de famsa
df_Sub_famsa$Marca <- sapply(df_Sub_famsa$Nombre_producto,
                       function(cadena) extrae_marca(cadena, marcas))
df_Sub_famsa$Producto <- sapply(df_Sub_famsa$Nombre_producto,
                          function(art) tipo_producto(art))
df_Sub_famsa$Tamanio <- sapply(df_Sub_famsa$Nombre_producto,
                        function(cadena) extrae_tamano(cadena))

#para el data frame de ekt
df_ekt$Marca <- sapply(df_ekt$Nombre,
                             function(cadena) extrae_marca(cadena, marcas))
df_ekt$Producto <- sapply(df_ekt$Nombre,
                                function(art) tipo_producto(art))
df_ekt$Tamanio <- unlist(sapply(df_ekt$Nombre,
                              function(cadena) extrae_tamano_ekt(cadena)))


x1 <- df_Sub_famsa %>% 
  select(Nombre = Nombre_producto,
         Precio = precio_contado,
         Marca,
         Producto,
         Tamanio) %>% 
  filter(complete.cases(.)) %>% 
  mutate(Tienda = "Famsa")

x2 <- df_Sub_coppel %>% 
  select(Nombre,
         Precio = Price_Contado,
         Marca,
         Producto,
         Tamanio) %>% 
  filter(complete.cases(.)) %>% 
  mutate(Tienda = "Coppel")

x3 <- df_ekt %>% 
  select(Nombre,
         Precio = Precio_rebaja,
         Marca,
         Producto,
         Tamanio) %>% 
  filter(complete.cases(.)) %>% 
  mutate(Tienda = "Elektra")

df_final <- rbind(x1, x2, x3)

saveRDS(df_final, "../data/productos_tiendas.RDS")



