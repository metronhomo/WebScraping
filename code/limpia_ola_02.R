coppel <- readRDS("data/coppel_20160410.RDS") %>% 
  mutate(Fecha_Levantamiento = as.Date("2016-04-06"),
         Levantamiento = 2)

famsa <- readRDS("data/famsa_20160410.RDS") %>% 
  mutate(Fecha_Levantamiento = as.Date("2016-04-06"),
         Levantamiento = 2,
         Tienda = "Famsa") %>% 
  select(Nombre, Precio, Marca, Producto, Tamaño, Tienda, Levantamiento, Fecha_Levantamiento)

ekt <- readRDS("data/elektra_20160410.RDS") %>% 
  mutate(Fecha_Levantamiento = as.Date("2016-04-06")) %>% 
  filter(Producto %in% c("PANTALLA", "REFRIGERADOR", "LAVADORA", "ESTUFA", "COLCHON"))

productos_ola_02 <- rbind(coppel, ekt, famsa)


ix <- productos_ola_02$Producto == "COLCH\xd3N"
productos_ola_02$Producto[ix] = "COLCHON"

saveRDS(productos_ola_02, "data/productos_ola_02.RDS")