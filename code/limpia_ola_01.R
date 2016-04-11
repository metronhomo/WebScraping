library(dplyr)

productos_ola_01 <- readRDS("data/old/productos_ola_01_original.RDS") %>% 
  rename(Tamaño = Tamanio) %>% 
  mutate(Levantamiento = 1,
         Fecha_Levantamiento = as.Date("2016-03-28"),
         Producto = toupper(Producto))

saveRDS("data/productos_ola_01.RDS")