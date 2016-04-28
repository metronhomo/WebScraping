library(dplyr)
library(tidyr)
library(knitr)
library(stringi)
library(plotly)
library(scales)

source("helpers.R")

# Indica folder donde están los archivos rds con los datos de los levantamientos
olas_folder <- "data/olas"

# Guarda los archivos en un vector
archivos <- list.files(olas_folder, full.names = T)

# Lee todos los archivos y luego los convierte en un solo dataframe
productos <- lapply(archivos, function(x){
  readRDS(x) %>% 
    unique(.) %>% 
    mutate(Precio = as.numeric(as.character(Precio)))
}) %>% rbind_all()

# Encuentra qué levantamientos son válidos para todas las tiendas.
# Por ejemplo, si en el levantamiento 3 no está incluida Famsa,
# entonces el vector levantamientos_num no contendrá al levantamiento 3
levantamientos_num <- productos %>% 
  group_by(Levantamiento, Tienda) %>% 
  tally() %>% 
  select(Levantamiento) %>% 
  tally() %>% 
  filter(n == 3) %>% 
  .$Levantamiento

# Filtra los productos para que solo tengan los levantamientos completos
productos <- productos %>% 
  filter(Levantamiento %in% levantamientos_num)

# Vector que aparecerá en las opciones del Shiny
levantamientos <- paste("Levantamiento", sort(levantamientos_num, 
                                               decreasing = F))

productos_list <- unique(productos$Producto)
