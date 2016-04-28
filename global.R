library(dplyr)
library(tidyr)
library(knitr)
library(stringi)
library(plotly)
library(scales)

source("helpers.R")

olas_folder <- "data/olas/"
archivos <- paste0(olas_folder, list.files(olas_folder))


productos <- lapply(archivos, function(x){
  readRDS(x) %>% 
    unique(.) %>% 
    mutate(Precio = as.numeric(Precio))
}) %>% rbind_all()


levantamientos <- paste("Levantamiento", unique(productos$Levantamiento))

productos_list <- list("Lavadoras" = "LAVADORA",
                       "Refrigeradores" = "REFRIGERADOR",
                       "Pantallas" = "PANTALLA",
                       "Estufas" = "ESTUFA",
                       "Colchones" = "COLCHON")
