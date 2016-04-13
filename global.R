library(dplyr)
library(tidyr)
library(knitr)
library(stringi)
library(plotly)
library(scales)

source("helpers.R")

productos <- readRDS("data/productos_ola_01.RDS") %>% 
  rbind(readRDS("data/productos_ola_02.RDS")) %>% 
  unique()
