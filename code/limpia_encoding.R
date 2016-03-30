library(stringi)
library(dplyr)


df_coppel <- readRDS("./base_coppel_1.RDS") %>% 
  filter(Nombre != "Error")

df_famsa <- readRDS("./base_Famsa_1.RDS") %>% 
  filter(Nombre_producto != "Error")

# cambiar el encoding de los df's
df_famsa$Caracteristicas <- stri_replace_all_fixed(df_famsa$Caracteristicas, 
                                                   pattern = "Ã¡", 
                                                   replacement = "á") %>% 
  stri_replace_all_fixed(., pattern = "Ã©", replacement = "é") %>%
  stri_replace_all_fixed(., pattern = "Ãƒ¡", replacement = "á") %>%
  stri_replace_all_fixed(., pattern = "Ãƒ³", replacement = "ó") %>%
  stri_replace_all_fixed(., pattern = "Ãƒ±", replacement = "ñ") %>%
  stri_replace_all_fixed(., pattern = "Ãƒ©", replacement = "é") %>%
  stri_replace_all_fixed(., pattern = "Ã³", replacement = "ó") %>% 
  stri_replace_all_fixed(., pattern = "Ã±", replacement = "ñ") %>%
  stri_replace_all_fixed(., pattern = "Ãº", replacement = "ú")%>%
  stri_replace_all_fixed(., pattern = "Â", replacement = "")

df_famsa$Nombre_producto <- stri_replace_all_fixed(df_famsa$Nombre_producto, 
                                                   pattern = "Ã", 
                                                   replacement = "í")
df_famsa$Caracteristicas <- stri_replace_all_fixed(df_famsa$Caracteristicas, 
                                                   pattern = "Ã", 
                                                   replacement = "í")


df_famsa$Caracteristicas <- stri_replace_all_fixed(df_famsa$Caracteristicas, 
                                                   pattern = "(íƒ¡)", 
                                                   replacement = "k") %>% 
    stri_replace_all_fixed(., pattern = "Ãƒ³", replacement = "ó") %>%
    stri_replace_all_fixed(., pattern = "Ãƒ±", replacement = "ñ") %>%
    stri_replace_all_fixed(., pattern = "Ãƒ©", replacement = "é")



saveRDS(df_coppel, "productos_coppel_detalle.RDS")
saveRDS(df_famsa, "productos_famsa_detalle.RDS")
