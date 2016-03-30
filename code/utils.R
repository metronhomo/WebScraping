extrae_tamano <- function(cadena){
  hu <- grep("Pantalla", cadena, ignore.case = T)
  ho <- grep("Refrigerador", cadena,ignore.case = T)
  he <- grep("Lavadora", cadena, ignore.case = T)
  if (length(hu) !=0){
    ju <- as.character(unlist(stri_extract_all(cadena, regex = '([0-9]+")'))) %>% 
      stri_extract_all(regex = "[0-9]+")
    if(is.na(ju)) {
      a <- strsplit(cadena, " ")[[1]]
      b <- a[length(a)] %>% stri_extract_all(str = ., regex = "[0-9]+")
      ju <- substr(b[[1]][1], 1, 2)
    }
    ja <- ifelse(is.na(ju), NA, paste(ju, "pulgadas"))
  }
  if (length(ho) !=0){
    ju <- stri_extract_all(regex = '\\d+( ?)p', 
                           str = cadena, 
                           case_insensitive = TRUE) %>% 
      stri_extract_all(regex = "[0-9]+") %>% 
      unlist()
    ja <- ifelse(is.na(ju[1]), NA, paste(ju[1], "pies"))
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




extrae_tamano_ekt <- function(cadena){
  hu <- grep("Pantalla",cadena, ignore.case = T)
  ho <- grep("Refrigerador",cadena,ignore.case = T)
  he <- grep("Lavadora",cadena, ignore.case = T)
  ja <- NA
  if (length(hu) !=0){
    ju <- stri_extract_all(str = cadena, regex = "[0-9]+ *pulgadas")
    ja <- ifelse(is.na(ju), NA, ju)
  }
  if (length(ho) !=0){
    ju <- stri_extract_all(regex = '\\d+ *pies', 
                           str = cadena, 
                           case_insensitive = TRUE) %>% 
      stri_extract_all(regex = "[0-9]+")
    ja <- ifelse(is.na(ju), NA, paste(ju, "pies"))
  }
  if (length(he) !=0){
    ju <- stri_extract_all(regex = '\\d+ *kilos', 
                           str = cadena, 
                           case_insensitive = TRUE) %>% 
      stri_extract_all(regex = "[0-9]+")
    ja <- ifelse(is.na(ju), NA, paste(ju, "Kg"))
  }
  return(ja)
}



tipo_producto <- function(art){
  hu <- grep("Pantalla", as.character(art), fixed = T)
  ho <- grep("Refrigerador", as.character(art), fixed = T)
  he <- grep("Lavadora", as.character(art), fixed = T)
  pr = NA
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



extrae_marca <- function(cadena, marcas){
  kl <- as.list(strsplit(as.character(cadena),  " ")[[1]]) %>% 
    toupper()
  m <- gsub("[^A-Za-z0-9ñÑ-]", "", kl)
  vb <- intersect(marcas, m)
  if(length(vb) == 0) vb = NA  
  return(vb)
}





marcas <- list("Acer",
               "Acros",
               "Advio",
               "Amana",
               "White-Westinghouse",
               "Quasar",
               "Aoc",
               "Apple",
               "Armada",
               "Aspix",
               "Asus",
               "SEIKI",
               "Atvio",
               "Black_&_Decker",
               "Blackberry",
               "Blue_Point",
               "Bosch",
               "TCL",
               "Cinsa",
               "Coby",
               "Compaq",
               "Continental",
               "Crolls",
               "Daewoo",
               "Delher",
               "Dell",
               "Easy",
               "EKT",
               "Electrolux",
               "E-Machines",
               "Emerson",
               "Extranjero",
               "Flamineta",
               "Fraga",
               "Frigidaire",
               "Game_Boy",
               "Gate_Way",
               "Ge",
               "Goldstar",
               "Hisense",
               "Hitachi",
               "Hoover",
               "Hp",
               "Ibm",
               "Iem",
               "Joinet",
               "Kelvinator",
               "Kenmore",
               "Kenwood",
               "Koblenz",
               "Lanix",
               "Lenovo",
               "Lg",
               "Mabe",
               "Majestic",
               "Maytag",
               "México",
               "Mitsui",
               "Mitzu",
               "Nintendo",
               "Nintendo Revolution / Wii",
               "Panasonic",
               "Philco",
               "Philips",
               "Phillips",
               "Polaroid",
               "Pioneer",
               "RCA",
               "Samsung",
               "Sanyo",
               "Sharp",
               "Singer",
               "Sony",
               "Supermatic",
               "Techpad",
               "Toshiba",
               "Westing House",
               "Whirlpool",
               "X-Box") %>% 
  toupper()