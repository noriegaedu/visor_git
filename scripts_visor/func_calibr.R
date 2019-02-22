#' @author Eduardo Noriega
#' 
#' @description La funcion permite crear un shapefile de calibracion en base a un shapefile de reporte
#' de un balance hidrico
#' 
#' @param datos_resumen csv con la siguiente estructura (sin nombre de columnas)
#' |-a-|-b-|-c-|-d-|
#' |---|---|---|---|
#' |---|---|---|---|
#' a = nombre estacion hidrmetrica (igual a nombre usado en WEAP)
#'     Si no existe hidrometrica anhadir NG_ al inicio del nombre
#' b = nombre rio donde esta estacion hidrometrica
#' c = Nodo (de WEAP) de donde se extrae informacion de caudal 
#' d = Existe estacion hidrometrica para nodo del que se extrae informacion? Binario (1 o 0)
#' @param shp_reporte ruta del shapefile de reporte de la cuenca (el que fue usado para visor)
#' @param shp_calibr ruta para escribir shapefile de calibracion
#' 
#' @details dentro de la estructura de la funcion se presenta una funcion adicional llamada 'uh_calibr'
#' esta funcion permite asociar la UH del reporte con las UH de la calibracion de forma interactiva
#' al ser llamada, la funcion itera a traves de la cantidad de datos (filas, uh de calibracion) del archivo
#' 'datos_resumen'. Para cada dato se pide intrducir el numero de UH del reporte que le corresponde separado 
#' por espacios.
#' 
#' @example 
#' datos_resumen : 
#' | Hidro Nujchu   | Rio Ravelo Bajo | Sist.Riego RaveloBajo Return | 1 |
#' | NG_Canchi Mayu | Rio Canchi Mayu | Sist.Riego Canchimayu Return | 0 |
#' 
#' calibr("H:/mmaya/calibr_input_prueba.csv",
#'        "C:/Users/HP/Documents/WEAP Areas/Balance Hidrico Ravelo FINAL/bh/shp/PCP_visor.shp",
#'        "C:/Users/HP/Documents/WEAP Areas/Balance Hidrico Ravelo FINAL/bh/shp/ravelo_calibr_solo2.shp",
#'        reporte = TRUE,
#'        ruta_entrada_reportes = "C:/Users/HP/Documents/WEAP Areas/Balance Hidrico Ravelo FINAL/bh/", 
#'        ruta_salida_reportes = "C:/Users/HP/Documents/WEAP Areas/Balance Hidrico Ravelo FINAL/bh/")

calibr <- function(datos_resumen, shp_reporte, shp_calibr, 
                   reportes = FALSE, ruta_entrada_reportes, ruta_salida_reportes){
    
    require(tidyverse)
    require(sf)
    
    nombres_features <- read.csv(datos_resumen, 
                                 header = FALSE, 
                                 stringsAsFactors = FALSE) %>% 
        pull(1)
    
    shp_reporte <- st_read(shp_reporte)
    
    #source('H:/mmaya/proyectos_R/visor_git/scripts_visor/func_asign_uh.R') # funcion de asignacion uh reporte a uh calibracion
    uh_calibr <- function(){
        message('UH de Reporte de shp que corresponden a cada UH para shp de Calibracion:')
        uh <- lapply(seq_along(nombres_features), 
                     function(x) readline(paste0('UH para "', 
                                                 nombres_features[x], 
                                                 '" = '))) %>% 
            lapply(function(x) as.numeric(unlist(strsplit(x, ' ')))) %>%
            lapply(function(x) paste0('uh', x)) %>% 
            setNames(nombres_features)
        return(uh)
    }
    
    uh <- uh_calibr()
    
    lapply(seq_along(nombres_features), 
           function(x) shp_reporte %>%
               filter(codigo %in% uh[[x]]) %>% # si basado en shp original usar Name
               st_union() %>% 
               st_sf(Name = names(uh)[x], .)) %>%
        do.call(rbind, .) %>% 
        transmute(NOMBRE = Name, 
                  PFAF_HYD_ = 999, 
                  OBSERV_ = NA, 
                  codigo = paste0('uhc', seq_along(nombres_features))) %>% 
        st_write(shp_calibr)
    
    if (reportes == TRUE) {
        reportes_csv <- list.files(ruta_entrada_reportes, 
                                   pattern = 'visor', full.names = TRUE) %>% 
            # grep('esc_visor|PCP_visor|ETR_visor|ETP_visor|BF_visor|IN_visor|SR_visor', ., value = TRUE)
            lapply(read.csv)
        reportes_names <- list.files(ruta_entrada_reportes, 
                                     pattern = 'visor', 
                                     full.names = FALSE) %>% gsub('.csv', '', .)
        
        reportes_calibr <- reportes_csv %>% 
            lapply(function(y) lapply(seq_along(uh), 
                                      function(x) dplyr::select(y, uh[[x]])) %>% 
                       lapply(function(x) apply(x, 1, sum)) %>% 
                       setNames(paste0('uhc', seq_along(uh))) %>% 
                       do.call(cbind, .) %>%
                       cbind(y[1:2], .)) %>%
            setNames(reportes_names)
        
        mapply(function(x, y) write.csv(x, y, row.names = FALSE), 
               x = reportes_calibr, 
               y = paste0(ruta_salida_reportes, 
                          names(reportes_calibr), 
                          '_calibr.csv'))
        }
        
}