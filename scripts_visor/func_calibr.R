#' @author Eduardo Noriega
#' 
#' @description La funcion permite crear un shapefile de calibracion en base a un shapefile de reporte
#' de un balance hidrico y genera los archivos para el geovisor de las todas las variables (ETP, ETR, PCP, 
#' BF, IN, SR, Q_sim y Q_obs). Si es necesario la funcion corrige el desfase temporal 
#' de los archivos de caudal.
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
#' @param ruta_q ruta donde se enceuntran los archivos de caudal de WEAP
#' @param corregir_ah logico, TRUE para corregir fechas por anho hidorlogico
#' @param umbral_hueco numero que indica en m2 (para shapefiel en proyeccion geografica) el umbral por
#' debajo del cual se eliminaran los huecos (voids) dentro de una subcuenca
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
#' calibr("H:/mmaya/datos_q_azero.csv",
#'        "C:/Users/HP/Documents/WEAP Areas/Balance Hidrico Ravelo FINAL/bh/shp/PCP_visor.shp",
#'        "C:/Users/HP/Documents/WEAP Areas/Balance Hidrico Ravelo FINAL/bh/shp/ravelo_calibr_solo2.shp",
#'        reporte = TRUE,
#'        ruta_entrada_reportes = "C:/Users/HP/Documents/WEAP Areas/Balance Hidrico Ravelo FINAL/bh/", 
#'        ruta_salida_reportes = "C:/Users/HP/Documents/WEAP Areas/Balance Hidrico Ravelo FINAL/bh/",
#'        ruta_q = 'C:/Users/HP/Documents/WEAP Areas/BH Azero/bh/',
#'        corregir_ah = TRUE)

calibr <- function(datos_resumen, shp_reporte, shp_calibr, 
                   umbral_hueco = 100, reportes = FALSE, 
                   ruta_entrada_reportes, ruta_salida_reportes, 
                   ruta_q, corregir_ah = FALSE){
    
    require(tidyverse)
    require(sf)
    require(smoothr)
    
    nombres_features <- read.csv(datos_resumen, 
                                 header = FALSE, 
                                 stringsAsFactors = FALSE) %>% 
        pull(1)
    
    shp_reporte <- st_read(shp_reporte, quiet = TRUE)
    
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
                  codigo = paste0('uhc', seq_along(nombres_features)),
                  Cuenca = Name) %>% 
        fill_holes(threshold = umbral_hueco) %>% # valore de threshold varaible
        st_write(shp_calibr)
    
    # otras varaibles difenrtes de Q
    
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
    
    # para caudal
    qs <- list.files(ruta_q,
                     pattern = 'Q_', full.names = TRUE) %>% 
        lapply(read.csv, check.names = FALSE)
    qs_names <- list.files(ruta_q,
                           pattern = 'Q_', full.names = TRUE) %>% gsub('.csv', '', .)
    
    if (isFALSE(corregir_ah)) {
        fechas <- lapply(qs, function(x) dplyr::select(x, 2:3)) %>% 
            lapply(function(x) setNames(x, c('Year', 'Month')))
    }
    
    if (isTRUE(corregir_ah)) {
        fechas <- lapply(qs, function(x) dplyr::select(x, 2:3)) %>% 
            lapply(function(x) setNames(x, c('Year', 'Month')))
        
        anhos <- fechas[[1]]$Year %>% unique()
        ini <- anhos %>% head(1)
        fin <- anhos %>% tail(1)
        
        fechas <- lapply(qs, function(x) dplyr::select(x, 2:3)) %>% 
            lapply(function(x) setNames(x, c('Year', 'Month'))) %>% 
            lapply(function(x) mutate(x, Year = c(rep(ini - 1,4), 
                                                  rep(seq(ini, fin - 1), each = 12), 
                                                  rep(fin,8)),
                                      Month = rep(c(9:12,1:8), length(anhos))))
        
        # corregir salida de WEAP
        qs_orig <- qs %>% 
            lapply(function(x) mutate(x, Year = fechas[[1]]$Year,
                                      TS = fechas[[1]]$Month))
        mapply(function(x, y) write.csv(x, y, row.names = FALSE), 
               x = qs_orig, y = paste0(qs_names, '.csv'))
        
    }
    
    datos <- lapply(qs, function(x) dplyr::select(x, -c(1:3))) %>% 
        lapply(function(x) replace(x, x == -999, '')) %>% 
        lapply(function(x) setNames(x, paste0('uhc', seq_along(qs))))
    
    csv <- lapply(seq_along(qs), function(x) cbind(fechas[[x]], datos[[x]]))
    
    mapply(function(x, y) write.csv(x, y, row.names = FALSE), 
           x = csv, y = paste0(qs_names, '_visor.csv'))
    
}