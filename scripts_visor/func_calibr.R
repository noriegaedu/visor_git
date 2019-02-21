datos_resumen <- "H:/mmaya/calibr_input_prueba.csv"
shp_reporte <- "C:/Users/HP/Documents/WEAP Areas/Balance Hidrico Ravelo FINAL/bh/shp/PCP_visor.shp"
shp_calibr <- "C:/Users/HP/Documents/WEAP Areas/Balance Hidrico Ravelo FINAL/bh/shp/ravelo_calibr_solo2.shp"

calibr <- function(datos_resumen, shp_reporte, shp_calibr){
    
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
                  codigo = paste0('uhc', length(nombres_features))) %>% 
        st_write(shp_calibr)
        
}







