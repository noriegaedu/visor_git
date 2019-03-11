#' @author Eduardo Noriega
#' 
#' @description Funcion que permite generar archivos csv para alimentar a visor 
#' 
#' @param shp_cuencas shape con la informacion espacial de la cuenca modelada
#' Usar el shapefile generado por WEAP
#' Ej.shp_cuencas <- "C:/Users/HP/Documents/WEAP Areas/BHSB_Modelo Nacional_Amazonica_Aug2018/WEAPCatchment.shp"
#' Si se desea usar un shapefile similar al generado por WEAP tener ener cuidado con errores al usar la funcion
#' Dentro del codigo de esta funcion se incluye un pedazo de codigo que permite salvar errores de lectura 
#' de cuencas dentro de shapefile no generado por WEAP. Sin embargo no fiarse y revisar manualmente.
#' Ej. shape no WEAP: "C:/Users/HP/Documents/WEAP Areas/BHSB_Modelo Nacional_Amazonica_Aug2018/Calibracion/SIG/CatchmentsNacional.shp"
#' 
#' @param var_csv Ruta en la que se encuentran los archivos csv de las variables modeladas
#' Ej. "C:/Users/HP/Documents/WEAP Areas/BHSB_Modelo Nacional_Amazonica_Aug2018/bh/"
#' @param abr_var variable modelada de la cual obtener csv para visor
#' EJ. 'PCP'
#' @param salida Ruta de salidas de los archivos csv para visor
#' Ej. "C:/Users/HP/Documents/WEAP Areas/BHSB_Modelo Nacional_Amazonica_Aug2018/bh/"
#' @param salida_shp Ruta de salida para los shapefiles asociados a cada variable modelada
#' Ej. "C:/Users/HP/Documents/WEAP Areas/BHSB_Modelo Nacional_Amazonica_Aug2018/bh/shp/"
#' @param corregir_ah Logico, si modelo fue corrido con anho inicial septiembre se debe corregir
#' Ej. TRUE para corregir desfase de informacion de meses
#' @param escribir_shp Logico, se da la opcion de ecribir los shapes de cada variable (TRUE) o no (FALSE)
#' si es que ya se tienen escritos.
#' 
arch_visor <- function(shp_cuencas, var_csv, abr_var, salida, salida_shp, 
                       corregir_ah = FALSE, escribir_shp = TRUE){
    
    require(tidyverse)
    require(sf)
    
    ####### para cuencas de BHSB2017 shp con todas las cuencas: ########
    bhsb_shp <- st_read(shp_cuencas,
                        stringsAsFactors = FALSE) 
    
    #verificar si: shp no tiene crs y si existen coordenadas negativas (lo que indica que es proy geogr)
    # entonces se asigan proy geogr a cuenca leida
    if (is.na(st_crs(bhsb_shp)) & all(st_bbox(bhsb_shp) < 0)) bhsb_shp <- st_set_crs(bhsb_shp, 4326)
    
    # nombres cuencas: 
    nombres_cuencas <- read.csv(paste0(var_csv,abr_var,'.csv'),
                                check.names = FALSE) %>% names
    nombres_cuencas <- nombres_cuencas[-c(1:3)] %>% gsub(paste0(abr_var, '_'), '', .)
    
    # seleccionar cuencas del shape BHSB2017 en base a nombres de cuencas con informacion
    
    bhsb_shp_reducido <- bhsb_shp %>% filter(Name %in% nombres_cuencas)
    
    faltantes <- setdiff(nombres_cuencas, bhsb_shp_reducido$Name)
    
    # # codigo para verificar nombres que no concuerdan entre los nombres de cuencas y nombres dentro del shape
    # # ver descripcion
    # while (length(faltantes) != 0) {
    #     pos_faltantes <- sapply(seq_along(faltantes),
    #                             function(x) return(agrep(setdiff(nombres_cuencas,
    #                                                              bhsb_shp_reducido$Name)[x],
    #                                                      bhsb_shp$Name))) # loop para posicion de faltantes en un solo vector
    #     bhsb_shp[pos_faltantes,3] <- faltantes
    # 
    #     bhsb_shp_reducido <- bhsb_shp %>% filter(Name %in% nombres_cuencas) # se repite para incluir
    # 
    #     faltantes <- setdiff(nombres_cuencas, bhsb_shp_reducido$Name)
    # 
    # }
    # 
    
    # si existe faltantes
    if (length(faltantes) != 0) {

        # correccion manual de nombres de shp y xlsx
        cat('**************************** \nNombres que no se encuentran en shapefile \n****************************')
        print(faltantes)
        cat('**************************** \nNombres que no se encuentran en shapefile \n****************************\n')
        
        menu_opcion <- menu(c('Tratar de corregir reemplazando caracteres en lista de faltantes', 
               'Corregir fuera de R'), 
             title = 'Que desea hacer?')  
        
        if (menu_opcion == 1) {
            str_buscar <- readline('Que caracteres se buscaran? ')
            str_reemplazar <- readline('Con que caracteres se reemplazaran? ')
            
            faltantes_comparar <- faltantes %>% gsub(str_buscar, str_reemplazar, .)
            
            pos_faltantes <- sapply(seq_along(faltantes),
                                    function(x) return(which(bhsb_shp$Name == faltantes_comparar[x]))) # loop para posicion de faltantes en un solo vector

            bhsb_shp[pos_faltantes,3] <- faltantes
            
            # comprobacion
            bhsb_shp_reducido <- bhsb_shp %>% filter(Name %in% nombres_cuencas)
            faltantes <- setdiff(nombres_cuencas, bhsb_shp_reducido$Name)
            if (length(faltantes != 0)) { stop('Aun no se corrigio los nombres de las cuencas. Creacion de archivos y shapefiles visor fallo')}
        }
        
        if (menu_opcion == 2) {
            stop('Codigo termino aqui. No se crearon archivos para visor.')
        }
    }
    
    bhsb_shp_reducido <- bhsb_shp_reducido %>% 
        arrange(Name) %>% 
        transmute(NOMBRE = Name, 
                  PFAF_HYD_ = 999, 
                  OBSERV_ = NA, 
                  codigo = paste0('uh', 1:nrow(.)), 
                  Cuenca = Name)

    crear_csv_visor <- read.csv(paste0(var_csv,abr_var,'.csv'), 
                                check.names = FALSE) %>% 
        dplyr::select(-c(1:3)) 
    
    if (isFALSE(corregir_ah)) {
        fechas <- read.csv(paste0(var_csv,abr_var,'.csv')) %>% 
            dplyr::select(2:3) %>% 
            setNames(c('Year', 'Month'))
    }
    
    if (isTRUE(corregir_ah)) {
        fechas <- read.csv(paste0(var_csv,abr_var,'.csv')) %>% 
            dplyr::select(2:3) %>% 
            setNames(c('Year', 'Month')) 
        
        anhos <- fechas$Year %>% unique()
        ini <- anhos %>% head(1)
        fin <- anhos %>% tail(1)
        
        fechas  <- fechas %>%  
            mutate(Year = c(rep(ini - 1,4), 
                            rep(seq(ini, fin - 1), each = 12), 
                            rep(fin,8)),
                   Month = rep(c(9:12,1:8), length(anhos)))
        
        # corregir salida de WEAP
        read.csv(paste0(var_csv,abr_var,'.csv'), 
                 check.names = FALSE) %>% 
            mutate(Year = fechas$Year,
                   TS = fechas$Month) %>% 
            write.csv(paste0(var_csv,abr_var,'.csv'),
                      row.names = FALSE)
        
    }

    write.csv(cbind(fechas, 
                    crear_csv_visor %>%
                        dplyr::select(sort(names(.)))) %>% 
                  setNames(., c('Year', 'Month', bhsb_shp_reducido$codigo)), 
              paste0(salida, abr_var, '_visor.csv'),
              row.names = FALSE)
    
    if (isTRUE(escribir_shp)) {
        st_write(bhsb_shp_reducido, 
                 paste0(salida_shp, abr_var, '_visor.shp'))
    }
}