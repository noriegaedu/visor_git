#' @author Eduardo Noriega
#' @description La funcion prepara las salidas de WEAP para caudal con la opcion de corregir las fechas
#' por anho hidrologico
#' 
#' @param ruta_q ruta donde se enceuntran los archivos de caudal de WEAP
#' @param corregir_ah logico, TRUE para corregir fechas por anho hidorlogico
#' 
#' @example 
#' csv_q_visor('C:/Users/HP/Documents/WEAP Areas/Balance Hidrico Ravelo FINAL/bh', 
#'             TRUE)

csv_q_visor <- function(ruta_q, corregir_ah){
    
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