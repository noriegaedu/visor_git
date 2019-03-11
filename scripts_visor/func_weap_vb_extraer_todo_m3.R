#' @author Eduardo Noriega
#' 
#' @description MOdificado de 'weap_vb_extraer_varios.R'
#' Con esta nueva funcion se permite generar un solo script vbs para varias variables
#' 
#' @param directorio carpeta donde se almacenara la info de salida
#' Ej: 'bh'
#' @param Var vector con variables
#' *OJO* Por ahora en este orden: c('ETP', 'ETR', 'PCP', 'SR', 'IN', 'BF')
#' Ej: 'ETP', 'ETR', 'PCP', 'SR', 'IN', 'BF'
#' @param estaciones ruta del archivo Excel exportado desde WEAP con la
#' informacion a extraer 
#' En WEAP ir al apartado DATA, del arbol de informacion escoger 'Demand and Catchments',
#' En el panel de la derecha escoger 'Land Use/Area'
#' Abrir de la barra de menu 'Edit/Export Expression to Excel...'
#' en la ventana emergente escoger 
#' - New Workbook 
#' - All Branches
#' - Area Variables
#' - Reference Scenario
#' Guardar la salida de Excel y recordar la ruta.
#' Ej. "H:/mmaya/proyectos_R/visor_git/datos_visor/BHSB_Modelo Nacional_Amazonica_Aug2018.xlsx"
#' @param nom_salida ruta con el nombre del archivo de salida con extension `.vsb`
#' 
#' @example 
#' weap_vb_extraer_todo(directorio = 'bh', 
#'                      Var = c('ETP', 'ETR', 'PCP', 'SR', 'IN', 'BF'), 
#'                      estaciones = "H:/mmaya/proyectos_R/visor_git/datos_visor/BHSB_Modelo Nacional_Amazonica_Aug2018.xlsx", 
#'                      nom_salida = "H:/mmaya/proyectos_R/visor_git/salidas_visor/TODO.vbs")  

weap_vb_extraer_todo_m3 <- function(directorio, estaciones, nom_salida, Var = c('ETP','ETR','PCP','SR','IN','BF')){
    require(tidyverse)
    require(readxl)
    
    directorio <- paste0('"', directorio, '\\"')
    
    var_A <- paste0('A', '_')
    
    #Var <- c('ETP', 'ETR', 'PCP', 'SR', 'IN', 'BF')
    Var <- paste0(Var, '_')
    
    val_name_A <- ':Area Calculated[M^2]' # para llevar a mm
    
    val_name <- c(':ET Potential[m^3]', ':ET Actual[m^3]', ':Observed Precipitation[m^3]', 
                  ':Surface Runoff[m^3]', ':Interflow[m^3]', ':Base Flow[m^3]')
    
    
    est <- read_xlsx(estaciones) %>% 
        pull(5) %>% unique() %>% na.omit() 
    est <- est[-1] # se elmina 'Level 2'
    
    est_variables <- est %>% 
        gsub(' ', '_', .) #%>% 
    #####
    # gsub('.', '_', .) %>% 
    # gsub('ñ', 'n', .) %>% 
    # gsub('Ñ', 'N', .)
    ####
    
    aa <- paste0(var_A, est_variables)
    
    var_q <- paste0('"', Var)
    est_q <- paste0(est, '"')
    
    base <- paste('"Scenario"', '"Year"', '"TS"', sep = ' & "," & ') %>% 
        paste0(., ' & "," &')
    
    zz <- sapply(seq_along(Var), 
                 function(y) paste(sapply(seq_along(est), 
                                          function(x) paste0(var_q[y], est_q[x], 
                                                             collapse = ' & "," & '))))
    # inicio de escritura de vbs
    con <- file(nom_salida)
    
    writeLines(
        c('WEAP.ActiveScenario = "Reference"',
          '',
          sapply(seq_along(Var), 
                 function(x) paste0('salida', x, ' = WEAP.ActiveArea.Directory & ', 
                                    directorio,
                                    ' & "', 
                                    paste0(gsub('_', '', Var[x]), 'm3',
                                           '.csv"'))),
          '',
          'Set objFSO = CreateObject("Scripting.FileSystemObject")',
          '',
          sapply(seq_along(Var) , 
                 function(x) paste0('if objFSO.FileExists(salida', x, ') then',
                                    '\n set objFile', x, '= objFSO.OpenTextFile(salida',x,', 8)',
                                    '\nElse',
                                    '\n set objFile', x, '= objFSO.CreateTextFile(salida',x,')',
                                    paste0('\n z',x,' = ', base, ' ', paste(zz[,x], 
                                                                            collapse = ' & "," & ')),
                                    '',
                                    paste0('\nobjFile',x,'.WriteLine z',x),
                                    '\nEnd If',
                                    '\n')
          ),
          '',
          'For Yr = (BaseYear+1) to EndYear',
          ' For Mes = 1 to NumTimeSteps',
          '',
          sapply(seq_along(est), 
                 function(x) paste0(zz[x,] %>% 
                                        gsub('"', '',.) %>% 
                                        gsub(' ', '_',.), 
                                    ' = WEAP.ResultValue("Demand Sites and Catchments\\',
                                    est[x],
                                    val_name,'", Yr, Mes, WEAP.ActiveScenario)')),
          '',
          sapply(seq_along(Var), 
                 function(y) paste0('z', y, ' = ', 
                                    paste(c(c('WEAP.ActiveScenario', 'Yr', 'Mes'), 
                                            sapply(seq_along(est), 
                                                   function(x) paste0('round(', 
                                                                      zz[x,y] %>% 
                                                                          gsub('"', '',.) %>% 
                                                                          gsub(' ', '_',.),',',2,')'))), 
                                          collapse = ' & "," & '))),
          '',
          sapply(seq_along(Var), 
                 function(x) paste0('objFile',x,'.WriteLine z',x)),
          '',
          ' Next',
          'Next',
          '',
          sapply(seq_along(Var), 
                 function(x) paste0('objFile',x,'.close'))),
        con)
    close(con)
}
