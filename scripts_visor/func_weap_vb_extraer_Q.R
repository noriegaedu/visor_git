#' @author Edaurdo Noriega
#' @description La funcion crea un archivo vsb (script de visual basic) para extraer informacion de caudal 
#' observado o simulado en WEAP
#' 
#' @param directorio caracter, carpeta donde se guardaran la salida
#' @param nom_salida ruta con el nombre para el archivo vbs 
#' @param ctrl csv con la siguiente estructura (sin nombre de columnas)
#' |-a-|-b-|-c-|-d-|
#' |---|---|---|---|
#' |---|---|---|---|
#' a = nombre estacion hidrmetrica (igual a nombre usado en WEAP)
#'     Si no existe hidrometrica anhadir NG_ al inicio del nombre
#' b = nombre rio donde esta estacion hidrometrica
#' c = Nodo (de WEAP) de donde se extrae informacion de caudal 
#' d = Existe estacion hidrometrica para nodo del que se extrae informacion? Binario (1 o 0)
#' @param sim_obs caracter para indicar que tipo de caudal se quiere extraer: simulado u observado
#' 
#' @example 
#' #' datos_resumen : 
#' | Hidro Nujchu   | Rio Ravelo Bajo | Sist.Riego RaveloBajo Return | 1 |
#' | NG_Canchi Mayu | Rio Canchi Mayu | Sist.Riego Canchimayu Return | 0 |
#' 
#' weap_vb_extraer_Q('bh',
#'                   'C:/Users/HP/Documents/WEAP Areas/prueba_q_ojo.vbs',
#'                   "H:/mmaya/calibr_input_prueba.csv",
#'                   sim_obs = 'sim)

weap_vb_extraer_Q <- function(directorio, nom_salida, ctrl, sim_obs = 'sim'){
    
    require(tidyverse)
    
    base <- paste('"Scenario"', '"Year"', '"TS"', sep = ' & "," & ') %>% 
        paste0(., ' & "," &')
    
    directorio <- paste0('"', directorio, '\\"')
    
    ctrl <- read.csv(ctrl, header = FALSE, stringsAsFactors = FALSE)
    ctrl <- list(pto_ctrl = ctrl$V1,
                 rio = ctrl$V2,
                 nodo_ctrl = ctrl$V3,
                 obs = ctrl$V4)
    
    head_q <- paste(base, 
                    paste0(paste0('"',
                                  ctrl$pto_ctrl %>% 
                                      gsub(' ', '_', .), '"'), 
                           collapse = ' & "," & '))
    
    if (sim_obs == 'sim') {
        nombre_csv <- '"Q_sim.csv"'
        extraccion <- sapply(seq_along(ctrl$pto_ctrl), 
                             function(x) paste0(ctrl$pto_ctrl[x] %>% gsub(' ', '_', .), 
                                                ' = WEAP.ResultValue("Supply and Resources\\River\\',
                                                ctrl$rio[x],
                                                '\\Reaches\\Below ',
                                                ctrl$nodo_ctrl[x],
                                                ':Streamflow[CMS]", Yr, Mes, WEAP.ActiveScenario)'))
    }
    
    if (sim_obs == 'obs') {
        nombre_csv <- '"Q_obs.csv"'
        extraccion <- sapply(seq_along(ctrl$pto_ctrl), 
                             function(x) ifelse(ctrl$obs[x] == 1, 
                                                paste0(ctrl$pto_ctrl[x] %>% gsub(' ', '_', .), 
                                                       ' = WEAP.ResultValue("Supply and Resources\\River\\',
                                                       ctrl$rio[x],
                                                       '\\Streamflow Gauges\\',
                                                       ctrl$pto_ctrl[x],
                                                       ':Streamflow Data[CMS]", Yr, Mes, WEAP.ActiveScenario)'),
                                                paste0(ctrl$pto_ctrl[x] %>% gsub(' ', '_', .), ' = -999')))
    }
    
    con <- file(nom_salida)
    
    writeLines(
        c('WEAP.ActiveScenario = "Reference"',
          '',
          paste0('salida1 = WEAP.ActiveArea.Directory & ', directorio, ' & ', nombre_csv),
          '',
          'Set objFSO = CreateObject("Scripting.FileSystemObject")',
          '',
          'if objFSO.FileExists(salida1) then',
          ' set objFile1 = objFSO.OpenTextFile(salida1, 8)',
          'Else',
          ' set objFile1 = objFSO.CreateTextFile(salida1)',
          paste0(' z1 = ', head_q),
          '',
          'objFile1.WriteLine z1',
          'End If',
          '',
          'For Yr = (BaseYear+1) to EndYear',
          ' For Mes = 1 to NumTimeSteps',
          '',
          extraccion,
          '',
          paste0('\nz1 = ', paste(c(c('WEAP.ActiveScenario', 'Yr', 'Mes'), 
                                    sapply(seq_along(ctrl$pto_ctrl), 
                                           function(x) paste0('round(',
                                                              ctrl$pto_ctrl[x] %>% 
                                                                  gsub(' ', '_', .),
                                                              ',2)'))), 
                                  collapse = ' & "," & ')),
          '',
          'objFile1.WriteLine z1',
          '',
          ' Next',
          'Next',
          '',
          'objFile1.close'),
        con)
    close(con)
}
