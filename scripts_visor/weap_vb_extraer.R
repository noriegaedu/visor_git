#' @author Eduardo Noriega
#' 
#' @description el script genera un archivo para utilizarlo en el editor de 
#' scripts de WEAP (Visual Basic) de acuerdo a la variable de la que se
#' requiera infomraicon
#' 
#' @param directorio carpeta donde se almacenara la info de salida
#' Ej: 'bh'
#' @param var variable de la que se obtendra la informacion
#' Ej: 'ETP'
#' @param estaciones ruta del archivo Excel exportado desde WEAP con la
#' informacion a extraer 
#' En WEAP ir a: al aprtado de DATA, uan vez alli Edit/Export Expression to Excel...
#' en la ventana emrgente escoger 
#' - New Workbook 
#' - All Branches
#' - All Variables
#' - Reference Scenario
#' @param nom_salida ruta con el nombre del archivo de salida con extension 
#' `.vsb`
#' 
#' @example 
#' weap_vb_extraer('bh', 
#'                 'PCP', 
#'                 "C:/Users/Gustavo Ayala/Documents/WEAP Areas/zongo_2/BHSB_Modelo_Nacional_Amazonica_Aug2018.xlsx", 
#'                 "C:/Users/Gustavo Ayala/Documents/WEAP Areas/zongo_2/scripts/ejemplo.vbs")
#'                 
weap_vb_extraer <- function(directorio, var, estaciones, nom_salida){
  require(dplyr)
  require(readxl)
  
  directorio <- paste0('"', directorio, '\\"')
  
  var <- paste0(var, '_')
  if (var == 'ETP_') val_name <- ':ET Potential[m^3]'
  if (var == 'ETR_') val_name <- ':ET Actual[m^3]'
  if (var == 'A_') val_name <- ':Area Calculated[M^2]'
  if (var == 'PCP_') val_name <- ':Observed Precipitation[m^3]'
  if (var == 'SR_') val_name <- ':Surface Runoff[m^3]'
  if (var == 'IN_') val_name <- ':Interflow[m^3]'
  if (var == 'BF_') val_name <- ':Base Flow[m^3]'
  
  
  est <- read_xlsx(estaciones) %>% 
    pull(5) %>% unique() %>% na.omit() 
  est <- est[-1] # se elmina 'Level 2'
  
  est_variables <- est %>% gsub(' ', '_', .)
    
  var_q <- paste0('"', var)
  est_q <- paste0(est, '"')
  
  base <- paste('"Scenario"', '"Year"', '"TS"', sep = ' & "," & ') %>% 
    paste0(., ' & "," &')
  
  head <- paste(base, 
                paste0(var, est, 
                       collapse = ' & "," & '))
  
  head_q <- paste(base, 
                  paste0(var_q, est_q, 
                         collapse = ' & "," & '))
  
  guardar <- paste(c(c('WEAP.ActiveScenario', 'Yr', 'Mes'), 
                     sapply(seq_along(est), 
                            function(x) paste0('round(', 
                                               paste0(var, est_variables)[x], ',2)'))), 
                   collapse = ' & "," & ')
  
  # inicio de escritura de vbs
  con <- file(nom_salida)
  
  writeLines(
    c('WEAP.ActiveScenario = "Reference"',
      '',
      paste0('salida1 = WEAP.ActiveArea.Directory & ', 
             directorio, 
             ' & "', 
             paste0(gsub('_', '', var), 
                    '.csv"')),
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
      sapply(seq_along(est), 
             function(x) paste0(paste0(var, est_variables)[x], 
                                ' = WEAP.ResultValue("Demand Sites and Catchments\\',
                                est[x],
                                val_name,'", Yr, Mes, WEAP.ActiveScenario)')),
      paste0('\nz1 = ', guardar),
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
