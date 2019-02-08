
#base_2 <- paste('WEAP.ActiveScenario', 'Year', 'Mes', sep = ' & , & ') %>% 
#  paste0(., ' & , &')

estaciones = "H:/mmaya/proyectos_R/visor_git/datos_visor/BHSB_Modelo Nacional_Amazonica_Aug2018.xlsx"
est <- read_xlsx(estaciones) %>% 
  pull(5) %>% unique() %>% na.omit() 
est <- est[-1] # se elmina 'Level 2'

est_variables <- est %>% gsub(' ', '_', .)

#directorio <- 'BalanceHidrico'
directorio = 'bh'
directorio <- paste0('"', directorio, '\\"')

var <- 'PCP'
#var_A <- 'A'
var_A <- paste0('A', '_')
var <- paste0(var, '_') # redundancia pero a flata de tiempo
#var <- 'ETP_'
#est <- c('Abapo', 'Abuna')

if (var == 'ETP_') val_name <- ':ET Potential[m^3]'
if (var == 'ETR_') val_name <- ':ET Actual[m^3]'
val_name_A <- ':Area Calculated[M^2]'
if (var == 'PCP_') val_name <- ':Observed Precipitation[m^3]'
if (var == 'SR_') val_name <- ':Surface Runoff[m^3]'
if (var == 'IN_') val_name <- ':Interflow[m^3]'
if (var == 'BF_') val_name <- ':Base Flow[m^3]'


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
                                       paste0(var, est_variables)[x], ',2)/',
                                       paste0(var_A, est_variables)[x],
                                       '*1000'))), 
             collapse = ' & "," & ')


#head <- paste(paste('Scenario', 'Year', 'TS'), 
#      paste0(var, est), 
#      collapse = '') %>% 
#  gsub(' ', ' & , & ', .)

con <- file("H:/mmaya/proyectos_R/visor_git/salidas_visor/ejemplo_PCP_3.vbs")

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
       function(x) paste0(paste0(var_A, est_variables)[x], 
                          ' = WEAP.ResultValue("Demand Sites and Catchments\\',
                          est[x],
                          val_name_A,'", Yr, Mes, WEAP.ActiveScenario)')),
'',
sapply(seq_along(est), 
       function(x) paste0(paste0(var, est_variables)[x], 
                         ' = WEAP.ResultValue("Demand Sites and Catchments\\',
                         est[x],
                         val_name,'", Yr, Mes, WEAP.ActiveScenario)')),
paste0('\nz1 = ', guardar),
'',
'objectFile.WriteLine z1',
'',
' Next',
'Next',
'',
'objFile1.close'),
  con)
close(con)

#sapply(seq_along(est), 
#       function(x) paste(paste0(var, est)[x], 
#                         ' = WEAP.ResultValue'))

#c('asd', sapply(seq_along(est), 
#       function(x) paste0(paste0(var, est)[x], 
#                         ' = WEAP.ResultValue("Demand Sites and Catchments\\',
 #                        est[x],
  #                       ':ET Potential[m^3]", Yr, Mes, WEAP.ActiveScenario)')))
