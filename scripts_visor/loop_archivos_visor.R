variables <- c('BF', 'ETP', 'ETR', 'IN', 'PCP', 'SR')

shp_cuencas <- "C:/Users/HP/Documents/WEAP Areas/BHSB_Modelo Nacional_Amazonica_Aug2018/WEAPCatchment.shp"
# usar el del modelo weap #shp_cuencas <- "C:/Users/HP/Documents/WEAP Areas/BHSB_Modelo Nacional_Amazonica_Aug2018/Calibracion/SIG/CatchmentsNacional.shp"
#shp_cuencas <- "C:/Users/HP/Documents/WEAP Areas/BHSB_Modelo Nacional_Amazonica_Aug2018/Calibracion/SIG/CatchmentsNacional.shp"
var_csv <- "C:/Users/HP/Documents/WEAP Areas/BHSB_Modelo Nacional_Amazonica_Aug2018/bh/"
#abr_var <- 'PCP'
salida <- "C:/Users/HP/Documents/WEAP Areas/BHSB_Modelo Nacional_Amazonica_Aug2018/bh/"
salida_shp <- "C:/Users/HP/Documents/WEAP Areas/BHSB_Modelo Nacional_Amazonica_Aug2018/bh/shp/"


for (i in variables) {
    arch_visor(shp_cuencas, var_csv, i, salida, salida_shp)
}

# para escurriemiento
lis_esc <- list.files(salida, pattern = 'visor', full.names = TRUE) %>% 
    grep('BF|IN|SR', ., value = TRUE) %>% 
    lapply(function(x) read.csv(x, stringsAsFactors = FALSE)) %>% 
    Reduce('+', .)

lis_esc[,1:2] <- lis_esc[,1:2]/3L

write.csv(lis_esc, paste0(salida, 'esc_visor.csv'), row.names = FALSE)


