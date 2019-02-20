# funcion para coincidencias de nombre
ClosestMatch2 <- function(string, stringVector){
    
    require(RecordLinkage)
    distance = levenshteinSim(string, stringVector)
    #stringVector[which.max(distance)]
    which.max(distance)
}
