# Funcion que permite calcular los cuantiles de los datos de rendimeinto por restado y tipo de de produccion
# El usuario ingresa el codigo del grupo de actividad, el año y ciclo agricola
# luis Vargas --  l.vargas@cgiar.org -- junio de 2015

qRendimientoNacional <- function(year, cycle, type){  
        
        ################################################################
        ################ Variables a modificar ########################
        
        wDirectory = 'C:/Users/LVARGAS/Documents/MEGA/Bases de datos/2015 Consultas/Bitacoras completas 2013 2014' # Directorio de trabajo
        nameFile = '/quantilesRnacional.csv'  # Nombre del archivo en que se almacenaran los resultados
        
        ###############################################################
        
        # Establecer el directorio de trabajo y leer los datos
        setwd(wDirectory)
        datosArchivo <- read.csv("consultaRendimiento.csv", colClasses = "character")
        ### names(datosArchivo)
        
        ## year = 2014                         ## test
        ## cycle = 'Otoño-Invierno'            ## test
        ## type = 'Temporal'                   ## test
        ## state = 'Hidalgo'                   ## test
        
        # Extrae en un vector los campo de año
        # Despues valida que en dicho vector exista el año
        vectorYear <- datosArchivo[,2]
        existYear = 0
        
        for(i in vectorYear){
                if(i == year){
                        existYear = 1
                }
        } 
        
        # Validacion de año
        if(existYear == 1){ 
                
                print("The year is valid") 
                
                # Crea un subconjunto de datos de acuerdo al año ingresado
                subDatosYear <- datosArchivo[datosArchivo[,2]== year, ] 
                ### head(subDatosYear)
                
                # Extrae en un vector los campo de ciclo agronomico
                # Despues valida que en dicho vector exista el nombre del ciclo
                vectorCycle <- subDatosYear[,3]
                existCycle = 0
                
                for(i in vectorCycle){
                        if(i == cycle){
                                existCycle = 1
                        }
                } 
                
                # Validacion de ciclo
                if(existCycle == 1){
                        
                        print("The cycle is valid")
                        
                        # Crea un subconjunto de datos de acuerdo al ciclo ingresado
                        subDatosYearCycle <- subDatosYear[subDatosYear[,3]== cycle , ]
                        ### head(subDatosYearCycle)
                        
                        # Extrae en un vector del tipo de produccion
                        # Despues valida que exista el tipo de produccion
                        vectorType <- subDatosYearCycle[,4]
                        
                        existeType = 0
                        
                        for(i in vectorType){
                                if(i == type){
                                        existeType = 1
                                }                                
                                
                        }
                        
                        
                        # Validacion de ciclo
                        if(existeType == 1){
                                
                                print("The type is valid")
                                
                                # Crea un subconjunto de datos de acuerdo al tipo de produccion ingresado
                                subDatosYearCycleType <- subDatosYearCycle[subDatosYearCycle[,4]== type , ]
                                ### head(subDatosYearCycleType)                                                                                  
                                                                      
                                                              
                                # Calculo de cuantiles de la columna rendimiento
                                # El resultado se almacena en el vector result
                                yield <- subDatosYearCycleType[,29]
                                yield <- as.numeric(yield)
                                class(yield)
                                label <- c("los cuantiles de ",year,"-", cycle, "-",type,", son:")
                                print(paste(label,collapse=""))
                                result <- quantile(yield)
                                print(result)
                                
                                #Quitar el nombre de los elementos del vector result
                                ###namesResult <- names(result)
                                dataNoName <- unname(result)
                                        
                                # Concatenar los valores de las variables
                                namesStrings  <- c(year, cycle, type)
                                
                                # crear la matrizz concatenando el resultado con los valores de las variables para identificar los registros
                                numeroColumnas <- length(dataNoName) + length(namesStrings)
                                dataMatrix <- matrix(c(dataNoName,namesStrings), ncol=numeroColumnas, byrow=TRUE)
                                vectorfileDirectory  <- c(wDirectory,nameFile)
                                fileDirectory <- paste(vectorfileDirectory, collapse="")
                                write.table(dataMatrix, fileDirectory, sep=",", append=TRUE, col.names=FALSE)                                       
                                        
                                                
                        }else{                                
                                stop("Invalid type")
                        }
                        
                }else{                        
                        stop("Invalid cycle")
                }
                
                
        }else{
                stop("invalid year")  
        }
        
        
}

qRendimientoNacional(2013, 'Primavera-Verano', 'Riego')
qRendimientoNacional(2013, 'Primavera-Verano', 'Temporal')
qRendimientoNacional(2014, 'Primavera-Verano', 'Temporal')
qRendimientoNacional(2014, 'Primavera-Verano', 'Riego')
qRendimientoNacional(2013, 'Otoño-Invierno', 'Temporal')
qRendimientoNacional(2013, 'Otoño-Invierno', 'Riego')
qRendimientoNacional(2014, 'Otoño-Invierno', 'Riego')
qRendimientoNacional(2014, 'Otoño-Invierno', 'Temporal')
