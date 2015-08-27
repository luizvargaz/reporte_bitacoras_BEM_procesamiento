# Funcion que permite calcular los cuantiles de los datos de rendimeinto por restado y tipo de de produccion
# El usuario ingresa el codigo del grupo de actividad, el año y ciclo agricola
# luis Vargas --  l.vargas@cgiar.org -- junio de 2015

qRendimiento <- function(year, cycle, type, state){  
        
        ################################################################
        ################ Variables a modificar ########################
        
        wDirectory = 'C:/Users/LVARGAS/Documents/MEGA/Bases de datos/2015 Consultas/Bitacoras completas 2013 2014' # Directorio de trabajo
        nameFile = '/quantilesR.csv'  # Nombre del archivo en que se almacenaran los resultados

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
                                
                                # Extrae en un vector del estado
                                # Despues valida que exista el estado
                                vectorState <- subDatosYearCycleType[,9]
                                
                                existeState = 0
                                
                                for(i in vectorState){
                                        if(i == state){
                                                existeState = 1
                                        }                                
                                        
                                }
                                
                                if(existeState == 1){
                                        
                                        print("The state is valid")
                                        
                                        # Crea un subconjunto de datos de acuerdo al estado ingresado
                                        subDatosYearCycleTypeState <- subDatosYearCycleType[subDatosYearCycleType[,9]== state , ]
                                        ### head(subDatosYearCycleTypeState)
                                        
                                        # Calculo de cuantiles de la columna rendimiento
                                        # El resultado se almacena en el vector result
                                        yield <- subDatosYearCycleTypeState[,29]
                                        yield <- as.numeric(yield)
                                        class(yield)
                                        label <- c("los cuantiles de ",year,"-", cycle, "-",type,"-",state,", son:")
                                        print(paste(label,collapse=""))
                                        result <- quantile(yield)
                                        print(result)
                                        
                                        #Quitar el nombre de los elementos del vector result
                                        ###namesResult <- names(result)
                                        dataNoName <- unname(result)
                                        
                                        # Concatenar los valores de las variables
                                        namesStrings  <- c(year, cycle, type, state)
                                        
                                        # crear la matrizz concatenando el resultado con los valores de las variables para identificar los registros
                                        numeroColumnas <- length(dataNoName) + length(namesStrings)
                                        dataMatrix <- matrix(c(dataNoName,namesStrings), ncol=numeroColumnas, byrow=TRUE)
                                        vectorfileDirectory  <- c(wDirectory,nameFile)
                                        fileDirectory <- paste(vectorfileDirectory, collapse="")
                                        write.table(dataMatrix, fileDirectory, sep=",", append=TRUE, col.names=FALSE)
                                         
                                        
                                }else{                                        
                                        stop("Invalid state") 
                                        
                                }
                        
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


qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Veracruz de Ignacio de la Llave')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Guerrero')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Jalisco')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Michoacán de Ocampo')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Michoacán de Ocampo')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Oaxaca')
qRendimiento(2013, 'Otoño-Invierno', 'Riego', 'Sonora')
qRendimiento(2013, 'Otoño-Invierno', 'Riego', 'Sinaloa')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Tlaxcala')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Querétaro')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Querétaro')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Hidalgo')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'México')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Morelos')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Hidalgo')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Chiapas')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'México')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Morelos')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Puebla')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Distrito Federal')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Distrito Federal')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Zacatecas')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Durango')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Zacatecas')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'San Luis Potosí')
qRendimiento(2014, 'Otoño-Invierno', 'Riego', 'Sinaloa')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Campeche')
qRendimiento(2013, 'Otoño-Invierno', 'Temporal', 'Jalisco')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Jalisco')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Guanajuato')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Puebla')
qRendimiento(2013, 'Otoño-Invierno', 'Temporal', 'Tlaxcala')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Tlaxcala')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Oaxaca')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Chihuahua')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Colima')
qRendimiento(2013, 'Otoño-Invierno', 'Temporal', 'Oaxaca')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Durango')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Yucatán')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Tabasco')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Chiapas')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Quintana Roo')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Chiapas')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Guerrero')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'México')
qRendimiento(2014, 'Otoño-Invierno', 'Temporal', 'Sinaloa')
qRendimiento(2013, 'Otoño-Invierno', 'Temporal', 'Guerrero')
qRendimiento(2013, 'Primavera-Verano', 'Temporal', 'Guanajuato')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Aguascalientes')
qRendimiento(2013, 'Otoño-Invierno', 'Riego', 'Oaxaca')
qRendimiento(2013, 'Otoño-Invierno', 'Temporal', 'Quintana Roo')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Hidalgo')
qRendimiento(2014, 'Primavera-Verano', 'Riego', 'Sinaloa')
qRendimiento(2014, 'Primavera-Verano', 'Riego', 'Querétaro')
qRendimiento(2014, 'Primavera-Verano', 'Riego', 'Michoacán de Ocampo')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Sinaloa')
qRendimiento(2013, 'Otoño-Invierno', 'Temporal', 'Sinaloa')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Sinaloa')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Puebla')
qRendimiento(2014, 'Primavera-Verano', 'Riego', 'Guanajuato')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Morelos')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Guanajuato')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Michoacán de Ocampo')
qRendimiento(2014, 'Primavera-Verano', 'Riego', 'Hidalgo')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Zacatecas')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Durango')
qRendimiento(2014, 'Otoño-Invierno', 'Temporal', 'Morelos')
qRendimiento(2014, 'Otoño-Invierno', 'Riego', 'Baja California Sur')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Querétaro')
qRendimiento(2014, 'Primavera-Verano', 'Riego', 'Zacatecas')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Jalisco')
qRendimiento(2014, 'Otoño-Invierno', 'Temporal', 'Guanajuato')
qRendimiento(2014, 'Primavera-Verano', 'Riego', 'Guerrero')
qRendimiento(2014, 'Primavera-Verano', 'Riego', 'Chihuahua')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Tlaxcala')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Oaxaca')
qRendimiento(2014, 'Primavera-Verano', 'Riego', 'Aguascalientes')
qRendimiento(2014, 'Primavera-Verano', 'Riego', 'México')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Veracruz de Ignacio de la Llave')
qRendimiento(2014, 'Primavera-Verano', 'Riego', 'Chiapas')
qRendimiento(2014, 'Otoño-Invierno', 'Temporal', 'Chiapas')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Colima')
qRendimiento(2013, 'Primavera-Verano', 'Riego', 'Baja California Sur')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Campeche')
qRendimiento(2014, 'Primavera-Verano', 'Temporal', 'Aguascalientes')
qRendimiento(2014, 'Otoño-Invierno', 'Temporal', 'Hidalgo')
qRendimiento(2014, 'Primavera-Verano', 'Riego', 'Oaxaca')
qRendimiento(2014, 'Otoño-Invierno', 'Riego', 'Guanajuato')
