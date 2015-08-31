# Obtener datos diferentes tabas, almacenarlas en un solo data-frame y eliminar los registros NA. Almacenar los datos en una sola hoja CSV. 
# funcion que permite unir los datos de varias hojas contenidas en diferentes archivos de extension .xlsx
# Por Luis Vargas (luizvargaz@hotmail.com) junio de 2015
# Los parametros de la funcion son: 1) el nombre de la hoja, y 2) el numero de la hoja a unir. Ejemplo: abrirExcel('01_caracteristicas Bitácora', 1)

# Leer cada uno de los libros de excel y obtener sus datos
abrirExcel <- function(nuevoArchivo, numeroHoja){
        
        #Fijar el espacio de trabajo
        directorio <- c("C:/Users/LVARGAS/Documents/MEGA/Bases de datos/2015 Consultas/Bitacoras completas 2013 2014/Bitacoras para extraer/",nuevoArchivo)
        nombreDirectorio <-paste(directorio,collapse="")
        setwd(nombreDirectorio)
        
        numeroHoja = as.numeric(numeroHoja)
        extensionArchivo = '.xlsx'
        extensionNuevo = '.csv'
        library(readxl)
        
        # Obtener los datos del archivo Excel que contiene los nombres de los estados 
        # y despues almacenearlos en un vector
        archivosExcel <- read.table("nbArchivos.txt", 1)
        nombresVector <- archivosExcel[,] 
        archivosExcel <- read.table("nbArchivos.txt")
        nombresVector <- archivosExcel[,] 
        
        numeroColumnas <- ncol(archivosExcel)
        if(numeroColumnas  > 1){
                nombresVector <- apply(nombresVector[,], 1, paste, collapse = " ")        
        } 
        
        count = 0
        
        # tomar cada uno de los elementos del vector con los nombres de los estados, para asi generar los nombres de
        # los archivos al concatenerlos con la cadena .xlsx
        for(i in nombresVector){
                
                print(i)

                archivo <-c(i,extensionArchivo)
                nombreArchivo <- paste(archivo,collapse="")
                print(nombreArchivo)
                
                datos <- read_excel(nombreArchivo, sheet = numeroHoja)
                                
                # Abrir cada archivo y unir el contenido con el resto de los datos
                if(count == 0){
                        
                        unionDatos <- datos
                        
                }else{
                        
                        unionDatos <- rbind(unionDatos, datos)
                        
                }                
                
                count = count + 1
        }
        
        ####################
        
        ### Procedimiento para eliminar las filas que contengan valores NA en todos sus registros
        valoresNA <- is.na(unionDatos[,1])
        unionDatosSinNA <- unionDatos[!valoresNA,]
                
        ###################
        
        # attributes(unionDatos)
        # exportar el data frame que contiene los datos que se han almacenado en el objeto unionDatos
        nuevoArchivo <- c(nuevoArchivo,extensionNuevo)
        nombreArchivoNuevo <- paste(nuevoArchivo,collapse="")
        
        write.csv(unionDatosSinNA , file = nombreArchivoNuevo, row.names = FALSE)

        
}


abrirExcel('01_caracteristicas Bitácora', 1)
abrirExcel('02_tecnologias', 1)
abrirExcel('03_productores', 1)
abrirExcel('04_parcelas', 1)
abrirExcel('05_etapa Captura', 1)
abrirExcel('20_riegos_Descripcion', 1)
abrirExcel('24_rendimiento', 1)

abrirExcel('08_aplicacion Insumos_descripci', 1)
abrirExcel('09_aplicacion Insumos _producto', 1)
abrirExcel('11_labores CulturalesCosecha', 1)
abrirExcel('13_siembra Resiembra_descripcio', 1)
abrirExcel('15_analisis Suelo_Descripcion', 1)
abrirExcel('16_analisis Suelo_Resultado', 1)
abrirExcel('18_fertilizante Organico', 1)
abrirExcel('22_afectaciones Cosecha_Factore', 1)
abrirExcel('30_rotacion Cultivos', 1)
abrirExcel('31_variedades Adecuadas', 1)
