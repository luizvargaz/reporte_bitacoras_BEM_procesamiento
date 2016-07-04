import win32com.client 
import os
import xlwt
while True:
	extension = '.xlsx'
	inp = raw_input('Write "q" or press "Enter" to exit.\n Enter a number of Sheet: ')
	if inp == 'q' :
		break
	if len(inp) < 1:
		break
	try:
			numeroHoja = int(inp)
	except:
		print "Invalid input"
		continue
	##################
	xlApp = win32com.client.DispatchEx('Excel.Application') 
	directorio = os.getcwd()
	count = 0

	## abrir la lista de archivos
	listaArchivos = open('archivos.txt')

	for nombreArchivo in listaArchivos:
	
		numero = str(count)

		## Quitar el espacio \n al final del nombre del archivo y abrirlo 
		nombreArchivo = nombreArchivo.rstrip()
		xlwb = xlApp.Workbooks.Open(directorio + '\\' + nombreArchivo + extension)
		print 'Name of file: ', xlwb
		
		import xlwt
			
		## Extraer la hoja de calculo y almacenarla en una variable
		sheet = xlwb.Worksheets(numeroHoja)
		print 'Reading:',sheet

		## Crear un nuevo libro de trabajo en el cual se copiara la hoja de calculo obtenida		
		print 'The sheet is:',sheet
		xlApp = win32com.client.Dispatch("Excel.Application")
		nwb = xlApp.Workbooks.Add()
		print 'El nwb es: ',nwb
		
		## Crear un nuevo directorio para guardar los archivos
		nombreSheet = sheet.Name
		nombreSheet = nombreSheet.rstrip()
		directoriohojas = directorio + '\\' + nombreSheet
		nuevaruta = directoriohojas 
		print 'The new directory is: ', nuevaruta
		if not os.path.exists(nuevaruta): os.makedirs(nuevaruta)
		
		## Copiar la hoja de trabajo obtenida, guardarla en la ruta especficada y cerrar el archivo
		sheet.Copy(nwb.Worksheets(1))
		nwb.SaveAs(nuevaruta + '\\' + numero + nombreSheet + extension)
		nwb.Close(True)
		print 'Copied sheet...'
		count = count + 1
		print 'Count = ', count

		## Cerrar los procesos de excel 
		xlwb.Close(True)
		xlApp.Quit()
