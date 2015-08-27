# Obtener los nombres de los archivos de un directorio

import os
 
dirname = os.getcwd()
directorio = os.listdir(dirname)
count = 0 
for archivo in directorio:
	if archivo == 'getNameFiles.py':
		continue
	else:
		bitacora = os.path.splitext(archivo)[0]
	if count == 0:
		outfile = open('nbArchivos.txt', 'w') # Indicamos el valor 'w'.
		print '\n The file has been created...\n'
	print bitacora
	outfile.write(bitacora + '\n')
	print 'The text line has been created...\n'
	count = count + 1
outfile.close()
print "\nDone!!!"
