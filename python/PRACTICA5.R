#############################################################################
#
# PRACTICA R
#
# Expresión diferencial de genes de ratón
# Microarray de Affymetrix (Affymetrix Murine Genome U74A version 2 MG_U74Av2
# Origen de los datos: GEO GSE5583 (http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE5583)
# Publicación: Mol Cell Biol 2006 Nov;26(21):7913-28.  16940178 (http://www.ncbi.nlm.nih.gov/pubmed/16940178)
#
# Muestras: 3 Wild Type x 3 Histone deacetylase 1 (HDAC1)
#
# R código original (credits): Ahmed Moustafa
#
#
##############################################################################

# Instalar RCurl

if (!requireNamespace("BiocManager"))
    install.packages("BiocManager")
BiocManager::install("RCurl")

# Si esto falla, que seguro lo hace tratar de instalarlo usando el menú, Paquetes, Servidor Spain A Coruña, RCurl

# Cargamos el paquete y los datos
library(RCurl)
url = getURL ("http://bit.ly/GSE5583_data", followlocation = TRUE)
data = as.matrix(read.table (text = url, row.names = 1, header = T))

# Chequeamos las dimensiones de los datos, y vemos las primeras y las últimas filas
dim(data)
head(data)
tail(data)

# Hacemos un primer histograma para explorar los datos
hist(data, col = "gray", main="GSE5583 - Histogram")

# Transformamos los datos con un logaritmo 
# ¿Qué pasa si hacemos una transformación logarítima de los datos? ¿Para qué sirve?
#Para mejorar la visualización de los datos.
data2 = log2(data)
hist(data2, col = "gray", main="GSE5583 (log2) - Histogram")
#Para mejorar la visualización de los datos.
#No hay que utilizar los datos transformados para los análisis, se usan los datos brutos.

# Hacemos un boxplot con los datos transformados. ¿Qué significan los parámetros que hemos empleado?
# ¿Qué es un boxplot?
boxplot(data2, col=c("blue", "blue", "blue",
	"orange", "orange", "orange"),
	main="GSE5583 - boxplots, las=2")
#Un boxplot es una representación gráfica que permite visualizar
#la distribución de un conjunto de datos numéricos y 
#detectar posibles valores outliers.
#Muestra la dispersión y la simetría de los datos numéricos.
#Mínimo 
#Primer cuartil: el 25% de los datos están por debajo de este valor
#Mediana: valor central
#Tercer cuartil: 75% de los datos están por debajo de este valor.
#Máximo: sin contar outliers.
#Los bigotes se extienden hasta 1.5 veces el rango del intercuartil
#Con los datos transformados nos  referimos a que antes de hacer el boxplot se ha
#aplicado alguna transformación de los datos como la raíz cuadrada (estabilizar varianza)
#o la estandarización (comparar variables).
#El objetivo de transformar datos es mejorar la interpretación y visualización siempre y cuando los datos estén muy sesgados.	
#Data2: datos que tenemos que representar
#Col: colores de las cajas
#Main: título del gráfico
#las: orientación (las2: nombres en el eje x)

# Hacemos un hierarchical clustering de las muestras basándonos en un coeficiente de correlación ç
# de los valores de expresión. ¿Es correcta la separación?
hc = hclust(as.dist(1-cor(data2)))
plot(hc, main="GSE5583 - Hierarchical Clustering")


#######################################
# Análisis de Expresión Diferencial 
#######################################

# Primero separamos las dos condiciones. ¿Qué tipo de datos has generado?
#Hemos generado una matriz de datos, es decir, una tabla compuesta por vectores.
wt <- data[,1:3]
ko <- data[,4:6]
class(wt)


head(wt)
head(ko)

# Calcula las medias de las muestras para cada condición. Usa apply
wt.mean = apply(wt, 1, mean)
ko.mean = apply(ko, 1, mean)
head(wt.mean)
head(ko.mean)

# ¿Cuál es la media más alta?
limit = max(wt.mean, ko.mean)
limit

# Ahora hacemos un scatter plot (gráfico de dispersión)
plot(ko.mean ~ wt.mean, xlab = "WT", ylab = "KO",
	main = "GSE5583 - Scatter", xlim = c(0, limit), ylim = c(0, limit))
# Añadir una línea diagonal con abline
abline(0, 1, col = "red")
#En un gráfico de dispersión comparamos los datos del eje x con los del eje y. Si las variables del eje x son identicas a las del eje y
#nos sale una diagonal perfecta.

# ¿Eres capaz de añadirle un grid?
grid()
#abline(a, b): línea de pendiente b y ordenada en el origen a
#abline(h=y): línea horizontal
#abline(v=x): línea vertical
abline(1, 2, col = "red")     # línea y = 2x + 1
abline(h = 2, col = "green")  # línea y = 2
abline(v = 3, col = "violet") # línea x = 3
#Si a las tres líneas les quitas el comentario, los vectores aparecen como  líneas en la gráfica representada.
#Si, le he añadido  una cuadrícula.

# Calculamos la diferencia entre las medias de las condiciones
diff.mean = wt.mean - ko.mean
#Si el KO es mayor que el WT,el valor es negativo.
#Si el WT es mayor que el KO, el valor es positivo
# Hacemos un histograma de las diferencias de medias
hist(diff.mean, col = "gray")


# Calculamos la significancia estadística con un t-test.
# Primero crea una lista vacía para guardar los p-values
# Segundo crea una lista vacía para guardar las estadísticas del test.
# OJO que aquí usamos los datos SIN TRANSFORMAR. ¿Por qué?
#Aquí utilizamos los datos sin transformar porque para análisis utilizamos datos brutos, sino estaríamos viendo una máscara.
#Los datos transformados se añaden en imágenes.
# ¿Cuántas valores tiene cada muestra?
#Los valores que tiene cada muestra es el número de genes o número de filas, que es 12.488
pvalue = NULL 
tstat = NULL 
for(i in 1 : nrow(data)) { #Para cada gen
	x = wt[i,] # gene wt número i
	y = ko[i,] # gene ko número i
	
	# Hacemos el test
	t = t.test(x, y)
	
	# Añadimos el p-value a la lista
	pvalue[i] = t$p.value
	# Añadimos las estadísticas a la lista
	tstat[i] = t$statistic
}

head(pvalue)

# Ahora comprobamos que hemos hecho TODOS los cálculos
length(pvalue)

# Hacemos un histograma de los p-values.
# ¿Qué pasa si le ponemos con una transformación de -log10?
#El primero representa los p-values brutos.
#El segundo representa los p-values transformados. Es decir, transformas (pones bonitos) los  datos.
hist(pvalue,col="gray")
hist(-log10(pvalue), col = "gray")

# Hacemos un volcano plot. Aquí podemos meter la diferencia de medias y la significancia estadística
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano")

# Queremos establecer que el mínimo para considerar una diferencia significativa, es con una diferencia de 2 y un p-value de 0.01
# ¿Puedes representarlo en el gráfico?
#Si, si puedo.
diff.mean_cutoff = 2
pvalue_cutoff = 0.01
abline(v = diff.mean_cutoff, col = "blue", lwd = 3)
#abline(v = -diff.mean_cutoff, col = "red", lwd = 3)
abline(h = -log10(pvalue_cutoff), col = "green", lwd = 3)

# Ahora buscamos los genes que satisfagan estos criterios
# Primero hacemos el filtro para la diferencia de medias (fold)
filter_by_diff.mean = abs(diff.mean) >= diff.mean_cutoff
dim(data[filter_by_diff.mean, ])

# Ahora el filtro de p-value
filter_by_pvalue = pvalue <= pvalue_cutoff
dim(data[filter_by_pvalue, ])

# Ahora las combinamos. ¿Cuántos genes cumplen los dos criterios?
#Los genes que cumplen los dos criterios son 426 genes de interes, es decir, que pasan el filtro.
filter_combined = filter_by_diff.mean & filter_by_pvalue
filtered = data[filter_combined,]
dim(filtered)
head(filtered)
#Filtered contiene todos los genes expresados de forma diferencial, nos dice cuantos genes tiene, cuantos han pasado el corte.

# Ahora generamos otro volcano plot con los genes seleccionados marcados en rojo
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano #2")
points (diff.mean[filter_combined], -log10(pvalue[filter_combined]),col = "red")

# Ahora vamos a marcar los que estarían sobreexpresados (rojo) y reprimidos (azul). ¿Por qué parece que están al revés?
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano #3")
points (diff.mean[filter_combined & diff.mean < 0],
	-log10(pvalue[filter_combined & diff.mean < 0]), col = "red")
points (diff.mean[filter_combined & diff.mean > 0],
	-log10(pvalue[filter_combined & diff.mean > 0]), col = "blue")
#Los valores rojos son sobreexpresados.
#Los valores negros son reprimidos.
#Están al reves ya que los datos sobreexpresados están marcados con valores positivos, pero en este caso es al reves, están marcados con valores negativos.
#Ocurre los mismo con los datos reprimidos, que se ven en valores positivos.

# Ahora vamos a generar un mapa. Para ello primero tenemos que hacer un cluster de las columnas y los genes 
# ¿Qué es cada parámetro que hemos usado dentro de la función heatmap?
#Los parámetros expresados son los genes diferencialmente expresados de forma que visualmente vemos los distintos blqoues de expresión.
# ¿Eres capaz de cambiar los colores del heatmap? Pista: usar el argumento col y hcl.colors
rowv = as.dendrogram(hclust(as.dist(1-cor(t(filtered)))))
colv = as.dendrogram(hclust(as.dist(1-cor(filtered))))
heatmap(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,labRow=FALSE)

heatmap(filtered)


# Ahora vamos a crear un heatmap más chulo. Para ello necesitamos dos paquetes: gplots y RcolorBrewer
#if (!requireNamespace("BiocManager"))
#    install.packages("BiocManager")
#BiocManager::install(c("gplots","RColorBrewer"))
install.packages("gplots")		
install.packages("RColorBrewer")	

library(gplots)
library(RColorBrewer)

# Hacemos nuestro heatmap
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
	col = rev(redblue(256)), scale = "row")

# Lo guardamos en un archivo PDF
pdf ("GSE5583_DE_Heatmap.pdf")
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
	col = rev(redblue(256)), scale = "row",labRow=FALSE)
dev.off()
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,col = redgreen(75), scale = "row",labRow=FALSE)

# Guardamos los genes diferencialmente expresados y filtrados en un fichero
write.table (filtered, "GSE5583_DE.txt", sep = "\t",quote = FALSE)
