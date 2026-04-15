# Ingreso al directorio
setwd("~/Documentos/GitHub/Trabajo-1-An-lisis-de-Datos")

# Importación y limpieza inicial de campos vacíos, ERROR, UNKNOWN a NA
datos <- read.csv("dirty_cafe_sales.csv", na.strings = c("", "NA", "UNKNOWN", "ERROR" ))

# Verificación de campos nulos
colSums(is.na(datos))

# Verificación del tipo de dato
str(datos)

# Verificación inconsistencias
unique(datos$Item)
unique(datos$Payment.Method)
unique(datos$Location)
unique(datos$Transaction.Date)

# Detección outliers
hist(datos$Quantity, main="Distribución de cantidad de artículos")
boxplot(datos$Quantity)

hist(datos$Total.Spent, main="Distribución de total gastado")
boxplot(datos$Total.Spent)

hist(datos$Price.Per.Unit, main="Distribución de precios por unidad")
boxplot(datos$Price.Per.Unit)

# Test Shapiro para imputar nulos
vector1 <- na.omit(datos$Quantity)
muestra1 <- sample(vector1, 5000)
test_cantidad <- shapiro.test(muestra1)
print(test_cantidad)

vector2 <- na.omit(datos$Price.Per.Unit)
muestra2 <- sample(vector2, 5000)
test_precio <- shapiro.test(muestra2)
print(test_precio)

vector3 <- na.omit(datos$Total.Spent)
muestra3 <- sample(vector3, 5000)
test_total <- shapiro.test(muestra3)
print(test_total)

# Decisión Media o Mediana e Imputar nulos
if(test_cantidad$p.value > 0.05) {
  datos$Quantity[is.na(datos$Quantity)] <- mean(datos$Quantity, na.rm = TRUE)
} else {
  datos$Quantity[is.na(datos$Quantity)] <- median(datos$Quantity, na.rm = TRUE)
}

if(test_precio$p.value > 0.05) {
  datos$Price.Per.Unit[is.na(datos$Price.Per.Unit)] <- mean(datos$Price.Per.Unit, na.rm = TRUE)
} else {
  datos$Price.Per.Unit[is.na(datos$Price.Per.Unit)] <- median(datos$Price.Per.Unit, na.rm = TRUE)
}

if(test_total$p.value > 0.05) {
  datos$Total.Spent[is.na(datos$Total.Spent)] <- mean(datos$Total.Spent, na.rm = TRUE)
} else {
  datos$Total.Spent[is.na(datos$Total.Spent)] <- median(datos$Total.Spent, na.rm = TRUE)
}

# Medidas de Tendencia Central
media_quantity   <- mean(datos$Quantity)
mediana_quantity <- median(datos$Quantity)

media_priceperunit   <- mean(datos$Price.Per.Unit)
mediana_priceperunit <- median(datos$Price.Per.Unit)

media_totalspent   <- mean(datos$Total.Spent)
mediana_totalspent <- median(datos$Total.Spent)

# Medidas de Disperción 
rango_quantity    <- diff(range(datos$Quantity))
varianza_quantity <- var(datos$Quantity)
desv_est_quantity <- sd(datos$Quantity)
iqr_quantity      <- IQR(datos$Quantity)
cv_quantity       <- (desv_est_quantity / media_quantity) * 100

rango_priceperunit    <- diff(range(datos$Price.Per.Unit))
varianza_priceperunit <- var(datos$Price.Per.Unit)
desv_est_priceperunit <- sd(datos$Price.Per.Unit)
iqr_priceperunit      <- IQR(datos$Price.Per.Unit)
cv_priceperunit       <- (desv_est_priceperunit / media_priceperunit) * 100

rango_totalspent    <- diff(range(datos$Total.Spent))
varianza_totalspent <- var(datos$Total.Spent)
desv_est_totalspent <- sd(datos$Total.Spent)
iqr_totalspent      <- IQR(datos$Total.Spent)
cv_totalspent       <- (desv_est_totalspent / media_totalspent) * 100

# Visualización de Medidas
print(paste("Media de Cantidad:", round(media_quantity, 2)))
print(paste("Mediana de Cantidad:", round(mediana_quantity, 2)))
print(paste("Desviación Estándar de Cantidad:", round(desv_est_quantity, 2)))
print(paste("Coeficiente de Variación (CV) de Cantidad:", round(cv_quantity, 2), "%"))

print(paste("Media de Precio por Unidad:", round(media_priceperunit, 2)))
print(paste("Mediana de Precio por Unidad:", round(mediana_priceperunit, 2)))
print(paste("Desviación Estándar de Precio por Unidad:", round(desv_est_priceperunit, 2)))
print(paste("Coeficiente de Variación (CV) de Precio por Unidad:", round(cv_priceperunit, 2), "%"))

print(paste("Media de Total de Gasto:", round(media_totalspent, 2)))
print(paste("Mediana de Total de Gasto:", round(mediana_totalspent, 2)))
print(paste("Desviación Estándar de Total de Gasto:", round(desv_est_totalspent, 2)))
print(paste("Coeficiente de Variación (CV) de Total de Gasto:", round(cv_totalspent, 2), "%"))

# Distribuciones Numéricas
