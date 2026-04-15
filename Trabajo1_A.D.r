# Ingreso al directorio
setwd("~/Documentos/GitHub/Trabajo-1-An-lisis-de-Datos")

# Importación y limpieza inicial de campos vacíos, ERROR, UNKNOWN a NA
datos <- read.csv("dirty_cafe_sales.csv", na.strings = c("", "NA", "UNKNOWN", "ERROR" ))

# Verificación de campos nulos
colSums(is.na(datos))

# Verificación del tipo de dato
str(datos)

#Verificación inconsistencias
unique(cafe$Item)
unique(cafe$Payment.Method)
unique(cafe$Location)
unique(cafe$Transaction.Date)

#Detección outliers
hist(cafe$Price.Per.Unit, main="Distribución de cantidad de artículos")
boxplot(cafe$Quantity)

hist(cafe$Price.Per.Unit, main="Distribución de total gastado")
boxplot(cafe$Total.Spent)

hist(cafe$Price.Per.Unit, main="Distribución de precios por unidad")
boxplot(cafe$Price.Per.Unit)

#Test Shapiro para imputar nulos
vector1 <- na.omit(cafe$Quantity)
muestra1 <- sample(vector1, 5000)
shapiro.test(muestra1)

vector2 <- na.omit(cafe$Price.Per.Unit)
muestra2 <- sample(vector2, 5000)
shapiro.test(muestra2)

vector3 <- na.omit(cafe$Total.Spent)
muestra3 <- sample(vector3, 5000)
shapiro.test(muestra3)
