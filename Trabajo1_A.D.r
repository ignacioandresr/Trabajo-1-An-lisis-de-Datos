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

