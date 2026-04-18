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
boxplot(datos$Quantity, main="Detección de Outliers (Quantity)")

boxplot(datos$Total.Spent, main="Detección de Outliers (Price.Per.Unit)")

boxplot(datos$Price.Per.Unit, main="Detección de Outliers (Total.Spent)")

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

# Medidas de Dispersión 
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
# Cantidad total vendida por categoria:
library(dplyr)
library(ggplot2)
ventas_por_categoria <- datos %>%
  group_by(Item) %>%
  summarise(Total.Spent = sum(Quantity * Price.Per.Unit))
ggplot(ventas_por_categoria, aes(x = Item, y = Total.Spent, fill = Item)) +
  # preparamos el grafico, y asignamos esteticas.
  geom_bar(stat = "identity") + # generamos el grafico de barras.
  ggtitle("Total de ventas por categoria") + # agregamos el titulo.
  xlab("Categoria") + # agregamos la etiqueta del eje x.
  ylab("Ventas totales (Ingresos)") # agregamos la etiqueta del eje y.

# Gráfico de Torta de Proporción de transacciones por método de pago 
#(NULOS FILTRADOS):
cantidad_por_metodo <- datos %>%
  filter(!is.na(Payment.Method)) %>%
  group_by(Payment.Method) %>% 
  summarise(cantidad = n())

cantidad_por_metodo <- cantidad_por_metodo %>%
  mutate(
    porcentaje = cantidad / sum(cantidad),
    etiqueta = scales::percent(porcentaje)
  )

ggplot(cantidad_por_metodo, aes(x = "", y = cantidad, fill = Payment.Method)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), 
            position = position_stack(vjust = 0.5)) +
  ggtitle("Proporción de transacciones por método de pago") +
  theme_void() 
# Gráfico de dispersión Quantity y Total.Spent

ggplot(datos, aes(x = Quantity, y = Total.Spent)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relación entre Cantidad de transacciones y Total Gastado",
       x = "Cantidad",
       y = "Total Gastado") +
  theme_minimal()

# Gráfico de Lineas, ventas por fecha.
datos$Transaction.Date <- as.Date(datos$Transaction.Date)

ventas_por_fecha <- datos %>% 
  group_by(Transaction.Date) %>% 
  summarise(Total.Spent = sum(Quantity))

ventas_por_fecha <- ventas_por_fecha %>% 
  filter(!is.na(Transaction.Date), !is.na(Total.Spent))

ggplot(ventas_por_fecha, aes(x = Transaction.Date, y = Total.Spent, group = 1)) +
  geom_line() +
  geom_point() +
  ggtitle("Ventas a lo largo del tiempo") +
  xlab("Fecha") + 
  ylab("Ventas totales")

#Boxplot Total gastado por lugar de consumo
boxplot(Total.Spent ~ Location,
        data = datos,
        main = "Total gastado por lugar de consumo",
        xlab = "Location",
        ylab = "Total Spent",
        col = c("lightblue", "lightgreen"))

# Histogramas
hist(datos$Quantity, main="Histograma de Quantity")

hist(datos$Price.Per.Unit, main="Histograma de Price.Per.Unit")

hist(datos$Total.Spent, main="Histograma de Total.Spent")

# Medidas de forma (Asimetría y Curtosis)
library(moments)
asimetria_quantity <- skewness(datos$Quantity)
curtosis_quantity <- kurtosis(datos$Quantity)

asimetria_ppu <- skewness(datos$Price.Per.Unit)
curtosis_ppu <- kurtosis(datos$Price.Per.Unit)

asimetria_totalSpent <- skewness(datos$Total.Spent)
curtosis_totalSpent <- kurtosis(datos$Total.Spent)

print(paste("Asimetría Cantidad de Ventas:", round(asimetria_quantity, 2)))
print(paste("Curtosis Cantidad de Ventas:", round(curtosis_quantity, 2)))

print(paste("Asimetría Precio por unidad:", round(asimetria_ppu, 2)))
print(paste("Curtosis Precio por unidad:", round(curtosis_ppu, 2)))

print(paste("Asimetría Total Gastado:", round(asimetria_totalSpent, 2)))
print(paste("Curtosis Total Gastado:", round(curtosis_totalSpent, 2)))


