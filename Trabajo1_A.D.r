# Ingreso al directorio
setwd("~/Documentos/GitHub/Trabajo-1-An-lisis-de-Datos")

# Importación y limpieza inicial de campos vacíos, ERROR, UNKNOWN a NA
datos <- read.csv("dirty_cafe_sales.csv", na.strings = c("", "NA", "UNKNOWN", "ERROR" ))

# Verificación de campos nulos
colSums(is.na(datos))

# Verificación del tipo de dato
str(datos)
