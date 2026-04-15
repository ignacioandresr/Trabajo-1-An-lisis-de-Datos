setwd("~/Documentos/GitHub/Trabajo-1-An-lisis-de-Datos")

cafe <- read.csv("dirty_cafe_sales.csv", na.strings = c("", "NA", "UNKNOWN", "ERRROR" ))

colSums(is.na(cafe))
