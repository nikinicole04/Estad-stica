library(e1071)
library(stats)
library(plotly)

# Análisis de las variables categoricas

# Representación Tabular
ni <- table(Divert_c) # frec. absoluta
Ni <- cumsum(ni) # frec. abs. acumulada
fi <- ni / sum(ni) # frec. relativa
Fi <- cumsum(fi) # frac. relat. acumulada
Rt <- cbind(ni, Ni, fi, Fi) # Tabla
Rt

# Diagrama de Pastel
pie(fi,
    main = "Distribución de la diversion")
percent_labels <- paste(round(fi * 100, 1), "%") # Crea etiquetas de porcentaje
pie(fi,
    main = "Distribución de la diversión",
    labels = percent_labels) # Agrega las etiquetas

# Diagrama de barras
barplot(fi, 
        main = "Distribución de la diversion de los encuestados",
        xlab = "Probabilidad",
        ylab = "Frecuencia",
        col = "red")
barplot_heights <- barplot(fi, 
                           main = "Distribución de la diversión de los encuestados",
                           xlab = "Probabilidad",
                           ylab = "Frecuencia",
                           col = "red",
                           ylim = c(0, max(fi) * 1.2)) # Aumenta el límite del eje y para espacio

# Agrega las etiquetas de porcentaje sobre las barras
text(barplot_heights, fi, labels = paste(round(fi * 100, 1), "%"), pos = 3)


# ---- Análisis Variables Cuantitativas ----
# Tabla de frecuencia
n <- length(Estat)
n

# Si n < 50, k <- sqrt(n)
# Si n => 50, k <- round(1+3.322*log10(n)) Regla Sturges
k <- round(1+3.322*log10(n))
k

clase <- cut(Estat, k, right = T)
clase

ni <- table(clase)
Ni <- cumsum(ni)
fi <- ni/sum(ni)
Fi <- cumsum(fi)
Rt <- cbind(ni, Ni, fi, Fi)
Rt

# Histograma
barplot(fi,
        space = 0, 
        xlab = "Estatura",
        ylab = "Frecuencia Absoluta",
        main = "Histograma de Frecuencia para Estatura",
        col = "blue")

var <- Estat
var <- sort(var)
var

mean(var) # media
median(var) # mediana

quantile(var) # cuantiles (Pn, Qn, Qt)
quantile(var, c(0.01, 0.05, 0.10, 0.95, 0.99))
min(var)
max(var)

rango <- max(var) - min(var)
rango
IQR(var)
var(var)
sd(var)
Cv <- sd(var) / mean(var)
Cv

skewness(var)
kurtosis(var)

# gráficos complementarios
# Diagrama de caja y bigotes
boxplot(Estat,
        main = "Diagrama de Caja y Bigotes para Estatura",
        ylab = "Estatura",
        col = "lightblue",
        notch = TRUE)

p <- plot_ly(y = ~Estat, 
             type = "box", 
             name = "Estatura",
             boxpoints = "all", 
             jitter = 0.3, 
             pointpos = -1.8,
             marker = list(color = 'rgb(7,40,89)'),
             line = list(color = 'rgb(7,40,89)')) %>%
  layout(title = "Diagrama de Caja y Bigotes para Estatura",
         yaxis = list(title = "Estatura"),
         xaxis = list(title = ""))

p
