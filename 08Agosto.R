################################################################################
##############################CÍRCULO DE ESTUDIO 1##############################
################################################################################
library(dplyr)
library(DescTools)
library(ggplot2)
library(reshape)

### 1) ANÁLISIS EXPLORATORIO DE DATOS ###
## 1.a) Carga de dataset
setwd("~/BEDU/FASE2/DATA")
df <- read.csv("telecom_service.csv")
View(df)

lapply(df, is.null)
lapply(df, class)

df <- mutate(df, international_plan = factor(international_plan),
             voice_mail_plan = factor(voice_mail_plan),
             churn = factor(churn))

lapply(df, class)

## 1.b) Análisis de datos 
# 1.b.1) Análisis estadísticos
# Resumen descriptivo
summary(df)

# Medidas de tendencia central
mean(df$total_day_calls)
mean(df$total_day_calls, trim = 0.20)

median(df$total_day_calls)

Mode(df$total_day_calls)

# Medidas de dispersión
var(df$total_day_calls)
sd(df$total_day_calls)

IQR(df$total_day_calls) #Dispersión alrededor a la mediana
(iqr = quantile(df$total_day_calls, probs = 0.75) - quantile(df$total_day_calls, probs = 0.25))

# Medidas de posición (CuaNtiles)
# CuaRtiles (Separan la distribución de los datos en 4 partes de 25% cada una)
# q1, q2 y q3
# Deciles (Separan la distribución de los datos en 10 partes de 10% cada una)
# d1, d2, d3, d4, d5, d6, d7, d8, d9
# Percentiles o centiles (Separan la distribución de los datos en 100 partes de 1% cada una) 
# p1, p2, p3, ... p10, ... p20, ..., p25, ... , p50, p75, ... p99

# Equivalencias
# d1 = p10 (En general dk = p(k*10) para toda k = 1, ... 9)
# q1 =  p25
# q2 = d5 = p50 (= mediana)
# q3 = p75

(cuartiles <- quantile(df$total_day_calls, probs = c(0.25, 0.50, 0.75)))

(deciles <-quantile(df$total_day_calls, probs = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)))

(percentiles <- quantile(df$total_day_calls, probs = seq(0.01,0.99, by=0.01)))

# Extra: Coeficiente de correlaciÃ³n
cor(df$total_day_calls, df$customer_service_calls)
corr_matrix <- round(cor(df[,c(-1,-2,-17)]),4)
corr_matrix <- melt(corr_matrix)
corr_matrix

ggplot(data = corr_matrix, aes(x=X1, y=X2, fill=value)) + 
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# 1.b.2) Análisis gráfico
# Boxplot
boxplot(df$total_intl_charge)

ggplot(df, aes(x = churn, y = total_intl_charge)) +
  geom_boxplot() +
  ggtitle("Boxplot") +
  xlab("Churn") +
  ylab("Total day calls")

ggplot(df, aes(x = international_plan, y = total_intl_charge)) +
  geom_boxplot() +
  ggtitle("Boxplot") +
  xlab("International_plan") +
  ylab("Total day international calls")

# Histograma
hist(df$total_intl_charge, prob=T, main="Histograma total cargos internacionales")

ggplot(df, aes(total_intl_charge)) + 
  geom_histogram(colour = 'orange', 
                 fill = 'orange',
                 alpha = 0.7, # Transparencia del color fill
                 binwidth = 0.5) + 
  geom_vline(xintercept = mean(df$total_intl_charge), linetype="dashed", color = "black") + 
  ggtitle('Histograma') + 
  labs(x = 'Total cargos internacionales', y = 'Frecuencia')+
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

### 2) PROBABILIDAD
media <- mean(df$total_intl_charge)
ds <- sd(df$total_intl_charge)
n <- length(df$total_intl_charge)

# ¿Cuál es la probabilidad de que el total de cargos internacionales sea menor a 1.85 usd?
pnorm(1.85, mean=media, sd=ds)
z <- (1.85-media)/ds
pt(z, df = n-1) #La dist. t de Studen se aproxima a la normal estándar a medida que n tiende a inf

# ¿Cuál es la probabilidad de que el total de cargos internacionales sea mayor a 3 usd?
pnorm(3, mean=media, sd=ds, lower.tail=F)
z <- (3-media)/ds
pt(z, df = n-1, lower.tail = F)

# ¿Cuál es la probabilidad de que el total de cargos internacionales esté entre 2.35usd y 4.85 usd?
pnorm(4.85, mean=media, sd=ds) - pnorm(2.35, mean=media, sd=ds)
z1 <- (2.35-media)/ds
z2 <- (4.85-media)/ds
pt(z2, df = n-1) - pt(z1, df = n-1)

#Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales más alto que podría esperar?
qnorm(p=0.48, mean=media, sd=ds)


# Â¿Cuáles son los valores del total de cargos internacionales que dejan exactamente al centro el 80% de probabilidad?
qnorm(p=0.10, mean=media, sd=ds)
qnorm(p=0.90, mean=media, sd=ds) #Alt: qnorm(p=0.10, mean=media, sd=ds, lower.tail = F)
  # ComprobaciÃ³n
  pnorm(3.73058, mean=media, sd=ds) - pnorm(1.798583, mean=media, sd=ds)
  
### 3) CONTRASTE DE HIPóTESIS ###
# 3.a) Inferencia a una población
# Un estudio anterior de telecomunicaciones, señala que, en promedio el total de llamadas internacionales
# es menor a 4.54. Â¿A qué NC EEE para concluir que lo mismo sucede en nuestro mercado?
# PASO 1: Planteamiento de hipotesis:
# H_nula: 
# H_alt: 
  
# PASO 2: Calcular estad?stico de prueba:
# t = (media-mu0)/(ds(sqrt(n)))
media <- mean(df$total_intl_calls)
ds <- sd(df$total_intl_calls)
n <- length(df$total_intl_calls)

t <- (media-4.54)/(ds/sqrt(n))
t

gl <- n - 1
  
# PASO 3: Calcular P-Value
pvalue <- pt(t, df = gl, lower.tail = T)
pvalue  

# PASO 4: Seleccionar nivel de confianza y concluir
# Usualmente se definen niveles de significancia est?ndar: 0.1, 0.05 o 0.01
# Si Pvalue < significancia, se rechaza H_nula
  
# Forma directa:
t.test(x=df$total_intl_calls, alternative = 'less', mu=4.54)

# El mismo estudio, seÃ±ala que, en promedio el nÃºmero de mensajes de voz
# es mayor a 7.79 A un NC del 95%, Â¿EEE para concluir que lo mismo sucede en nuestro mercado?
t.test(x=df$number_vmail_messages, alternative = 'greater', mu=7.79)

# El mismo estudio, seÃ±ala que, sl promedio de llamadas de atenciÃ³n a clientes
# en los usuarios que cancelaron el servicio es mayor que los que no cancelaron
# A un NC del 90%, Â¿EEE para concluir que lo mismo sucede en nuestro mercado?
# PASO 1: Planteamiento de hipotesis:
# H_nula: 
# H_alt: 

# PASO 2: Calcular estad?stico de prueba:
# Definamos m1 como customer_service_callsPromedio_churn y m2 como customer_service_callsPromedio_nochurn
m1 <- mean(df[df$churn == 1, "customer_service_calls"])
m2 <- mean(df[df$churn == 0, "customer_service_calls"])

var1 <- var(df[df$churn == 1, "customer_service_calls"])
var2 <- var(df[df$churn == 0, "customer_service_calls"])

n1 <- length(df[df$churn == 1, "customer_service_calls"])
n2 <- length(df[df$churn == 0, "customer_service_calls"])

t <- (m1-m2-0)/(sqrt(((n1-1)*var1+(n2-1)*var2)/(n1+n2-2))*sqrt(1/n1+1/n2))
gl <- n1 + n2 - 2

# PASO 3: Calcular P-Value
pvalue <- pt(t, df = gl, lower.tail = F)

# PASO 4: Seleccionar nivel de confianza y concluir
# Usualmente se definen niveles de significancia est?ndar: 0.1, 0.05 o 0.01
# Si Pvalue < significancia, se rechaza H_nula
t.test(x = df[df$churn == 1, "customer_service_calls"], y = df[df$churn == 0, "customer_service_calls"],
       alternative = "greater",
       mu = 0, paired = FALSE, var.equal = TRUE)
