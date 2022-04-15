# CARGAR LIBRERIAS ----------------------------------------------------
library(readxl)
library(Rcpp)
library(arrow)
library(packcircles)
library(viridis)
library(ggplot2)
library(tidyr)

# CARGAR BASE DATOS---------------------------------------------------

#file.choose()
d<-read_excel("C:\\Users\\MiguelAngel\\Documents\\R Miguelo\\Guillermo (Circular Packing)\\oms.xls")

head(d)
View(d)

d<-d[,-1]

# VERIFICAR [NAs] ------------------------------------------------------------

sapply(d, function(x) sum(is.na(x)))

d2 <- d[!is.na(d$Siglas),]

sapply(d2, function(x) sum(is.na(x)))


# CONSTRUCCION DE GRÁFICO -----------------------------------------------

d3 <- circleProgressiveLayout(d2$Frecuencia, sizetype='area')

d4 <- cbind(d2$Siglas, d2$Frecuencia , d3)

names(d4)

names(d4)[1] = "Siglas"
names(d4)[2] = "Frecuencia"

names(d4)

# CREACION DE VERTICES -----------------------------------------------

d5 <- circleLayoutVertices(d3, npoints=30)
head(d5)

# GRAFICO CIRCULAR --------------------------------------------------------

p<-ggplot() + 
  geom_polygon(data = d5, aes(x, y, group = id, fill=as.factor(id)), 
               colour = "gray8",size=0.34, alpha = 0.65) +
  geom_text(data = d4, aes(x, y, size=Frecuencia, label=Siglas),size=2.0) +
  scale_size_continuous(range = c(3,5)) +
  theme_void() + 
  theme(legend.position="none",
        plot.background = element_rect(fill="gray93"),
        plot.title = element_text(color="black")) +
  coord_equal()+
  ggtitle("Participacion organizaciones sindicales en Mexico")


