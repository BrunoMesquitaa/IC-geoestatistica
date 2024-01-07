#Aulas 2023
#Exemplo 8 - geoestatística - variável dicotomica
#pacotes
require(readxl)
require(fBasics)
require(geoR)
# Carregando os dados do ex8
dados8<-read_excel("casos-multi e geo.xlsx.",sheet="geo-ex8")#leitura dos dados
attach(dados8)
names(dados8)
#
# Estatistica Descritiva 
#nao faz sentido pois as variaveis sao dicotomicas 
#
# transformando os dados em geodados
geoq1<-as.geodata(dados8,coords.col=1:2, data.col=4)
geoq2<-as.geodata(dados8,coords.col=1:2, data.col=5)
geoq3<-as.geodata(dados8,coords.col=1:2, data.col=6)
#
#
#graficos espaciais
points(geoq1)
points(geoq2)
points(geoq3)
#
#Calculando as semivariancias
semiq1 <- variog(geoq1, max.dist=50)
semiq1
semiq2 <- variog(geoq2, max.dist=50)
semiq2
semiq3 <- variog(geoq3, max.dist=50)
semiq3
#
#semivariograma experimental
plot(semiq1, main="Semivariograma medio da dicotomica para Q1")
plot(semiq2, main="Semivariograma medio da dicotomica para Q2")
plot(semiq3, main="Semivariograma medio da dicotomica para Q3")
#
#ajustando o semivariograma 
m1q1<-eyefit(semiq1)
m1q1
#esf(0.09,0.158,26.86)
plot(semiq1, main="Semivariograma medio da dicotomica para Q1")
lines(m1q1, col="blue")
modq1
#
m1q2<- eyefit(semiq2)
m1q2
#gau(0.09,0.185,14.13)
plot(semiq2, main="Semivariograma medio da dicotomica para Q2")
lines(m1q2, col="blue")
#
m1q3<- eyefit(semiq3)
m1q3
#gau(0.06,0.12,13)
plot(semiq3, main="Semivariograma medio da dicotomica para Q3")
lines(m1q3, col="blue")
#
#KRIGAGEM DA INDICADORA
# definindo os locais para para as estimativas
loci <- expand.grid(seq(0,70,1), seq(0,70,1)) #cria a malha a ser estimada
loci #mostra as coordenadas da malha a ser estimada
#
#definindo os limites da malha 
limites = matrix(c(0,0,0,70,70,70,70,0),byrow= T,ncol=2) #criou os limites. Em caso de malha irregular pode ser um arquivo com coordenadas
#
# Preditos por Krigagem Ordinaria Krigagem da indicadora usando o semivariograma modelo2
kq1 <- krige.conv(geoq1, loc=loci, krige=krige.control(type.krige = "ok", obj.model = m1q1),borders=limites)
kq1 #mostra estimativas, vari�ncias,...
#
kq2 <- krige.conv(geoq2, loc=loci, krige=krige.control(type.krige = "ok", obj.model = m1q2),borders=limites)
kq2 #mostra estimativas, vari�ncias,...
#
kq3 <- krige.conv(geoq3, loc=loci, krige=krige.control(type.krige = "ok", obj.model = m1q3),borders=limites)
kq3 #mostra estimativas, vari�ncias,...
#
#mapas das indicadora
contour(kq1, filled=TRUE, levels=seq(0.00,1.00, by=0.1))
title(main="Mapa de probabilidades menores ou igual a Q1")
#
contour(kq2, filled=TRUE, levels=seq(0.00,1.00, by=0.1))
title(main="Mapa de probabilidades menores ou igual a Q2")
#
contour(kq3, filled=TRUE, levels=seq(0.00,1.00, by=0.1))
title(main="Mapa de probabilidades menores ou igual a Q3")
