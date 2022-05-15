
install.packages("carData")
install.packages("car")
install.packages("agricolae")
library(agricolae)
library(car)
library(tidyverse)
library(ggplot2)
library(ggfortify)
getwd()
dir()
indiv <- read.csv("../../data/ClaseEstadistica/BD proyecto final/BD_Individuos!.csv", stringsAsFactors = TRUE)
pops <- read.csv("../../data/ClaseEstadistica/BD proyecto final/BD_pops!.csv")
geoclim_riq <- read.csv("../../data/ClaseEstadistica/BD proyecto final/GeoClimRiqueza.csv", stringsAsFactors = TRUE)

indiv %>%               #para ver la mean y la suma total de clorofila por spp
  group_by(ESPECIE) %>%
  summarize(mean = mean(CLOROFILA),
            sum = sum(CLOROFILA))




boxplot(indiv$CLOROFILA~indiv$ESPECIE)
cloranova <- aov(CLOROFILA~ESPECIE, data=indiv)  #si cumple supuestos
shapiro.test(cloranova$residuals)
leveneTest(CLOROFILA~ESPECIE, data=indiv)
summary(cloranova)
hist(cloranova$residuals)
#qqnorm(cloranova$residuals)
qqPlot(cloranova$residuals)
LSD.clorof <- LSD.test(y = cloranova, trt = "ESPECIE", DFerror = cloranova$df.residual,
                        MSerror = deviance(cloranova)/cloranova$df.residual, alpha = 0.05,group = TRUE,
                        console = TRUE)
plot(LSD.clorof)
#Obtener el valor máximo de cada atributo funcional
value_max <- indiv %>% 
  group_by(ESPECIE) %>% 
  summarise(max_value= max(CLOROFILA))
#anova del atributo por especie
cloranova <- aov(CLOROFILA~ESPECIE, data=indiv)
#comparaciones múltiples
LSD.especie <- LSD.test(y = cloranova, trt = "ESPECIE", DFerror = cloranova$df.residual,
                        MSerror = deviance(cloranova)/cloranova$df.residual, alpha = 0.05,group = TRUE,
                        console = TRUE)
#obtener letras y ordenarlas alfabéticamente
sig.letters <- LSD.especie$groups[order(row.names(LSD.especie$groups)), ]
#gráfica
ggplot(indiv, aes(x=ESPECIE, y=CLOROFILA)) +
  geom_boxplot() +
  geom_text(data=value_max, aes(x=ESPECIE, y=0.4 + max_value, vjust=0, label= sig.letters$groups))+
  theme_bw()













boxplot(indiv$AFE~indiv$ESPECIE)
afeanova <- aov(sqrt(AFE)~ESPECIE, data=indiv)  #con transformac cumplió supuestos
shapiro.test(afeanova$residuals)
leveneTest(sqrt(AFE)~ESPECIE, data=indiv)
hist(afeanova$residuals)
qqPlot(afeanova$residuals)
LSD.afe <- LSD.test(y = afeanova, trt = "ESPECIE", DFerror = afeanova$df.residual,
                       MSerror = deviance(afeanova)/afeanova$df.residual, alpha = 0.05,group = TRUE,
                       console = TRUE)
plot(LSD.afe)
#Obtener el valor máximo de cada atributo funcional
value_max <- indiv %>% 
  group_by(ESPECIE) %>% 
  summarise(max_value= max(AFE))
#anova del atributo por especie
afeanova <- aov(AFE~ESPECIE, data=indiv)
#comparaciones múltiples
LSD.especie <- LSD.test(y = afeanova, trt = "ESPECIE", DFerror = afeanova$df.residual,
                        MSerror = deviance(afeanova)/afeanova$df.residual, alpha = 0.05,group = TRUE,
                        console = TRUE)
#obtener letras y ordenarlas alfabéticamente
sig.letters <- LSD.especie$groups[order(row.names(LSD.especie$groups)), ]
#gráfica
ggplot(indiv, aes(x=ESPECIE, y=AFE)) +
  geom_boxplot() +
  geom_text(data=value_max, aes(x=ESPECIE, y=0.4 + max_value, vjust=0, label= sig.letters$groups))+
  theme_bw()










boxplot(indiv$TANINOS~indiv$ESPECIE)
tananova <- aov(sqrt(TANINOS)~ESPECIE, data=indiv)  #con transformación ya cumplió normalidad
shapiro.test(tananova$residuals)                    #levene estaba bien pero tuve q transformar también
leveneTest(sqrt(TANINOS)~ESPECIE, data=indiv)
hist(tananova$residuals)



boxplot(indiv$PH~indiv$ESPECIE)
phanova <- aov(sqrt(PH)~ESPECIE, data=indiv)  #no cumple supuestos
shapiro.test(phanova$residuals)
leveneTest(sqrt(PH)~ESPECIE, data=indiv)
hist(phanova$residuals)

boxplot(indiv$GROSOR~indiv$ESPECIE)
grosanova <- aov(sqrt(GROSOR)~ESPECIE, data=indiv)  #no cumple supuestos
shapiro.test(grosanova$residuals)
leveneTest(sqrt(GROSOR)~ESPECIE, data=indiv)
hist(grosanova$residuals)

boxplot(indiv$DENS_ESTOM~indiv$ESPECIE)
densestanova <- aov(sqrt(DENS_ESTOM)~ESPECIE, data=indiv)  #no cumple supuestos
shapiro.test(densestanova$residuals)
leveneTest(sqrt(DENS_ESTOM)~ESPECIE, data=indiv)
hist(densestanova$residuals)




boxplot(indiv$LONG_ESTOM~indiv$ESPECIE)
longanova <- aov(LONG_ESTOM~ESPECIE, data=indiv)  #no cumple supuestos. cerca pero no
shapiro.test(longanova$residuals)
leveneTest(LONG_ESTOM~ESPECIE, data=indiv)
hist(longanova$residuals)


boxplot(indiv$AGUA~indiv$ESPECIE)
aguanova <- aov(log10(max(AGUA+1)-AGUA) ~ ESPECIE, data=indiv)  #no cumple normalidad pero si homogen
shapiro.test(aguanova$residuals)
leveneTest(AGUA~ESPECIE, data=indiv)
hist(aguanova$residuals)
#intentado conversiones:
sqrt(max(x+1) - x)
log10(max(x+1) - x) f







boxplot(pops$Ct~pops$Especie)
Ctanova <- aov(Ct~Especie, data=pops)  #cumple normalidad. error en homogen
shapiro.test(Ctanova$residuals)
leveneTest(Ct~Especie, data=pops)
hist(Ctanova$residuals)

boxplot(pops$Nt~pops$Especie)
Ntanova <- aov(Nt~Especie, data=pops)  #cumple normalidad. error en homogen
shapiro.test(Ntanova$residuals)
leveneTest(Nt~Especie, data=pops)
hist(Ntanova$residuals)

boxplot(pops$Pt~pops$Especie)
Ptanova <- aov(Pt~Especie, data=pops)  #cumple normalidad. error en homogen
shapiro.test(Ptanova$residuals)
leveneTest(Pt~Especie, data=pops)
hist(Ptanova$residuals)








boxplot(pops$Clorofila~pops$Especie)
cloranova <- aov(CLOROFILA~ESPECIE, data=pops)  #si cumple supuestos
shapiro.test(cloranova$residuals)
leveneTest(CLOROFILA~ESPECIE, data=pops)
summary(cloranova)
hist(cloranova$residuals)
#qqnorm(cloranova$residuals)
qqPlot(cloranova$residuals)
LSD.clorof <- LSD.test(y = cloranova, trt = "ESPECIE", DFerror = cloranova$df.residual,
                       MSerror = deviance(cloranova)/cloranova$df.residual, alpha = 0.05,group = TRUE,
                       console = TRUE)
plot(LSD.clorof)
#Obtener el valor máximo de cada atributo funcional
value_max <- pops %>% 
  group_by(ESPECIE) %>% 
  summarise(max_value= max(CLOROFILA))
#anova del atributo por especie
cloranova <- aov(CLOROFILA~ESPECIE, data=pops)
#comparaciones múltiples
LSD.especie <- LSD.test(y = cloranova, trt = "ESPECIE", DFerror = cloranova$df.residual,
                        MSerror = deviance(cloranova)/cloranova$df.residual, alpha = 0.05,group = TRUE,
                        console = TRUE)
#obtener letras y ordenarlas alfabéticamente
sig.letters <- LSD.especie$groups[order(row.names(LSD.especie$groups)), ]
#gráfica
ggplot(pops, aes(x=ESPECIE, y=CLOROFILA)) +
  geom_boxplot() +
  geom_text(data=value_max, aes(x=ESPECIE, y=0.4 + max_value, vjust=0, label= sig.letters$groups))+
  theme_bw()




# 1.- INTRODUCCION.- IMPORTANCIA CARACTERES FUNCIONALES COMO DETERMINANTES DE COMPOSICION
#DE COMUNIDADES DE ENDÓFITOS. SE SABE POR LITERATURA QUE TALES VARIABLES PODRIAN ESTAR RELACIONADAS

# 2.- PCA PARA VER DE MANERA GENERAL LA AFINIDAD DE ESPECIES HACIA LOS TRAITS
#Hay que hacer una transformacion a normalidad antes de meter los datos al PCA???

# 3.- MATRIZ DE CORRELACIONES PARA EMPEZAR A IDEAR MODELOS

# 4.- MODELOS LINEALES MIXTOS (ANIDAMIENTO POR SPP DE ENCINO). clima, alt, riq


##### 2.- PCA #####
dataPCA <- pops[c(7:17)]
#dataPCA <- na.omit(dataPCA)
pca <- prcomp(dataPCA, scale = TRUE)
summary(pca)
names(pca)
# Ahora hay que hacer esto:
pca$rotation <- -pca$rotation
pca$x        <- -pca$x

pops$RIQUEZA <- as.factor(pops$RIQUEZA)
autoplot(pca, data=pops, main = "PCA",
         loadings.label.size=4.5,
         colour="ESPECIE", shape="RIQUEZA", size = 2.8,
         loadings=TRUE, loadings.label.vjust = 3.1,
         loadings.colour= '8', loadings.label.colour= '1',
         loadings.label= TRUE) +
  scale_shape_manual(values=c(17,15,18,16,15,17,18,16,18,17)) +
  scale_color_manual(values = c("darkcyan","red","chartreuse3","cornflowerblue",
                                "brown","6","darkgoldenrod1",
                                "blue","9","darkslategray3"))


##### 3.- MATRIZ DE CORRELACIONES #####
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Elimina la función abs si lo prefieres
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Escala el texto al nivel de correlaci?n
}

pairs(pops[,7:17],
      upper.panel = panel.cor,    # Panel de correlaci?n
      lower.panel = panel.smooth)


pops_mocho <- pops[c(7:17)]
pops_mocho <- na.omit(pops_mocho)
cor_pops<- cor(pops_mocho, method = "pearson")
cor_pops
write.csv(cor_pops, "matriz_correlac.csv") #para ver más o menos por pares

#correlaciones por pares aparte
cor.test(pops$CLOROFILA, pops$AFE) #-0.507
cor.test(pops$CLOROFILA, pops$DENS_ESTOM) #-0.37
cor.test(pops$CLOROFILA, pops$TANINOS) #-0.48
cor.test(pops$AFE, pops$GROSOR) #-0.54
cor.test(pops$AFE, pops$AGUA) #0.472
cor.test(pops$AFE, pops$DENS_ESTOM) #0.356
cor.test(pops$AFE, pops$Ct) #-0.359
cor.test(pops$AFE, pops$Nt) #0.454
cor.test(pops$AFE, pops$Pt) #0.361
cor.test(pops$GROSOR, pops$AGUA) #-0.455
cor.test(pops$AGUA, pops$DENS_ESTOM) #0.321
cor.test(pops$DENS_ESTOM, pops$LONG_ESTOM) #-0.638
cor.test(pops$DENS_ESTOM, pops$Ct) #-0.318
cor.test(pops$DENS_ESTOM, pops$Nt) #0.356
cor.test(pops$DENS_ESTOM, pops$Pt) #0.357
cor.test(pops$LONG_ESTOM, pops$Ct) #0.32
cor.test(pops$Ct, pops$Nt) #-0.513
cor.test(pops$Nt, pops$Pt) #0.783
cor.test(pops$CLOROFILA, pops$LONG_ESTOM) #0.262
cor.test(pops$TANINOS, pops$PH) #-0.29

### 4.- MODELOS generalizados mixtos. 
#ANOVA de modelos para comparar estos y decidir
#AIC (frecuenctista, menor es mejor) vs 
#BIC (bayesiano, menor es mejor, se saca con ANOVA) - Ho- efecto de 0. Pi()Chiq


# Luego de tener el modelo "super cool" ver efecto del clima y altit y riqueza



#