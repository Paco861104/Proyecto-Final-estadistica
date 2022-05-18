
library(ggplot2)
library(ggfortify)
datos <- read.csv("../../data/ClaseEstadistica/BD proyecto final/GeoClimRiq_Traits.csv", stringsAsFactors = TRUE)



                               ##### PCA #####
dataPCA <- datos[c(27:38)]
pca <- prcomp(dataPCA, scale = TRUE, center = TRUE)
summary(pca)
names(pca)
# Ahora hay que hacer esto:
pca$rotation <- -pca$rotation
pca$x        <- -pca$x

datos$RIQUEZASPP <- as.factor(datos$RIQUEZASPP)
autoplot(pca, data=datos, main = "PCA",
         loadings.label.size=4.5,
         colour="ESPECIE", shape="RIQUEZASPP", size = 2.8,
         loadings=TRUE, loadings.label.vjust = 3.1,
         loadings.colour= '8', loadings.label.colour= '1',
         loadings.label= TRUE) +
  scale_shape_manual(values=c(17,15,18,16,15,17,18,16,18,17)) +
  scale_color_manual(values = c("darkcyan","red","chartreuse3","cornflowerblue",
                                "brown","6","darkgoldenrod1",
                                "blue","9","darkslategray3"))



                         ##### CORRELACIONES #####
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

pairs(datos[,27:38],
      upper.panel = panel.cor,    # Panel de correlaci?n
      lower.panel = panel.smooth)


cor_datos <- cor(datos[,27:38], method = "pearson")
cor_datos


# correlaciones pareadas. r > 0.5 # (nos quedaríamos con clorofila, AFE, grosor,
# densidad estomát, longit estomát, Ct, Nt, Pt...???)
cor.test(datos$CLOROFILA, datos$AFE) #-0.507
cor.test(datos$AFE, datos$GROSOR) #-0.54 
cor.test(datos$DENS_ESTOM, datos$LONG_ESTOM) #-0.638 
cor.test(datos$Ct, datos$Nt) #-0.513  
cor.test(datos$Nt, datos$Pt) #0.783
                 
