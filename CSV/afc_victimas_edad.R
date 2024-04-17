Edad_victimas=read.csv("Victimas_por_edad_2022.csv")
Raza_victimas=read.csv("Victimas_por_raza_2022.csv")
Genero_victimas=read.csv("Victimas_por_genero_2022.csv")

head(Edad_victimas)
library(dplyr)
library(readr)

#Ponemos de nombre de filas el tipo de delito
rownames(Edad_victimas) = Edad_victimas[,1] 

#Quitamos la primera y la ultima columna
Edad_victimas <- subset(Edad_victimas, select = -1)

Edad_victimas<- Edad_victimas[, -c(1)]

Edad_victimas<- Edad_victimas[-c(1),]

#Cambiamos el nombre a las columnas

colnames(Edad_victimas) = c("0-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50","51-55","56-60","61-65","66+")

##Hacemos el AFC
library(factoextra)
library(FactoMineR)
library(knitr)
res.afc = CA(Edad_victimas, graph = FALSE)
eig.val <- get_eigenvalue(res.afc)
Vmedia = 100 * (1/nrow(eig.val))
fviz_eig(res.afc, addlabels = TRUE) +
  geom_hline(yintercept=Vmedia, linetype=2, color="red")

#Seleccionamos 2 componentes
K=2
res.afc = CA(Edad_victimas, graph = TRUE, ncp = K)
p= plot(res.afc, cex = 0.8)

library(ggrepel)
p + geom_text_repel(max.overlaps = 300)


fviz_ca_row(res.afc, axes = c(1,2), repel = TRUE)

fviz_ca_col(res.afc, axes = c(1,2), repel = TRUE)

fviz_contrib(res.afc, choice = "row", axes = c(1))



