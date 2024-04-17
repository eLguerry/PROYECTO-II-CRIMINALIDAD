library(readxl)
datosPersonas=read.csv("Todos_crimenes_estado_2022.csv",row.names=1,as.is=TRUE)
head(datosPersonas)
datosPersonas[,1]
head(datosPersonas)
datos <- datosPersonas[-1,-1]
head(datosPersonas)
head(datos)
datosPersonas[,c(1:5)]
summary(datosPersonas)
dfPersonas = as.data.frame(datos)
dfPersonas
library(FactoMineR)
library(factoextra)
res.pca = PCA(dfPersonas, scale.unit = TRUE, graph = FALSE, ncp = 10)
              
eig.val <- get_eigenvalue(res.pca)
VPmedio = 100 * (1/nrow(eig.val))
fviz_eig(res.pca, addlabels = TRUE) +
  geom_hline(yintercept=VPmedio, linetype=2, color="red")
#VALORES ATÍPICOS
#Distancia por T de Hotelling
K=2
misScores = res.pca$ind$coord[,1:K]
miT2 = colSums(t(misScores**2)/eig.val[1:K,1])
I = nrow(dfPersonas)
F95 = K*(I**2 - 1)/(I*(I - K)) * qf(0.95, K, I-K)
F99 = K*(I**2 - 1)/(I*(I - K)) * qf(0.99, K, I-K)
plot(1:length(miT2), miT2, type = "p", xlab = "Cereales", ylab = "T2")
abline(h = F95, col = "orange", lty = 2, lwd = 2)
abline(h = F99, col = "red3", lty = 2, lwd = 2)

#Datos anómalos(T hotelling)
anomalas = which(miT2 > F95)
(anomalas)


library(grid)
library(gridExtra)
library(tune)
#dev.off() por si se cae el terminal de gráficos

p1 = fviz_pca_ind(res.pca, axes = c(1,2), geom = c("point"),
                  habillage = factor(miT2 > F95)) +
  tune::coord_obs_pred()

p2 = fviz_pca_ind(res.pca, axes = c(1,3), geom = c("point"), 
                  habillage = factor(miT2 > F95)) +
  tune::coord_obs_pred() 


grid.arrange(p1,p2, nrow = 1)

#Funcion para calcular las contribuciónes T**2(T de Hotelling) de una o más observaciones anómalas
contribT2 = function (X, scores, loadings, eigenval, observ, cutoff = 2) {
  # X is data matrix and must be centered (or centered and scaled if data were scaled)
  misScoresNorm = t(t(scores**2) / eigenval)
  misContrib = NULL
  for (oo in observ) {
    print(rownames(misScores)[oo])
    print(misScores[oo,])
    misPCs = which(as.numeric(misScoresNorm[oo,]) > cutoff)
    lacontri = sapply(misPCs, function (cc) (misScores[oo,cc]/eigenval[cc])*loadings[,cc]*X[oo,])
    lacontri = rowSums((1*(sign(lacontri) == 1))*lacontri)
    misContrib = cbind(misContrib, lacontri)
  }
  colnames(misContrib) = rownames(misScoresNorm[observ,])
  return(misContrib)
}
#ANÁLISIS INDIVIDUAL DE LOS VALORES ANÓMALOS
#TEXAS

X=as.matrix(dfPersonas)
par(mar = c(10,2.3,3,1))
barplot(mycontrisT2[,1],las=2, #cex.names = 0.5,
        main= paste0("Observación: ", rownames(dfPersonas)[which.max(miT2)]))

misLoadings = sweep(res.pca$var$coord, 2, sqrt(res.pca$eig[1:K,1]), FUN="/")
mycontrisT2 = contribT2(X = X, scores = misScores, loadings = misLoadings, 
                        eigenval = eig.val[1:K,1], observ = which.max(miT2),
                        cutoff = 2)

#CALIFORNIA
X=as.matrix(dfPersonas)
par(mar = c(10,2.3,3,1))
barplot(mycontrisT2[,1],las=2, #cex.names = 0.5,
        main= paste0("Observación: ", rownames(dfPersonas)[anomalas[1]]))

misLoadings = sweep(res.pca$var$coord, 2, sqrt(res.pca$eig[1:K,1]), FUN="/")
mycontrisT2 = contribT2(X = X, scores = misScores, loadings = misLoadings, 
                        eigenval = eig.val[1:K,1], observ = anomalas[1],
                        cutoff = 2)


#DISTANCIA AL MODELO(SCR) (error con las dimesiones de las matrices)
myE = X - misScores %*% t(misLoadings) 
mySCR = rowSums(myE^2)  
plot(1:length(mySCR), mySCR, type = "l", main = "Distancia al modelo", 
     ylab = "SCR", xlab = "Crimenes", ylim = c(0,11))
g = var(mySCR)/(2*mean(mySCR))
h = (2*mean(mySCR)^2)/var(mySCR)
chi2lim = g*qchisq(0.95, df = h)
chi2lim99 = g*qchisq(0.99, df = h)
abline(h = chi2lim, col = "orange", lty = 2, lwd = 2)
abline(h = chi2lim99, col = "red3", lty = 2, lwd = 2)
dim(misScores)
dim(misLoadings)
misLoadings
dim(X)
head(misScores)
head(X)

#GRÁFICO LOADINGS
par(mar = c(2, 2, 2, 2))  # Establecer márgenes más pequeños
plot.new()  # Crear un nuevo dispositivo gráfico

fviz_pca_ind(res.pca, axes = c(1, 2), repel = TRUE, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), geom=c("point", "text"))


#GRÁFICO SCORES

fviz_pca_ind(res.pca, axes = c(1,2), geom = c("point", "text"), 
             habillage = "mfr", repel = TRUE, labelsize = 2)
fviz_pca_ind(res.pca, axes = c(1,2), geom = c("point", "text"), repel = TRUE, labelsize = 2,
             select.ind = list("cos2"=30), habillage = "shelf", addEllipses = TRUE)



