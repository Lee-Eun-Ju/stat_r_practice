###Multivariate Statistics (I) Term Project 
###201811526 이은주 
setwd("C:\\Users\\eunju\\Desktop\\junior\\TT10.30 다변량통계학\\Term Project")

###data
data = read.csv("Underdeveloped Countries.csv", header=TRUE)
head(data)
summary(data)

X = data[,-1]
rownames(X) = data[,1]

X$exports = X$exports*X$gdpp/100
X$imports = X$imports*X$gdpp/100
X$health = X$health*X$gdpp/100

n=dim(X)[1]; p=dim(X)[2]
Z=scale(X, scale=T)

###1. Multivariate Normality
S=cov(X)
xbar=colMeans(X)
m=mahalanobis(X, xbar, S)
m=sort(m)
id=seq(1, n)
pt=(id-0.5)/n
q=qchisq(pt, p)
plot(q, m, pch="*", xlab="Quantile", ylab="Ordered Squared Distance")
abline(0, 1)
rq=cor(cbind(q, m))[1,2]
rq

library(MVN)
mvn(X, mvnTest="mardia", multivariatePlot = "qq")

###2. PCA
R=round(cor(X),3)
R
eigen=eigen(R)
round(eigen$values,2)
V=round(eigen$vectors,2)
V

gof=eigen$values/sum(eigen$values)*100 
round(gof, 2)
plot(eigen$values, type="b", main="Scree Graph", 
     xlab="Component Number", ylab="Eigenvalue")
V2=V[,1:2]
rownames(V2) = colnames(X); colnames(V2) = c("PC1","PC2")
V2

#PCs scores
PC=Z%*%V2
round(PC, 3)

plot(PC[,1], PC[,2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC",
     xlim=c(-6.5,4), ylim=c(-4,2))
text(PC[,1], PC[,2], labels=rownames(X), cex=0.6, col="blue", pos=1)
abline(v=0, h=0)

#Biplot
n= nrow(X)
joinnames= c(rownames(X),colnames(X))
Z=scale(X, scale=T); svd.Z <- svd(Z) 
U <- svd.Z$u
V <- svd.Z$v 
D <- diag(svd.Z$d)
G <- (sqrt(n-1)*U)[,1:2]
H <- (sqrt(1/(n-1))*V%*%D)[,1:2] 
C <- rbind(G, H)
rownames(G)<-rownames(X)
rownames(H)<-colnames(X)
rownames(C)<-joinnames

eig <- (svd.Z$d)^2 
per <- eig/sum(eig)*100;  per
gof <- sum(per[1:2])

lim<-range(pretty(G))
biplot(G[,1:2],H[,1:2], xlab="1st PC", ylab="2nd PC", main="biplot function",
       xlim=lim,ylim=lim,cex=0.6,pch=16)
abline(v=0,h=0)
biplot(G[,1:2],H[,1:2], xlab="1st PC", ylab="2nd PC", main="biplot function",
       xlim=c(-4,4),ylim=c(-2.5,2),cex=0.6,pch=16)
abline(v=0,h=0)

###PCFA
library(psych)
pcfa<-principal(Z, nfactors=2, rotate="varimax")

round(pcfa$values, 2)
gof=pcfa$values/p*100; round(gof, 3)
plot(pcfa$values, type="b", main="Scree Graph", 
     xlab="Component Number", ylab="Eigenvalue")

L=pcfa$loading[,1:2]; round(L, 3)
round(diag(L%*%t(L)), 3) 
Psi=pcfa$uniquenesses
round(Psi,2)
R=cor(X)
Rm = R-(L%*%t(L) + diag(Psi))
round(Rm, 2)

#factor loadings
lim<-range(pretty(L))
plot(L[,1], L[,2],main="PC Factor Loadings : f1 and f2",  
     xlab="f1", ylab="f2", xlim=lim, ylim=lim)
text(L[,1], L[, 2], labels=rownames(L), cex=0.6, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, L[,1], L[, 2], col=2, code=2, length=0.1)

#factor scores
fpc=pcfa$scores
round(fpc, 3)
plot(fpc[,1], fpc[,2],main="Factor Scores : pc f1 and f2", 
     xlim=c(-1,2), ylim=c(-2,4))
text(fpc[,1], fpc[,2], labels=rownames(fpc), cex=0.6, col="blue", pos=1)
abline(v=0, h=0)

#Biplot
svd.Z=svd(Z)
U=svd.Z$u
V=svd.Z$v 
D <- diag(svd.Z$d)
F <- (sqrt(n-1)*U)[,1:2] 
L <- (sqrt(1/(n-1))*V%*%D)[,1:2]
C <- rbind(F, L)
rownames(F)<-rownames(X);
rownames(L)<-colnames(X)

eig <- (svd.Z$d)^2 
per <- eig/sum(eig)*100;  per
gof <- sum(per[1:2])

varimax<-varimax(L)
Lt = varimax$loadings 
T=varimax$rotmat; T
Ft= F%*%T

biplot(Ft[,c(1,2)],Lt[,c(1,2)], xlab="f1",ylab="f2", 
       main="Varimax Rotated Biplot : f1 and f2", 
       xlim=c(-2,4),ylim=c(-3.5,1.5),cex=0.6,pch=16)
abline(v=0,h=0)

###CA
#Hierarchical clustering methods - Ward linkage
ds = dist(fpc, method="euclidean")
ward = hclust(ds, method="ward.D2")
plot(ward, hang=-1, labels=rownames(X), cex=0.5)

#Non-hierarchical clustering methods - K-means
library(NbClust)
all<-NbClust(fpc, distance="euclidean", min.nc = 2, max.nc = 10,
             method = "kmeans", index = "all")

kmeans <- kmeans(fpc, 4) 
cluster=data.frame(rownames(fpc), cluster=kmeans$cluster)
C1=cluster[(cluster[,2]==1),]
C2=cluster[(cluster[,2]==2),]
C3=cluster[(cluster[,2]==3),]
C4=cluster[(cluster[,2]==4),]
C1;C2;C3;C4

aggregate(fpc, by=list(kmeans$cluster), FUN=mean)

###Conclusion
G1 = matrix(0,0,2)
for (i in 1:n){
        for (j in 1:dim(C1)[1]){
                if (rownames(fpc)[i]==rownames(C1)[j]){
                        G1 = rbind(G1,fpc[i,])
                }
        }
}
rownames(G1) = rownames(C1)
plot(G1[,1], G1[,2],main="Group1 Factor Scores", 
     xlim=c(-1,2), ylim=c(-2,4))
text(G1[,1], G1[,2], labels=rownames(G1), cex=0.6, col="blue", pos=1)
abline(v=0, h=0)

G2 = matrix(0,0,2)
for (i in 1:n){
        for (j in 1:dim(C2)[1]){
                if (rownames(fpc)[i]==rownames(C2)[j]){
                        G2 = rbind(G2,fpc[i,])
                }
        }
}
rownames(G2) = rownames(C2)
plot(G2[,1], G2[,2],main="Group2 Factor Scores", 
     xlim=c(-1,2), ylim=c(-2,4))
text(G2[,1], G2[,2], labels=rownames(G2), cex=0.6, col="blue", pos=1)
abline(v=0, h=0)

G3 = matrix(0,0,2)
for (i in 1:n){
        for (j in 1:dim(C3)[1]){
                if (rownames(fpc)[i]==rownames(C3)[j]){
                        G3 = rbind(G3,fpc[i,])
                }
        }
}
rownames(G3) = rownames(C3)
plot(G3[,1], G3[,2],main="Group3 Factor Scores", 
     xlim=c(-1,2), ylim=c(-2,4))
text(G3[,1], G3[,2], labels=rownames(G3), cex=0.6, col="blue", pos=1)
abline(v=0, h=0)

G4 = matrix(0,0,2)
for (i in 1:n){
        for (j in 1:dim(C4)[1]){
                if (rownames(fpc)[i]==rownames(C4)[j]){
                        G4 = rbind(G4,fpc[i,])
                }
        }
}
rownames(G4) = rownames(C4)
plot(G4[,1], G4[,2],main="Group4 Factor Scores", 
     xlim=c(-1,2), ylim=c(-2,4))
text(G4[,1], G4[,2], labels=rownames(G4), cex=0.6, col="blue", pos=1)
abline(v=0, h=0)

