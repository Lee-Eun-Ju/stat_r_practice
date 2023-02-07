#여름방학 3주차 - FIFA 분석

install.packages("corrplot")
install.packages("psych")
library(dplyr)
library(corrplot)
library(psych)
library(ggplot2)
library(tidyr)


setwd("C:\\Users\\이은주\\Desktop\\AIR 동아리\\여름방학\\3주")
data = read.csv("FIFA 2018 Statistics.csv", header = TRUE)
glimpse(data)
glimpse(data)
#변수 독립적으로 만들어주기
attach(data)
glimpse(data)
#numeric 아닌 변수들이랑 yellow&red랑 red가 거의 0이라서 삭제
data=data[,c(4:20,22)]
glimpse(data)
dim(data)
#최종 : 관측치 128개 변수 16개

#NA를 0으로 바꿔주기
data[is.na(data)]=0
head(data)

#다중산점도 그리기->변수너무 많아서 안보임
plot(data)

#주성분분석(PCA;principal component analysis) 
#차원의 축소, 자료의 요약이 주목적이다.->자료한눈에 파악하기 쉬움 
#시각화를 하면 변수와 개체간의 연관성도 살펴볼 수 있다. 
df<-data
select<-which(colnames(df) %in% c("Goal.Scored","Ball.Possession..","Attempts",
                                  "Corners","Free.Kicks","Saves","Pass.Accuracy..","Passes","Fouls.Committed","Yellow.Card","Red"))
df1 <- df[,select]
head(df1)
df1_corr <- cor(df1) # Create a correlation matrix
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(round(df1_corr, 2), method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE) # hide correlation coefficient on the principal diagonal
round(df1_corr, 2) # Correlation matrix in table form

KMO(df1_corr)

nfactors <- 4 # Based on the screeplot suggestion

evalue = eigen(df1_corr)$values
evec = eigen(df1_corr)$vectors

# Plot Eigenvalues / Represented Variance
eigenvalues <- data.frame(eigen(df1_corr)$values)
colnames(eigenvalues) <- c("Values")
eigenvalues$Number <- 1:nrow(df1_corr)
eigenvalues$RepresentedVariance <- NA
for (i in 1:nrow(df1_corr)) {
  eigenvalues$RepresentedVariance[i] <- sum(evalue[1:i])/sum(evalue) * 
    100
}
eigenvalues$RepresentedVariance_text <- paste(round(eigenvalues$RepresentedVariance, 
                                                    0), " %")

e1 <- ggplot(eigenvalues, aes(Number, y = Values), group = 1)
e1 <- e1 + geom_bar(stat = "identity")
e1 <- e1 + geom_line(aes(y = Values), group = 2)
e1 <- e1 + xlab("Number [-]")
e1 <- e1 + ylab("Eigenvalue [-]")
e1 <- e1 + geom_hline(aes(yintercept = 1), col = "red")
e1 <- e1 + geom_text(aes(label = RepresentedVariance_text), nudge_y = 0.2)
e1 <- e1 + ggtitle("Eigenvalues and explained Variance")
e1 <- e1 + theme_bw()
e1 <- e1 + scale_x_continuous(breaks = seq(1, 10, 1))
x11()
e1


## PCA
gof = evalue/sum(evalue)*100
gof
round(gof,3)
sum(gof[1:4]) 

V = evec[,1:4]
V
rowname = colnames(x)
rownames(V) = rowname
V
Z = scale(x)
PS = Z%*%V

dim(PS)
x11()
par(mfrow=c(2,2))
plot(PS[,1],PS[,2],xlab="1st PC",ylab="2nd PC",main = "PC Scores",col ="white")
text(PS[,1],PS[,2],labels=rownames(PS),cex=0.8,col="blue")
abline(v=0,h=0)

plot(PS[,1],PS[,3],xlab="1st PC",ylab="3rd PC",main = "PC Scores",col ="white")
text(PS[,1],PS[,3],labels=rownames(PS),cex=0.8,col="blue")
abline(v=0,h=0)

plot(PS[,2],PS[,3],xlab="2nd PC",ylab="3rd PC",main = "PC Scores",col ="white")
text(PS[,2],PS[,3],labels=rownames(PS),cex=0.8,col="blue")
abline(v=0,h=0)

D = diag(sqrt(evalue[1:3]))

# Biplot
par(pty="s")
lim1 <- range(pretty(H))
lim2 <- range(pretty(G))
x11()
biplot(G,H, xlab="1st PC",ylab="2nd PC", main="Biplot",
       xlim=lim2,ylim=lim2,cex=0.8,pch=14)
abline(v=0,h=0)

#PCFA
Z = scale(df1)
PCFA = principal(Z,nfactor = 4, rotate= "none")
L = PCFA$loadings[,1:4]
L
x11()
lim = range(pretty(L))
par(mfrow=c(2,2))
plot(L[,1],L[,2],xlab="f1",ylab="f2",main = "Plot of Factor Loadingse",xlim=lim,ylim=lim)
text(L[,1], L[, 2], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0,h=0)
arrows(0,0,L[,1],L[,2],col=2, code=2, length=0.1)

plot(L[,1],L[,3],xlab="f1",ylab="f3",main = "Plot of Factor Loadings",xlim=lim,ylim=lim)
text(L[,1], L[, 3], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0,h=0)
arrows(0,0,L[,1],L[,3],col=2, code=2, length=0.1)

plot(L[,2],L[,3],xlab="f2",ylab="f3",main = "Plot of Factor Loadings",xlim=lim,ylim=lim)
text(L[,2], L[, 3], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0,h=0)
arrows(0,0,L[,2],L[,3],col=2, code=2, length=0.1)

# Communaliy
Com = diag(L%*%t(L))
as.matrix(Com,ncol=1)

# Psi
R=df1_corr
Psi = diag(R-L%*%t(L))
as.matrix(Psi,ncol=1)

# Rm
Rm = R-(L%*%t(L) + diag(Psi))
round(Rm,3)

# Factor Scores
FS = PCFA$scores
lim = c(-2,2)
rownames(FS) = 1:128
par(mfrow=c(2,2))
x11()
plot(FS[,1],FS[,2],main = "Factor 
     Scores",xlab="f1",ylab="f2",xlim=lim,ylim=lim,col="black")
text(FS[,1],FS[,2],labels=rownames(FS),cex=0.8,col="blue",pos=1)
abline(v=0,h=0)

plot(FS[,1],FS[,3],main = "Factor 
     Scores",xlab="f1",ylab="f3",xlim=lim,ylim=lim,col="black")
text(FS[,1],FS[,3],labels=rownames(FS),cex=0.8,col="blue",pos=1)
abline(v=0,h=0)

plot(FS[,2],FS[,3],main = "Factor 
     Scores",xlab="f2",ylab="f3",xlim=lim,ylim=lim,col="black")
text(FS[,2],FS[,3],labels=rownames(FS),cex=0.8,col="blue",pos=1)
abline(v=0,h=0)

#주성분변수 해석
nvars<-dim(df1)[2]
loadings_mat <- as.data.frame(matrix(nrow = nvars, ncol =nfactors))
loadings_mat$Variable <- colnames(df1)
for (i in 1:nfactors) {
  for (j in 1:nvars) {
    loadings_mat[j, i] <- PCFA$loadings[j, i]  
  }
}
colnames(loadings_mat) <- c("Factor1","Factor2", "Factor3", "Factor4","Variable")
loadings_mat_gather <- loadings_mat %>% gather("Factor", "Value", 1:nfactors)

g1 <- ggplot(loadings_mat_gather, aes(Variable, abs(Value), fill=Value))
g1 <- g1 + facet_wrap(~ Factor, nrow=1)
g1 <- g1 + geom_bar(stat="identity")
g1 <- g1 + coord_flip()
g1 <- g1 + scale_fill_gradient2(name = "Loading", 
                                high = "blue", mid = "white", low = "red", 
                                midpoint=0, guide=F) 
g1 <- g1 + xlab("Variable")  # improve x-axis label
g1 <- g1 + ylab("Factor Loading")  #improve y-axis label
g1 <- g1 + ggtitle("Factors")
g1 <- g1 + theme(axis.text=element_text(size=10),
                 axis.title=element_text(size=12, face="bold"))
g1 <- g1 + theme(plot.title = element_text(size=12))
g1 <- g1 + theme_bw(base_size=12)
x11()
g1
