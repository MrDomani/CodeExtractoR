########################
##                    ##
##    INTRODUCTION    ##
##                    ##
########################       

data("butterfly", package = "fclust")
butterfly <- butterfly[-c(1,17),]
rownames(butterfly) <- as.character(rep(1:nrow(butterfly)))
plot(butterfly, type = "n", xlab = "Var. 1", ylab="Var. 2")
text(butterfly[,1], butterfly[,2], labels = rownames(butterfly), cex = 0.7, lwd = 2)
set.seed(12)
nt <- 1000
ca8 <- rep(NA,nt)
lfv <- rep(NA,nt)
for (n in 1: nt){
  km.butterfly <- kmeans(butterfly, centers = 2, iter.max = 1000, nstart = 10)
  lfv[n] <- km.butterfly[[5]]
  if (km.butterfly$cluster[8] == km.butterfly$cluster[1]){
    ca8[n] <- 1
  }else{
    ca8[n] <- 2
  } 
}
summary(lfv)
table(ca8)

install.packages("fclust")
library(fclust)

Fclust (X, k, type, ent, noise, stand, distance)
Fclust (X = X, k = 3, type = "standard", noise = TRUE)

#######################
##                   ##
##    THE PACKAGE    ##
##                   ##
#######################    

###############################
##    Fuzzy k-means (FKM)    ##
###############################  

# possible label switching
data("NBA")
X.NBA <- NBA[,c(4,7,10,11,12,13,14,15,16,17,20)]
fkm.NBA <- FKM(X = X.NBA, m = 1.2, RS = 50, stand = 1, index = "SIL.F")
summary(fkm.NBA)
plot(fkm.NBA, pca = TRUE)
# pr <- prcomp(X.NBA, scale. = TRUE)
# round(pr$rotation[,1],3)
round(Hraw(X = X.NBA, H = fkm.NBA$H), 3)
Fclust.compare(VC = NBA$Playoff, U = fkm.NBA$U)
Fclust.compare(VC = NBA$Conference, U = fkm.NBA$U)

#################################################################################
##    Gustafson-Kessel extensions of the FKM algorithm (FKM.gk and FKM.gkb)    ##
#################################################################################

data(synt.data2)
# plot(synt.data2)
fkm.gk.synt <- FKM.gk(X = synt.data2, k = 3, RS = 1)
fkm.gk.synt$iter
fkm.gk.synt$value
fkm.gkb.synt <- FKM.gkb(X = synt.data2, k = 3, RS = 1, seed = 123)
fkm.gkb.synt$iter
fkm.gkb.synt$value


################################################################################
##    Fuzzy clustering for indirect relational data  (dichotomous variables)  ##
################################################################################

# possible label switching
data("houseVotes")
colnames(houseVotes)
level.drop <- droplevels(houseVotes, exclude = "yn")
houseVotesComplete <- level.drop[complete.cases(level.drop),]
X.houseVotesComplete <- houseVotesComplete[,-1]
library(cluster)
D.houseVotes <- daisy(x = X.houseVotesComplete, metric = "gower")
nefrc.houseVotes <- NEFRC(D = D.houseVotes, k = 2, m = 1.5, index = "SIL.F")
Fclust.compare(VC = houseVotesComplete$class, U = nefrc.houseVotes$U)
table(nefrc.houseVotes$clus[,1], houseVotesComplete$class)
plot(nefrc.houseVotes)


############################################################################
##    Fuzzy clustering for indirect relational data  (ordinal variables)  ##
############################################################################

# possible label switching
library(likert)
data("mass")
library(cluster)
D.mass <- daisy(x = mass[,-1], metric = "gower")
nefrc.mass <- NEFRC(D = D.mass, index = "SIL.F")
nefrc.mass$criterion
nefrc.mass <- Fclust(X = D.mass, k = 2, noise = FALSE, distance = TRUE)
PV <- rep(NA,ncol(mass))
for (j in 1:ncol(mass)) PV[j] <- chisq.test(nefrc.mass$clus[,1], mass[,j])$p.value
alpha <- 0.05
names(mass)[PV < alpha]


#####################################################
##    Fuzzy clustering for direct relational data  ##
#####################################################

# possible label switching
library(smacof)
data("FaceExp")
labels(FaceExp)
nefrc.FaceExp <- NEFRC(D = FaceExp, index = "SIL.F")
nefrc.FaceExp$criterion
round(nefrc.FaceExp$clus[(nefrc.FaceExp$clus[,1] == 1), 2], 2)
round(nefrc.FaceExp$clus[(nefrc.FaceExp$clus[,1] == 2), 2], 2)
round(nefrc.FaceExp$clus[(nefrc.FaceExp$clus[,1] == 3), 2], 2)
