library(readxl)
library(FactoMineR)
library(outliers)
library(lmtest)
library(AER)
library(car)
library(MASS)
library(PerformanceAnalytics)
library(stats)
library(factoextra)
library(gridExtra)
library('corrplot')
library(pls)


base1<-read_excel("Base_Final_R2.xlsx")
base1



    ##### Analyse Univariée #####

# Toutes les variables sont quantitatives et continues. Incluant la variable Y
base1$INS_DEN<-as.numeric(base1$INS_DEN)
base1$LITERACY<-as.numeric(base1$LITERACY)
base1$GOOD_HEALTH<-as.numeric(base1$GOOD_HEALTH)
base1$URBAN_POP<-as.numeric(base1$URBAN_POP)
base1$LIFE_EXP<-as.numeric(base1$LIFE_EXP)
base1$UNEMP<-as.numeric(base1$UNEMP)
base1$OLD_DEP<-as.numeric(base1$OLD_DEP)
base1$INFLATION<-as.numeric(base1$INFLATION)
base1$FINANC_DEV<-as.numeric(base1$FINANC_DEV)
base1$GNI<-as.numeric(base1$GNI)
base1$GDP<-as.numeric(base1$GDP)
base1$YOUNG_DEP<-as.numeric(base1$YOUNG_DEP)

str(base1)
summary(base1)


par(mar = rep(1, 2))
par(mfrow=c(1,1))
par(mfcol=c(1,1))
histogram<-hist(base1$INS_DEN,
                xlim=c(0,9000),ylim = c(0,40),
                main="Distribution_Densité de l'assurance-vie",
                xlab = "Densité de l'assurance",
                border="black",
                col="lightblue",breaks = 10,cex.main=0.8,cex.lab=0.8)

boxplot(base1[3:13])


      ##### Analyse Bivariée #####

# Correlation entre variables en utilisant le test de Spearman
Data1<-base1[,c("LITERACY","FINANC_DEV","INFLATION","GOOD_HEALTH","URBAN_POP","LIFE_EXP","UNEMP","OLD_DEP","YOUNG_DEP","GDP","GNI","INS_DEN")]
chart.Correlation(Data1,histogram=TRUE,pch=19,method=c("spearman"))
corrplot(cor(base1[,2:13]))

# Comparaison entre la variable expliquée et les variables explicatives (analyse individuelle)

par(mar = rep(4, 4))
par(mfrow=c(1,1))
plot(base1$INS_DEN~base1$LITERACY, type = "p",main = "INS_DEN vs LITERACY", xlab = "Taux d'alphabétisation", ylab = "Densité de l'assurance-vie en $US")
abline(lm(base1$INS_DEN~base1$LITERACY),col="red",lwd=2)

plot(base1$INS_DEN~base1$GOOD_HEALTH, type = "p",main="INS_DEN vs GOOD_HEALTH", xlab = "% population en bonne santé", ylab = "Densité de l'assurance-vie en $US")
abline(lm(base1$INS_DEN~base1$GOOD_HEALTH),col="red",lwd=2)

plot(base1$INS_DEN~base1$URBAN_POP, type = "p",main="INS_DEN vs URBAN_POP", xlab = "% population urbaine", ylab = "Densité de l'assurance-vie en $US")
abline(lm(base1$INS_DEN~base1$URBAN_POP),col="red",lwd=2)

plot(base1$INS_DEN~base1$LIFE_EXP, type = "p",main="INS_DEN vs LIFE_EXP", xlab = "Espérance de vie (de la naissance)", ylab = "Densité de l'assurance-vie en $US")
abline(lm(base1$INS_DEN~base1$LIFE_EXP),col="red",lwd=2)

plot(base1$INS_DEN~base1$UNEMP, type = "p",main="INS_DEN vs UNEMP",xlim=c(0,15), xlab = "Taux de chômage", ylab = "Densité de l'assurance-vie en $US")
abline(lm(base1$INS_DEN~base1$UNEMP),col="red",lwd=2)

plot(base1$INS_DEN~base1$OLD_DEP, type = "p",main="INS_DEN vs OLD_DEP", xlab = "Ratio de personnes âgées", ylab = "Densité de l'assurance-vie en $US")
abline(lm(base1$INS_DEN~base1$OLD_DEP),col="red",lwd=2)

plot(base1$INS_DEN~base1$INFLATION, type = "p",main="INS_DEN vs INFLATION", xlab = "Taux d'inflation", ylab = "Densité de l'assurance-vie en $US")
abline(lm(base1$INS_DEN~base1$INFLATION),col="red",lwd=2)

plot(base1$INS_DEN~base1$FINANC_DEV, type = "p",main="INS_DEN vs FINANC_DEV", xlab = "Développement financier (M2/GDP Ratio)", ylab = "Densité de l'assurance-vie en $US")
abline(lm(base1$INS_DEN~base1$FINANC_DEV),col="red",lwd=2)

plot(base1$INS_DEN~base1$GNI, type = "p",main="INS_DEN vs GNI", xlab = "Revenu national brut par habitant en $US", ylab = "Densité de l'assurance-vie en $US")
abline(lm(base1$INS_DEN~base1$GNI),col="red",lwd=2)

plot(base1$INS_DEN~base1$YOUNG_DEP, type = "p",main="INS_DEN vs YOUNG_DEP",xlab = "Ratio de personnes moins de 18ans", ylab = "Densité de l'assurance-vie en $US")
abline(lm(base1$INS_DEN~base1$YOUNG_DEP),col="red",lwd=2)

plot(base1$INS_DEN~base1$GDP, type = "p",main="INS_DEN vs GDP", xlab = "PIB par habitant en $US", ylab = "Densité de l'assurance-vie en $US")
abline(lm(base1$INS_DEN~base1$GDP),col="red",lwd=2)


# Relation entre variables explicatives

# Négativement corrélées
plot(OLD_DEP~YOUNG_DEP, main="OLD_DEP vs YOUNG_DEP", xlab = "OLD_DEP", ylab = "YOUNG_DEP",data=base1)
plot(base1$GNI~base1$INFLATION, type = "p",main="GNI vs INFLATION", xlab = "Taux d'inflation", ylab = "Revenu national brut par habitant en $US")
abline(lm(base1$GNI~base1$INFLATION),col="red",lwd=2)

# Positivement corrélées
plot(base1$GNI~base1$FINANC_DEV, type = "p",main="GNI vs FINANC_DEV", xlab = "Développement financier (M2/GDP Ratio)", ylab = "Revenu national brut par habitant en $US")
abline(lm(base1$GNI~base1$FINANC_DEV),col="red",lwd=2)

# Autre relations
plot(base1$GNI~base1$UNEMP, type = "p",main="INITIAL DATA",sub = "GNI vs Unemployment Rate", xlab = "Unemployment rate", ylab = "GNI per capita in US$")
plot(base1$GDP~base1$UNEMP, type = "p",main="GDP vs UNEMP", ylim=c(0,400000),xlab = "Taux de chômage", ylab = "PIB par habitant en $US")
plot(base1$GOOD_HEALTH~base1$LIFE_EXP, type = "p",main="GOOD_HEALTH vs LIFE_EXP", xlab = "Espérance de vie", ylab = "% population en bonne santé")
abline(lm(base1$GOOD_HEALTH~base1$LIFE_EXP),col="red",lwd=2)
plot(base1$GNI~base1$LIFE_EXP, type = "p",main="GNI vs LIFE_EXP", xlab = "ERevenu national brut par habitant en $US", ylab = "Espérance de vie")
abline(lm(base1$GOOD_HEALTH~base1$LIFE_EXP),col="red",lwd=2)


      ##### DETECTION ET SUPPRESSION DES VALEURS ATYPIQUES #####

# On utilise les boxplots pour detecter les valeurs atypiques. Nous décidons de supprimer les valeurs atypiques dans notre cas, au lieu de faire un 'impute'

par(mar = rep(4, 4))
# Pas de valeurs atypiques
boxplot(base1$LIFE_EXP,xlab="Life Expectancy", main="Boxplot: Life Expectancy")

# Il y a une valeur atypique selon le boxplot. Nous utilisons le test de Grubbs.
boxplot(base1$GOOD_HEALTH,xlab="% population in good health",main="Boxplot: % good health")
#Grubbs test
grubbs.test(base1$GOOD_HEALTH,type=10,two.sided=TRUE)
# Outlier = 54.4

# Pas de valeurs atypiques
boxplot(base1$OLD_DEP,xlab="Ratio of old dependents", main="Boxplot: Ratio of old dependents")

# Pas de valeurs atypiques
boxplot(base1$URBAN_POP,xlab="% of urban population",main="Boxplot: % of urban population")

# Pas de valeurs atypiques
boxplot(base1$GDP,ylim=c(0,100000),xlab="GDP",main="Boxplot: GDP per capita")

# Pas de valeurs atypiques
boxplot(base1$YOUNG_DEP,xlab="Ratio of young dependency",main="Boxplot: Ratio of young dependents")

# # Selon le boxplot on assume la présence de plusieurs valeurs atypiques. Nous utilisons le test 'ESD pour confirmer ce résultat et détecter les valeurs atypiques.
boxplot(base1$UNEMP,xlab="Unemployment Rate",main="Boxplot: Unemployment rate")
# Test ESD au seuil de 5% de tolérance.
y = base1$UNEMP
rval = function(y){
  ares = abs(y - mean(y))/sd(y)
  df = data.frame(y, ares)
  r = max(df$ares)
  list(r, df)}
n = length(y)
alpha = 0.05
lam = c(1:15)
R = c(1:15)
for (i in 1:15){
  if(i==1){
    rt = rval(y)
    R[i] = unlist(rt[1])
    df = data.frame(rt[2])
    newdf = df[df$ares!=max(df$ares),]}
  else if(i!=1){
    rt = rval(newdf$y)
    R[i] = unlist(rt[1])
    df = data.frame(rt[2])
    newdf = df[df$ares!=max(df$ares),]}
  p = 1 - alpha/(2*(n-i+1))
  t = qt(p,(n-i-1))
  lam[i] = t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1))
}
newdf = data.frame(c(1:15),R,lam)
names(newdf)=c("No. Outliers","Test Stat.", "Critical Val.")
newdf
# Le test ESD confirme la présence d'une valeur atypique.
sort(base1$UNEMP)
order(base1$UNEMP)
# Outliers = 30

# Selon le boxplot on assume la présence de plusieurs valeurs atypiques. Nous utilisons le test 'ESD' pour confirmer ce résultat et détecter les valeurs atypiques.
boxplot(base1$INFLATION,xlab="Inflation Rate",main="Boxplot: Inflation Rate")
# Test ESD au seuil de 5% de tolérance.
y = base1$INFLATION
rval = function(y){
  ares = abs(y - mean(y))/sd(y)
  df = data.frame(y, ares)
  r = max(df$ares)
  list(r, df)}
n = length(y)
alpha = 0.05
lam = c(1:15)
R = c(1:15)
for (i in 1:15){
  if(i==1){
    rt = rval(y)
    R[i] = unlist(rt[1])
    df = data.frame(rt[2])
    newdf = df[df$ares!=max(df$ares),]}
  else if(i!=1){
    rt = rval(newdf$y)
    R[i] = unlist(rt[1])
    df = data.frame(rt[2])
    newdf = df[df$ares!=max(df$ares),]}
  p = 1 - alpha/(2*(n-i+1))
  t = qt(p,(n-i-1))
  lam[i] = t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1))
}
newdf = data.frame(c(1:15),R,lam)
names(newdf)=c("No. Outliers","Test Stat.", "Critical Val.")
newdf
# Le test ESD confirme la présence d'une valeur atypique.
sort(base1$INFLATION)
order(base1$INFLATION)
# Outliers = 45

# Selon le boxplot on assume la présence de plusieurs valeurs atypiques. Nous utilisons le test 'ESD' pour confirmer ce résultat et détecter les valeurs atypiques.
boxplot(base1$LITERACY,xlab="% of literate adults",main="Boxplot: Literacy rate")
y = base1$LITERACY
rval = function(y){
  ares = abs(y - mean(y))/sd(y)
  df = data.frame(y, ares)
  r = max(df$ares)
  list(r, df)}
n = length(y)
alpha = 0.05
lam = c(1:15)
R = c(1:15)
for (i in 1:15){
  if(i==1){
    rt = rval(y)
    R[i] = unlist(rt[1])
    df = data.frame(rt[2])
    newdf = df[df$ares!=max(df$ares),]}
  else if(i!=1){
    rt = rval(newdf$y)
    R[i] = unlist(rt[1])
    df = data.frame(rt[2])
    newdf = df[df$ares!=max(df$ares),]}
  p = 1 - alpha/(2*(n-i+1))
  t = qt(p,(n-i-1))
  lam[i] = t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1))
}
newdf = data.frame(c(1:15),R,lam)
names(newdf)=c("No. Outliers","Test Stat.", "Critical Val.")
newdf
# Le test ESD confirme la présence de 2 valeurs atypiques.
sort(base1$LITERACY)
order(base1$LITERACY)
# Outliers = 25,31

# Pas de valeurs atypiques
boxplot(base1$FINANC_DEV,xlab="Financial development",main="Boxplot: Financial development")

# Il y a une valeur atypique selon le boxplot. Nous utilisons le test de Grubbs.
boxplot(base1$GNI, xlab="GNI per capita",main="Boxplot: GNI per capita")
#Grubbs test
grubbs.test(base1$GOOD_HEALTH,type=10,two.sided=TRUE)
# Outlier = 54.4

baseFinal<-base1[-c(54.4,45,25,31,30),]
# Les valeurs atypiques représentent moins de 10% de la base initiale. Ainsi nous décidons de les supprimer afin d'enlever l'élément de biais.
save(baseFinal,file='baseFinal.rda')


      ##### REDUCTION DE DIMENSION PAR ACP #####

par(mar = rep(2, 4))
res.pca<-PCA(baseFinal[2:13],quanti.sup = 1)
res<-barplot(res.pca$eig[,2],xlab="Dim.",ylab="Percentage of variance",ylim=c(0,80))
fviz_eig(res.pca,addlabels=TRUE)


fviz_pca_var(res.pca,geom=c("point","text"),repel = TRUE)
plot.PCA(res.pca,title="Projection des variables sur les axes 1 et 3",axes = c(1,3),choix = "var", xlim=c(-8,8),ylim=c(-8,8))
plot.PCA(res.pca,title="Projection des variables sur les axes 2 et 3",axes = c(2,3),choix = "var", xlim=c(-8,8),ylim=c(-8,8))

fviz_pca_var(res.pca, axes = c(1,2), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel=TRUE)

fviz_pca_var(res.pca, axes = c(1,3), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel=TRUE)

fviz_pca_var(res.pca, axes = c(2,3), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel=TRUE)

help(fviz_pca_var)

Valeurs_Propres<-round(res.pca$eig,2)
Valeurs_Propres

#********************************************
    ##### REGRESSION #####

                          ## REGRESSION MULTIPLE ##

initial_model<-lm(INS_DEN~UNEMP+LITERACY+YOUNG_DEP+URBAN_POP+INFLATION+GNI+FINANC_DEV+OLD_DEP+GDP+URBAN_POP+LIFE_EXP+GOOD_HEALTH,data=baseFinal)
modele1<-step(initial_model,direction = 'both',criterion='AIC')
summary(modele1)

# Tests
# Distance < 1
par(mar = rep(1, 2))
par(mfrow=c(1,2))
par(mfcol=c(1,2))
dev.new(width=5, height=4)
plot(cooks.distance(modele1),type="h",ylim = c(0,1))

# ANALYSIS OF THE GRAPHS: MULTIPLE GRAPHS
par(mar = rep(1, 1))
dev.new(width=10, height=10)
plot(modele1)
reset(modele1)
vif(modele1)
bptest(modele1)
residus<-residuals(modele1)
shapiro.test(residus)

# En enlevant la variable LIFE_EXP pour régler le problème de colinéarité.
modele1a<-lm(INS_DEN~UNEMP+GNI+FINANC_DEV+OLD_DEP+YOUNG_DEP+LITERACY+GOOD_HEALTH,data=baseFinal)
par(mar = rep(1, 1))
dev.new(width=10, height=10)
plot(modele1a)
reset(modele1a)
vif(modele1a)
bptest(modele1a)
residus<-residuals(modele1a)
shapiro.test(residus)


#***************************************************
# Transformation logarithme de la variable expliquée uniquement
modele2<-lm(log(INS_DEN)~UNEMP+GNI+FINANC_DEV+OLD_DEP+YOUNG_DEP+LITERACY+GOOD_HEALTH,data=baseFinal)
par(mar = rep(1, 1))
dev.new(width=10, height=10)
plot(modele2)

reset(modele2)
# p-value = 0.01138 < 0.05. We still do not accept Ho: Linear Functional form of model is not accepted. 

vif(modele2)
# ok VIF < 10

bptest(modele2)
# p-value = 0.3162 > 0.05. We accept Ho: homoscedasticity of residues at 5% significant level.

residus<-residuals(modele2)
shapiro.test(residus)
# p-value=0.3063 > 0.05. We accept Ho, the residues are following a normal distribution



#*****************************************************
# Double-log transformation of GNI variable only
modele4<-lm(log(INS_DEN)~UNEMP+log(GNI)+FINANC_DEV+OLD_DEP+YOUNG_DEP+GOOD_HEALTH+LITERACY,data=baseFinal)

par(mar = rep(1, 1))
par(mfrow=c(1,2))
par(mfcol=c(1,2))
dev.new(width=10, height=10)
plot(modele4)

reset(modele4)
# p-value = 0.3015 > 0.05. We accept the linear functional form of the modele4.

vif(modele4)
# ok VIF < 10

bptest(modele4)
# p-value = 0.393 > 0.05. We accept Ho: homoscedasticity of residues at 5% significant level.

residus<-residuals(modele4)
shapiro.test(residus)
# p-value=0.4354 > 0.05. We accept Ho, the residues are following a normal distribution
# Higher than modele 3

residualPlots(modele4)

# MODELE 4 IS OUR SELECTED MODEL
#***********************************************************
# ENDOGENEITY TEST

modele4

# From modele4, I suspect that Financial development and Good Health are endogeneous.
# Since both variables are considered to be not as correlated because they passed the VIF test, I will test endogeneity together.
# We will use one instrument for each variable.

# To test endogeneity of Financial development
# Using GDP as instrument
DMC_modele1<-ivreg(log(INS_DEN)~UNEMP + log(GNI) + FINANC_DEV + OLD_DEP+ YOUNG_DEP+GOOD_HEALTH+LITERACY |UNEMP + log(GNI) + URBAN_POP + OLD_DEP+YOUNG_DEP+LITERACY+ LIFE_EXP,data=baseFinal)
summary(DMC_modele1,diagnostics = TRUE)

summary(modele4)




#*****************************************************************************
#                             PCR Elvina
#*****************************************************************************


X<-as.matrix(baseFinal[3:13])

#X[,'GNI'] = log(X[,'GNI']) # prise en compte de la transformation logarithmique de GNI

##Essayer avec ncomp = 10 ou 11 par exemple pour bien faire la validation crois�e et voir le r�sulat obtenu
##Ensuite faire la PCR d�finitive avec le ncomp minimisant le CVadj
##ncomp = 5 sans transformation logarithmique normalement. A voir si �a reste le cas en appliquant les deux transformations logarithmiques en commentaires !!

assurance.pcr <- pcr(baseFinal$INS_DEN ~ X, ncomp = 3, validation = "LOO", data=baseFinal)

#assurance.pcr <- pcr(log(baseFinal$INS_DEN) ~ X, ncomp = 3, validation = "LOO", data=baseFinal) # mod�le avec transformation logarithmique de la variable INS_DEN

summary(assurance.pcr)
plot(RMSEP(assurance.pcr), legendpos = "topright")
# Choisir le nombre de composante en utilisant la validation croisée
msepcv.pcr <- MSEP(assurance.pcr,estimate=c("train","CV"))
ncomp.pcr <- which.min(msepcv.pcr$val["CV",,])-1
ncomp.pcr

coef(assurance.pcr)
plot(assurance.pcr, plottype = "correlation", ncomp=1:2, legendpos = "bottomleft",labels = "names", xlab = "nm")
plot(assurance.pcr, plottype = "correlation", ncomp=1:3, legendpos = "topleft",labels = "names", xlab = "nm")
plot(assurance.pcr, plottype = "correlation", ncomp=2:3, legendpos = "bottomleft",labels = "names", xlab = "nm")

scores(assurance.pcr)



                      

#*****************************************************************************
#                             PLS
#*****************************************************************************

#Je fais la PLS directement avec les transformations logarithmiques de GNI et INS_DEN

baseFinal2=baseFinal
baseFinal2$INS_DEN<-log(baseFinal2$INS_DEN)
baseFinal2$GNI<-log(baseFinal2$GNI)
names(baseFinal2)[2]="log(INS_DEN)"
names(baseFinal2)[11]="log(GNI)"
X2<-as.matrix(baseFinal2[3:13])

#choix du nombre de composantes par validation crois�e

assurance.pls <- plsr(`log(INS_DEN)` ~ X2, scale=TRUE, validation = "LOO", data=baseFinal2)
summary(assurance.pls)

plot(RMSEP(assurance.pls), legendpos = "topright")

par(mfrow=c(1,2))
par(mfcol=c(1,2))
ncomp.onesigma <- selectNcomp(assurance.pls, method = "onesigma", plot = TRUE)
ncomp.permut <- selectNcomp(assurance.pls, method = "randomization", plot = TRUE)
ncomp.onesigma
ncomp.permut
#les deux m�thodes sugg�rent de garder une seule composante
ncomp.pls=min(ncomp.onesigma,ncomp.permut)

##Dans la suite, on s'int�resse donc principalement � la premi�re dimension
##en utilisant soit assurance.pls.final, soit assurance.pls avec ncomp=1
##on pourra voir si consid�rer d'autres dimensions suppl�mentaires apporterait
##plus � notre �tude

assurance.pls.final <- plsr(`log(INS_DEN)` ~ X2, ncomp=ncomp.pls, scale=TRUE, validation = "LOO", data=baseFinal2)
summary(assurance.pls.final)

#test d'hypoth�ses

reset(assurance.pls)
#p-value = 0.1983 > 0.05 : OK lin�arit� du mod�le

bptest(assurance.pls) #est-ce utile vu que les donn�es ont �t� r�duites pour faire la pls, je ne sais pas !!
#p-value = 0.9259 > 0.05 : OK homosc�dasticit�

#residus<-residuals(assurance.pls.final)
#shapiro.test(residus)

residus<-residuals(assurance.pls)
for (i in 1:11) {
  print(shapiro.test(residus[,,i]))
}
#OK hypoth�se de normalit� de r�sidus au risque 1%
#quel que soit le nombre de composantes consid�r�.




##################################################################
### Que se passerait-il au niveau des hypoth�ses si on fait
### la pls sans les transformations logarithmiques consid�r�es ?
##################################################################

assurance.pls.sanslog <- plsr(INS_DEN ~ X, scale=TRUE, validation = "LOO", data=baseFinal)
summary(assurance.pls.sanslog)
#on constate d�j� une d�t�rioration des performances de ce mod�le par rapport au pr�c�dent
#la capacit� explicative de la variance de Y a beacoup baiss�.

plot(RMSEP(assurance.pls.sanslog), legendpos = "topright")

#par(mfrow=c(1,2))
#par(mfcol=c(1,2))
ncomp.onesigma.sanslog <- selectNcomp(assurance.pls.sanslog, method = "onesigma", plot = TRUE)
ncomp.permut.sanslog <- selectNcomp(assurance.pls.sanslog, method = "randomization", plot = TRUE)
ncomp.onesigma.sanslog
ncomp.permut.sanslog
#les deux m�thodes sugg�rent de garder toujours une seule composante

#test d'hypoth�ses

reset(assurance.pls.sanslog)
#p-value = 0.002029 < 0.01 < 0.05 : On rejete l'hypoth�se nulle : le mod�le n'est pas lin�aire.
#on a donc perdu la lin�arit� v�rifi�e dans le mod�le utilisant les transformations logarithmiques

bptest(assurance.pls.sanslog) #est-ce utile vu que les donn�es ont �t� r�duites pour faire la pls, je ne sais pas !!
#p-value = 0.03368 > 0.01 : OK homosc�dasticit� mais plus au risque 5% aussi comme dans le cas pr�c�dent.
#les donn�es �tant r�duites lors de la pls, ce n'est pas ce test qui fera la diff�rence

residus.sanslog<-residuals(assurance.pls.sanslog)
#shapiro.test(residus.sanslog[,,1])
for (i in 1:11) {
  print(shapiro.test(residus.sanslog[,,i]))
}
#Hypoth�se de normalit� de r�sidus non v�rifi�e au risque 1% et 5%
#quand on consid�re 1, 2 ou 3 composantes.
#Elle n'est v�rifi�e qu'� partir de 4 composantes au risque 1%
#Ici encore la qualit� du mod�le a �t� d�trior�e. Et si on doit se limiter 
#� moins de 4 composantes comme c'est le cas, on ne peut donc pas satisfaire cette hypoth�se


######################################################################################################
### Il est clair que les transformations logarithmiques utilis�es pour la PLS sont pertinentes
### pour satisfaire les hypoth�ses de lin�rait� du mod�le et de normalit� des r�sidus.
### De plus, la variabilit� expliqu�e est nettement meilleure.
### La seule chose � v�rifier peut-�tre plus tard serait de voir si on ne pouvait pas se limiter
### au passage au log juste pour la variable r�ponse. Ce qui est s�r, les hypoth�ses sont v�rifi�es 
### et garder les deux log permet d'avoir un mod�le proche de celui du MCO mais avec plus des variables
########################################################################################################



#On continue donc l'�tude avec la PLS faisant intervenir les logarithmes


##Corr�lation :

#signification des axes : loadings

assurance.pls.final$loadings
assurance.pls$loadings
round(loadings(assurance.pls)[,1:3],2)
plot(assurance.pls.final, comps = 1, plottype = "loadings", legendpos="bottomleft", labels="names")
plot(assurance.pls, plottype = "loadings", legendpos="bottomright", labels="names", xlab="variables")
plot(assurance.pls, comps = 1:3, plottype = "loadings", legendpos="bottomright", labels="names", xlab="variables", ylab="X loadings",ylim=c(-1,0.7))

#cercles de corr�lation

par(mfrow=c(1,2))
plot(assurance.pls, plottype = "correlation")#,labels = "names")
plot(assurance.pls, plottype = "correlation",labels = "names")
plot(assurance.pls, plottype = "correlation",comps=c(1,3))
plot(assurance.pls, plottype = "correlation",comps=c(1,3),labels = "names")
plot(assurance.pls, plottype = "correlation",comps=c(2,3))
plot(assurance.pls, plottype = "correlation",comps=c(2,3),labels = "names")


#contribution des axes � la r�ponse : Yloadings
round(Yloadings(assurance.pls)[,1:3],2)

#Scores
par(mfrow=c(1,2))
plot(assurance.pls, plottype = "scores")#,labels = "names")
plot(assurance.pls, plottype = "scores",labels = "names")
plot(assurance.pls, plottype = "scores",comps=c(1,3))
plot(assurance.pls, plottype = "scores",comps=c(1,3),labels = "names")
plot(assurance.pls, plottype = "scores",comps=c(2,3))
plot(assurance.pls, plottype = "scores",comps=c(2,3),labels = "names")

#plots non utilis�s
plot(assurance.pls$Yscores,comps=c(1,2),labels = "names")
plot(assurance.pls$Yscores,comps=c(1,3),labels = "names")
plot(assurance.pls$Yscores,comps=c(2,3),labels = "names")

#pr�dictions
plot(assurance.pls, plottype = "prediction")#,labels = "names")
abline(reg=assurance.pls)

predict(assurance.pls, ncomp=1, newdata=baseFinal2)
plot(assurance.pls, ncomp=1, asp=1, line=TRUE)

#coefficients

coef(assurance.pls, ncomp=1:3, intercept = TRUE)
coef(assurance.pls) #avec les 11 composantes
coefplot(assurance.pls, ncomp = 11,intercept = TRUE,labels = "names",legendpos = "bottomright", ylim = c(-2,2))
plot(assurance.pls, plottype = "coef", ncomp=1:3,labels = "names", legendpos = "topleft")






#*****************************************************************************
#                   PCR avec test de diff�rentes transformations
#*****************************************************************************

# PCR sans transformations logarithmiques : �tude du mod�le

#choix du nombre de composantes

assurance.pcr <- pcr(INS_DEN ~ X, scale = TRUE, validation = "LOO", data=baseFinal)
summary(assurance.pcr)
plot(RMSEP(assurance.pcr), legendpos = "topright")
# Choisir le nombre de composante en utilisant la validation croisée
msepcv.pcr <- MSEP(assurance.pcr,estimate=c("train","CV"))
ncomp.pcr <- which.min(msepcv.pcr$val["CV",,])-1
ncomp.pcr 
#on trouve 11 : pas pertinent. On peut d�j� se limiter � 4 (premier minimum local proche du minimum global)
#ou choisir 3 pour des raisons de variances expliqu�s de Y pas trop diff�rentes entre 3 et 4 et des PRESS proches
#Sinon on essaye la fonction selectNcomp avec ses deux m�thodes :

ncomp.onesigma <- selectNcomp(assurance.pcr, method = "onesigma", plot = TRUE)
ncomp.permut <- selectNcomp(assurance.pcr, method = "randomization", plot = TRUE)
ncomp.onesigma
ncomp.permut
#la prmi�re m�thode sug�re de garder pl�tot 3 composantes, la deuxi�me une seule.
#on peut donc consid�rer 3 composantes dans la suite si on veut et les justifier
#comme combinaison PRESS + ces 2 m�thodes + variance Y

#Test d'hypoth�ses

reset(assurance.pcr)
#p-value = 0.002029 < 0.01 < 0.05 : On rejete l'hypoth�se nulle : le mod�le n'est pas lin�aire.
#on a donc perdu la lin�arit� v�rifi�e dans le mod�le utilisant les transformations logarithmiques
#m�me r�sultat que pour PLS

bptest(assurance.pcr) #est-ce utile vu que les donn�es ont �t� r�duites pour faire la pls, je ne sais pas !!
#p-value = 0.03368 > 0.01 : OK homosc�dasticit� mais plus au risque 5% aussi comme dans le cas pr�c�dent.
#les donn�es �tant r�duites lors de la pls, ce n'est pas ce test qui fera la diff�rence
#pareil aussi

residus<-residuals(assurance.pcr)
shapiro.test(residus[,,3])
#p-value = 8.503e-06 << 0.01 : pas de normalit� de r�sidus.


#Passage au log pour la variable d�pendante uniquement puis on refait les m�mes �tapes

assurance.pcr <- pcr(log(INS_DEN) ~ X, scale = TRUE, validation = "LOO", data=baseFinal)
summary(assurance.pcr)
#on remarque une nette am�lioration de la variance expliqu�e pour log(INS_DEN)

plot(RMSEP(assurance.pcr), legendpos = "topright")
# Choisir le nombre de composante en utilisant la validation croisée
msepcv.pcr <- MSEP(assurance.pcr,estimate=c("train","CV"))
ncomp.pcr <- which.min(msepcv.pcr$val["CV",,])-1
ncomp.pcr 
#on trouve 3 composantes

ncomp.onesigma <- selectNcomp(assurance.pcr, method = "onesigma", plot = TRUE)
ncomp.permut <- selectNcomp(assurance.pcr, method = "randomization", plot = TRUE)
ncomp.onesigma
ncomp.permut
#ces deux m�thodes sgg�rent 1 composante.
#si ce mod�le convient, on gardera plut�t trois composantes pour plus de variances expliqu�es 

#Test d'hypoth�ses

reset(assurance.pcr)
#p-value = 0.001586 < 0.01 < 0.05 : On rejete l'hypoth�se nulle : le mod�le n'est pas lin�aire.
#pas toujours de lin�arit� malhreusement

bptest(assurance.pcr) #est-ce utile vu que les donn�es ont �t� r�duites pour faire la pls, je ne sais pas !!
#p-value = 0.9071 > 0.01 : OK homosc�dasticit� toujours

residus<-residuals(assurance.pcr)
shapiro.test(residus[,,3])
#p-value = 0.4213 > 0.05 : OK normalit� de r�sidus.


#Passage au log pour GNI �galement

assurance.pcr <- pcr(`log(INS_DEN)` ~ X2, scale=TRUE, validation = "LOO", data=baseFinal2)
summary(assurance.pcr)
#on remarque une nouvelle am�lioration de la variance expliqu�e pour log(INS_DEN) et pour X

plot(RMSEP(assurance.pcr), legendpos = "topright")
# Choisir le nombre de composante en utilisant la validation croisée
msepcv.pcr <- MSEP(assurance.pcr,estimate=c("train","CV"))
ncomp.pcr <- which.min(msepcv.pcr$val["CV",,])-1
ncomp.pcr 
#on trouve 3 composantes toujours : on va utiliser le crit�re PRESS imm�diatement ici

#pour info : les deux autres m�thodes
ncomp.onesigma <- selectNcomp(assurance.pcr, method = "onesigma", plot = TRUE)
ncomp.permut <- selectNcomp(assurance.pcr, method = "randomization", plot = TRUE)
ncomp.onesigma
ncomp.permut
#ces deux m�thodes sugg�rent 1 composante encore.

#Test d'hypoth�ses

reset(assurance.pcr)
#p-value = 0.1983 > 0.05 : On ne rejete plus l'hypoth�se nulle : le mod�le est lin�aire.

bptest(assurance.pcr)
#p-value = 0.9259 > 0.01 : OK homosc�dasticit� toujours

residus<-residuals(assurance.pcr)
shapiro.test(residus[,,3])
#p-value = 0.2178 > 0.05 : OK normalit� de r�sidus.

#Toutes les hypoth�ses �tant v�rifi�es d�sormais, nous gardons ce mod�le.


coef(assurance.pcr, ncomp = 3) #, intercept = TRUE)
plot(assurance.pcr, plottype = "correlation", ncomp=1:2)
plot(assurance.pcr, plottype = "correlation", ncomp=1:2,labels = "names")
title(main = "Cercle de corr�lation plan 1-2")
plot(assurance.pcr, plottype = "correlation", ncomp=1:3)
plot(assurance.pcr, plottype = "correlation", ncomp=1:3,labels = "names")
plot(assurance.pcr, plottype = "correlation", ncomp=2:3)
plot(assurance.pcr, plottype = "correlation", ncomp=2:3,labels = "names")

scores(assurance.pcr)
par(mfrow=c(1,2))
plot(assurance.pcr, plottype = "scores")#,labels = "names")
title(main = "Scores plan 1-2")
plot(assurance.pcr, plottype = "scores",labels = "names")
title(main = "Scores plan 1-2")
plot(assurance.pls, plottype = "scores",comps=c(1,3))
plot(assurance.pls, plottype = "scores",comps=c(1,3),labels = "names")
plot(assurance.pls, plottype = "scores",comps=c(2,3))
plot(assurance.pls, plottype = "scores",comps=c(2,3),labels = "names")

#pr�dictions
predict(assurance.pls, ncomp=3, newdata=baseFinal2)
plot(assurance.pls, ncomp=3, asp=1, line=TRUE)