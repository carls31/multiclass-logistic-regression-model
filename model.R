## 0. Librerie

library( rms )
library(arm)
library(ResourceSelection)
library(pROC)
library(PRROC)
library( car )
library( ellipse )
library( faraway )
library( leaps )
library(MASS)
library( GGally)
library(rgl)
library(BAS)
library(corrplot)

setwd("C:/Users/39346/Desktop/progetto stat cardio")

#Importiamo i dati.

cardiodata = read.table( "cardiovascular.txt", head = TRUE, sep = ";"  )

str( cardiodata )

head( cardiodata )


#----------------------------------

sum(is.na(cardiodata)) # VERIFICO DATI MANCANTI

# ANALISI GRAFICA
par( mfrow = c( 1,2 ) )
boxplot( age ~ chd, data = cardiodata ) # con questo sembra significativo solo età
boxplot( ldl ~ chd, data = cardiodata ) # LDL carino ma con tantissimi outliers
# tutti gli altri non ci sono differenze
# --> fare boxplot per togliere outliers!!

pairs(cardiodata, pch=16)
pairs(cardiodata[ , c('sbp', 'tobacco', 'ldl', 'adiposity', 'typea', 'obesity', 'alcohol', 'age')], pch = 16)

ggpairs(data = cardiodata[ , -1], title ="Relationships between predictors & response", 
        lower = list(continuous=wrap("points", alpha = 0.5, size=0.1))) # vogliamo vedere correlazioni tra tutte le variabili

# AGE VS MALATTIA --> se al crescere età posso concludere che aumenta malattia (esplode sopra i 50)
min(cardiodata$age)
max(cardiodata$age)
#x  = c(15,24,28,32,37,42,48,55,64)
x  = c(15,25,30,35,40,45,50,55,64)
mid = c( ( x [ 2:10 ] + x [ 1:9 ] )/2 )
GRAGE = cut( cardiodata$age, breaks = x, include.lowest = TRUE, right = FALSE )
y = tapply( cardiodata$chd, GRAGE, mean )

plot( cardiodata$age, cardiodata$chd, pch = ifelse( cardiodata$chd == 1, 3, 4 ),
      col = ifelse( cardiodata$chd == 1, 'forestgreen', 'red' ),
      xlab = 'Age', ylab = 'CHD', main = 'Age vs CHD', lwd = 2, cex = 1.5 )
points( mid, y, col = "blue", pch = 16 )

# LDL VS MALATTIA --> ci accorgiamo che vale anche il colesterolo (oltre 9 sono valori davvero alti
# quindi ci sono poche osservazioni, ma numero malati è maggiore numero sani; fino a 9 cresce con
# l'aumentare del colesterolo)
min(cardiodata$ldl)
max(cardiodata$ldl)
x  = c(0.98,2.98,4.48,5.98,7.48,8.98,10.48,11.98,15.33)
mid = c( ( x [ 2:9 ] + x [ 1:8 ] )/2 )
GRAGE = cut( cardiodata$ldl, breaks = x, include.lowest = TRUE, right = FALSE )
y = tapply( cardiodata$chd, GRAGE, mean )

plot( cardiodata$ldl, cardiodata$chd, pch = ifelse( cardiodata$chd == 1, 3, 4 ),
      col = ifelse( cardiodata$chd == 1, 'forestgreen', 'red' ),
      xlab = 'Ldl', ylab = 'CHD', main = 'Ldl vs CHD', lwd = 2, cex = 1.5 )
points( mid, y, col = "blue", pch = 16 )

# Visualizziamo i dati.

plot( cardiodata$obesity, cardiodata$chd)
plot( cardiodata$age, cardiodata$chd)

pairs(cardiodata, pch=16)
pairs(cardiodata[ , c('sbp', 'tobacco', 'ldl', 'adiposity', 'typea', 'obesity', 'alcohol', 'age')], pch = 16)
ggpairs(data = cardiodata, title ="Relationships between predictors & response", 
        lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))

cardiodata$famhist   = factor( cardiodata$famhist )  # tratto la variabile FAMHIST come categorica
table(cardiodata$famhist)


open3d()
plot3d(x=cardiodata$ldl, y=cardiodata$obesity, z= cardiodata$alcohol, size=4, aspect = T)
plot3d(x=cardiodata$ldl, y=cardiodata$obesity, z= cardiodata$alcohol, col = cardiodata$age +1, size=4, aspect = T)

#-------------------------------------------------

mod = glm( chd ~ -ind + sbp + tobacco + ldl + adiposity +famhist + typea +obesity +alcohol +age, family = binomial( link = logit ) , data = cardiodata)
summary( mod )

step(mod, direction = "backward", trace = T) #conferma del mod5

# Riduciamo il modello manualmente
mod2 = glm( chd ~ -ind -alcohol + sbp + tobacco + ldl + adiposity +famhist + typea +obesity +age, family = binomial( link = logit ) , data = cardiodata)
summary( mod2 )

mod3 = glm( chd ~ -ind -alcohol -adiposity + sbp + tobacco + ldl +famhist + typea +obesity +age , family = binomial( link = logit ) , data = cardiodata)
summary( mod3 )

mod4 = glm( chd ~ -ind -alcohol -adiposity -sbp + tobacco + ldl  +famhist + typea +obesity +age , family = binomial( link = logit ) , data = cardiodata)
summary( mod4 )

mod5 = glm( chd ~ -ind -alcohol -adiposity -sbp -obesity + tobacco + ldl +famhist + typea +age , family = binomial( link = logit ) , data = cardiodata)
summary( mod5 )


#------------------------------------------------

x = c(-3.961459,-2.7,-2.1,-1.5,-0.9,-0.3,0.3,0.9,2.537633)
linear_pred= sort(mod5$linear.predictors)
#linear_pred
fitted_val= sort(mod5$fitted)
#fitted_val

mid = c( ( x [ 2:9 ] + x [ 1:8 ] )/2 )
GRAGE = cut( mod5$linear.predictors, breaks = x, include.lowest = TRUE, right = FALSE )
y = tapply( cardiodata$chd, GRAGE, mean )

plot( mod5$linear.predictors, cardiodata$chd, pch = ifelse( cardiodata$chd == 1, 3, 4 ),
      col = ifelse( cardiodata$chd == 1, 'forestgreen', 'red' ),
      xlab = 'Linear Predictors', ylab = 'CHD', main = 'CHD vs. Linear Predictors', lwd = 2, cex = 1.5 )
points( mid, y, col = "blue", pch = 16 )
#points( mod5$linear.predictors, mod5$fitted, col = "darkred", pch = 16 )
lines( linear_pred, fitted_val, col = 'darkblue' )


#-----------------------------------------------
# Massimizzo validità modello con STEP 
# Angelica 

mod01 = glm( chd ~ -ind + sbp + tobacco + ldl + adiposity +famhist + typea +obesity +alcohol +age 
             + log(sbp) +log(ldl) +log(adiposity) +log(typea) +log(obesity)+log(age), family = binomial( link = logit ) , data = cardiodata)
summary( mod01 )

mod01 = step(mod01, direction = "backward", trace = T) 
summary( mod01 )

#------------------------------------

# Calcoliamo l'OR relativo a famhist.

summary( mod5 )

exp(  coef( mod5 ) [ 4 ] )

# si deduce che il rischio di una malattia cardiaca aumenta quasi di 2.5 volte.


anova( mod5, mod, test = "Chisq" )

# L'ANOVA indica che il decremento nella devianza risultante dalla rimozione delle variabili non è 
# statisticamente significativo. Dunque, non c'è motivo di ritenere che il modello contenente 
# meno covariate sia meno informativo del modello completo.


# Facciamo un check sul GOF del modello.

hoslem.test( mod5$y, fitted( mod5 ), g = 7 )   #g > Numero Covariate

# Anche in questo caso, possiamo concludere che il modello dà un buon fit dei dati.


# Tabelle di (mis-)classificazione

# Un modo spesso utilizzato per presentare i risultati di un fit tramite regressione logistica sono 
# le tabelle di (mis-)classificazione. In queste tabelle i dati vengono classificati secondo due chiavi:

# _1._ il vero valore della variabile dipendente dicotoma y;

# _2._ il valore di una variabile dicotoma y_{mod}, che si deriva dalla stima della probabilità ottenuta dal modello.
#        I valori di questa variabile si ottengono confrontando il valore della probabilità con un cut-off. 
#        Di solito si usa il valore di 0.5, ma dipende molto da quello che stiamo indagando. 

soglia = 0.5

valori.reali  = cardiodata$chd
valori.predetti = as.numeric( mod5$fitted.values > soglia )
# 1 se > soglia, 0 se < = soglia
valori.predetti

tab = table( valori.reali, valori.predetti )

tab

# La tabella riportata è detta matrice di confusione, e riporta le osservazioni dette 
# Veri Positivi (True Positive o TP, osservazioni 1 classificate come 1), 
# Veri Negativi (True Negative o TN, osservazioni 0 classificate come 0), 
# Falsi Positivi (False Positive o FP, osservazioni 0 classificati come 1), 
# Falsi Negativi (Falsi Negativi o FN, osservazioni 1 classificati come 0). 

# Ci sono numerose metriche che permettono di valutare le performance del modello, a seconda delle esigenze:
# Accuracy, Sensitivity, Specificity

# Accuracy

# % di casi classificati correttamente:
round( sum( diag( tab ) ) / sum( tab ), 2 )

# % di casi misclassificati:
round( ( tab [ 1, 2 ] + tab [ 2, 1 ] ) / sum( tab ), 2 )

# Sensitivity
sensitivita =  tab [ 2, 2 ] /( tab [ 2, 1 ] + tab [ 2, 2 ] ) 
sensitivita

# Specificity 
specificita = tab[ 1, 1 ] /( tab [ 1, 2 ] + tab [ 1, 1 ] )
specificita



## 3. Curva ROC

# Costruire la Curva ROC a partire dai valori predetti per la risposta dal modello `mod5` 
# dell'analisi della variabile chd.

# Le curve ROC (Receiver Operating Characteristic, anche note come Relative Operating Characteristic) 
# sono degli schemi grafici per un classificatore binario.

# Una curva ROC è il grafico dell'insieme delle coppie (FP, TP) al variare di un parametro del classificatore. 

fit5 = mod5$fitted


#media campionaria della prob di malattia del campione (da verificare) 

soglia_roc  = seq( 0, 1, length.out = 2e2 )
lens = length( soglia_roc )-1
ascissa_roc  = rep( NA, lens )
ordinata_roc = rep( NA, lens )

for ( k in 1 : lens )
{
  soglia = soglia_roc [ k ]
  
  classification = as.numeric( sapply( fit5, function( x ) ifelse( x < soglia, 0, 1 ) ) )
  
  #  ATTENZIONE, voglio sulle righe il vero e sulle colonne il predetto
  # t.misc = table( datacardio$chd, classification )
  
  ordinata_roc[ k ] = sum( classification[ which( cardiodata$chd == 1 ) ] == 1 ) /
    length( which( cardiodata$chd == 1 ) )
  
  ascissa_roc[ k ] = sum( classification[ which( cardiodata$chd == 0 ) ] == 1 ) /
    length( which( cardiodata$chd == 0 ) )
  
  # ordinata_roc [ k ]  = t.misc [ 1, 1 ] /( t.misc [ 1, 1 ] + t.misc [ 1, 2 ] )
  #
  # ascissa_roc [ k ]  = t.misc [ 2, 1 ] /( t.misc [ 2, 1 ] + t.misc [ 2, 2 ] )
}


# Visualizziamo la curva ROC.

plot( ascissa_roc, ordinata_roc, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity",
      main = "Curva ROC", lwd = 2, col = 'darkblue', ylim = c( 0, 1 ), xlim = c( 0, 1 ) )
abline( h = c( 0, 1 ), v = c( 0, 1 ), lwd = 1, lty = 2, col = 'red' )
abline( a = 0, b = 1, lty = 2, col = 'black' )

# qual era il nostro punto?
abline( v = 1 - specificita,  h = sensitivita, lty = 3, col = 'blue' )
points( 1 - specificita, sensitivita, pch = 4, lwd = 3, cex = 1.5, col = 'blue' )

# Le linee tratteggiate corrispondono alle due metriche calcolate con la threshold = 0.5 che abbiamo scelto. 

# Attraverso l'analisi delle curve ROC si valuta la capacità del classificatore calcolando 
# l'area sottesa alla curva ROC (Area Under Curve, AUC). 

# R fa tutto in automatico --> calcola AUC e p0ottimale 

PRROC_obj <- roc.curve(scores.class0 = fit5, weights.class0=as.numeric(paste(cardiodata$chd)),
                       curve=TRUE)
x11()
plot(PRROC_obj)


# Se AUC < 0.5, possiamo invertire i positivi e i negativi.


#---------------------------------------------------------------------------------

# FACCIO MODELLO CON adiposity RISPOSTA E age, alcohol, obesity COVARIATE

rid = lm( adiposity ~ age + alcohol + obesity, data = cardiodata )
summary(rid)

rid2 = lm( adiposity ~ age + obesity, data = cardiodata )
summary(rid2)

plot( rid2$fit, rid2$res, xlab = "Fitted", ylab = "Residuals", 
      main = "Residuals vs Fitted Values", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )  # verifico omoschedasticità --> lo sono

qqnorm( rid2$res, ylab = "Raw Residuals", pch = 16 )
qqline( rid2$res )
shapiro.test( rid2$res ) # --> fa schifo, non sono normali

m = min(rid2$res)
m
M = max(rid2$res)
M
rid2$res[45] <- NA
rid2$res
rid2$res[462] <- NA
rid2$res[432] <- NA

b = boxcox(adiposity ~ age + obesity, data = cardiodata)
names(b)
best_lambda_ind = which.max( b$y )
best_lambda = b$x[ best_lambda_ind ]
best_lambda
rid1 = lm( (sbp ^ best_lambda - 1)/best_lambda ~ ldl + obesity + alcohol, data = cardiodata )
summary(rid1)
mod1_res = rid1$residuals/summary( rid1 )$sigma
plot( rid1$fitted, mod1_res, xlab = 'Fitted values',  ylab = 'Standarzized residuals'  ) 
qqnorm( mod1_res )
abline( 0, 1, col = 'red' )
shapiro.test( residuals( rid1 ) )  # --> applicando box-cox peggiora
