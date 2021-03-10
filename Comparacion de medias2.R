library(moments)
library(nortest)
library(openxlsx)

data <- read.xlsx('257OCTOCRILENO.xlsx', sheet = 'Hoja1')


==== # PRUEBAS DE NORMALIDAD ====
octocrileno <- data[1:3]
avobenzona <- data[4:6]
octisalato <- data[7:9]

hist(octocrileno$OCTOCRILENOTEC1)
hist(octocrileno$OCTOCRILENOTEC2)
hist(octocrileno$OCTOCRILENOAQ)

# si p< 0.05 entonces rechazo la H0
# si p>0.05 acepto H0
# H0 es normalidad
# H1 no normal 

## Normalidad Octocrileno shapiro
shapiro.test(na.omit(octocrileno$OCTOCRILENOTEC1))
shapiro.test(na.omit(octocrileno$OCTOCRILENOTEC2))
shapiro.test(na.omit(octocrileno$OCTOCRILENOAQ))

## Normalidad avobenzona shapiro
shapiro.test(na.omit(avobenzona$AVOBENZONATEC1))
shapiro.test(na.omit(avobenzona$AVOBENZONATEC2))
shapiro.test(na.omit(avobenzona$AVOBENZONAAQ))

## Normalidad octisalato shapiro
shapiro.test(na.omit(octisalato$OCTISALATOTEC1))
shapiro.test(na.omit(octisalato$OCTISALATOTEC2))
shapiro.test(na.omit(octisalato$OCTISALATOAQ))

-----------------------
## Normalidad Octocrileno jarque.test
jarque.test(na.omit(octocrileno$OCTOCRILENOTEC1))
jarque.test(na.omit(octocrileno$OCTOCRILENOTEC2))
jarque.test(na.omit(octocrileno$OCTOCRILENOAQ))

## Normalidad avobenzona jarque.test
jarque.test(na.omit(avobenzona$AVOBENZONATEC1))
jarque.test(na.omit(avobenzona$AVOBENZONATEC2))
jarque.test(na.omit(avobenzona$AVOBENZONAAQ))

## Normalidad octisalato jarque.test
jarque.test(na.omit(octisalato$OCTISALATOTEC1))
jarque.test(na.omit(octisalato$OCTISALATOTEC2))
jarque.test(na.omit(octisalato$OCTISALATOAQ))

-----------------------
  ## Normalidad Octocrileno jarque.test
ad.test(na.omit(octocrileno$OCTOCRILENOTEC1))
ad.test(na.omit(octocrileno$OCTOCRILENOTEC2))
ad.test(na.omit(octocrileno$OCTOCRILENOAQ))

## Normalidad avobenzona jarque.test
ad.test(na.omit(avobenzona$AVOBENZONATEC1))
ad.test(na.omit(avobenzona$AVOBENZONATEC2))
ad.test(na.omit(avobenzona$AVOBENZONAAQ))

## Normalidad octisalato jarque.test
ad.test(na.omit(octisalato$OCTISALATOTEC1))
ad.test(na.omit(octisalato$OCTISALATOTEC2))
ad.test(na.omit(octisalato$OCTISALATOAQ))



---------------------------
#curtosis # debe ser cercano a 3
kurtosis(data$TECNIMICRO)
kurtosis(data$AQ)

-------------------------
# Asimetria Octocrileno debe ser cercana a cero
skewness(octocrileno$OCTOCRILENOTEC1)
skewness(octocrileno$OCTOCRILENOTEC2)
skewness(octocrileno$OCTOCRILENOAQ)

# Asimetria avobenzona debe ser cercana a cero
skewness(avobenzona$AVOBENZONATEC1)
skewness(avobenzona$AVOBENZONATEC2)
skewness(avobenzona$AVOBENZONAAQ)

# Asimetria octisalato debe ser cercana a cero
skewness(octisalato$OCTISALATOTEC1)
skewness(octisalato$OCTISALATOTEC2)
skewness(octisalato$OCTISALATOAQ)
-----------------------------------------------------

#######################################
# Varianza iguales ?
# si p< 0.05 entonces rechazo la H0
# si p>0.05 acepto H0

### igualdad de varianzas octocrileno



var.test(x = data$TECNIMICRO,
         y = data$AQ)

#h0 Varianza iguales
#h1 Varianzas distintas
## R, las varianzas son iguales 


==== # Contraste para varianzas iguales ====

### Contraste avobenzona
# si p< 0.05 entonces rechazo la H0
# si p>0.05 acepto H0
# Ho : la media de A =  a la media de B
# H1 : la media de A != a la media de B

### Contraste octocrileno

t.test(x = octocrileno$OCTOCRILENOTEC1,
       y = octocrileno$OCTOCRILENOAQ,
       var.equal = T, paired = F)


t.test(x = avobenzona$AVOBENZONATEC1,
       y = avobenzona$AVOBENZONAAQ,
       var.equal = T, paired = F)


t.test(x = octisalato$OCTISALATOTEC1,
       y = octisalato$OCTISALATOAQ,
       var.equal = T, paired = F)

t.test(x = data$TECNIMICRO,
       y = data$AQ, var.equal = T,
      paired = F)


# Las medias son diferentes 








---------------------------------------------------------
==== # ANALISIS ANOVA ====

data2 <- read.xlsx('257OCTOCRILENO.xlsx', sheet = 'Hoja1')
# Si p valor menor a 0.05 rechazo la H0
# Si p valor mayor a 0.05 acepto la H0
# H0: las medias son iguales
# H1: Las medias son diferentes

incrustado <- stack(data2)
boxplot(values ~ ind, data = incrustado)

# anova Octocrileno

octo <- stack(octocrileno)
boxplot(values ~ ind, data = octo) 
aovoctocrileno <- aov(values ~ ind, data = octo)
summary(aovoctocrileno)
TukeyHSD(aovoctocrileno)

# anova Avobenzona

avo <- stack(avobenzona)
boxplot(values ~ ind, data = avo) 
aovavobenzona <- aov(values ~ ind, data = avo)
summary(aovavobenzona)
TukeyHSD(aovavobenzona)

# anova Avobenzona

octi <- stack(octisalato)
boxplot(values ~ ind, data = octi) 
aovoctisalato <- aov(values ~ ind, data = octi)
summary(aovoctisalato)
TukeyHSD(aovoctisalato)


-----------------------------------------------------


TukeyHSD(results)
# Para TEC1 y TEC2 las medias no son diferentes, pero para los contrastes 
# con AQ si son diferentes 

---------------------------------------------
  



##########################################################################
##########################################################################
##########################################################################
#############################  FIS 182  #################################


fis <- read.xlsx('257OCTOCRILENO.xlsx', sheet = 'Hoja2')
