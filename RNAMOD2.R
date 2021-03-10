library(readxl)
library(neuralnet)
library(tidyverse)
teoricoFPS <- data.frame(Lo = c(290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,
                                342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,
                                394,395,396,397,398,399,400),
                         num = c(0.000008741,0.000014500,0.000026589,0.000045745,0.000100570,0.000258940,0.000703490,0.001677600,0.003726800,0.006393187,0.009588049,0.013131046,0.017567581,0.021086352,0.023715829,0.026726464,0.028583575,0.028358035,
                                 0.028506361,0.026758044,0.025008033,0.023220870,0.020822301,0.019001980,0.016047196,0.014047820,0.012106600,0.010240148,0.008655035,0.007303116,0.006187458,0.005075764,0.004257950,0.003553161,0.002873215,0.002401862,
                                 0.001968354,0.001608549,0.001330636,0.001264272,0.001229464,0.001207681,0.001201445,0.001161920,0.001120934,0.001098896,0.001071787,0.001046995,0.001011272,0.000996670,0.000960128,0.000939689,0.000912209,0.000880889,
                                 0.000859406,0.000833232,0.000810996,0.000784675,0.000761766,0.000726994,0.000714960,0.000688373,0.000663663,0.000640771,0.000621154,0.000601738,0.000575523,0.000558135,0.000532600,0.000518882,0.000502133,0.000479621,
                                 0.000462253,0.000443592,0.000421025,0.000405759,0.000380740,0.000366804,0.000344847,0.000326106,0.000307698,0.000288918,0.000271046,0.000251022,0.000234043,0.000213866,0.000199164,0.000179991,0.000163282,0.000147280,
                                 0.000129612,0.000115459,0.000101508,0.000086026,0.000072457,0.000060951,0.000050624,0.000040928,0.000033131,0.000026426,0.000020489,0.000015605,0.000011661,0.000008567,0.000006000,0.000004170,0.000002887,0.000001888,
                                 0.000001239,0.000000780,0.000000507))
teoricoUVA <- data.frame(Lo = c(320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,
                                368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400),
                         numuva = c(0.0000048,0.0000083,0.0000129,0.0000192,0.0000273,0.0000376,0.0000488,0.0000627,0.0000788,0.0000942,0.0001129,0.0001313,0.0001493,0.0001649,0.0001842,0.0001991,0.0002154,0.0002288,
                                    0.0002413,0.0002509,0.0002599,0.0002769,0.0002925,0.0003073,0.0003203,0.0003341,0.0003456,0.0003554,0.0003684,0.0003743,0.0003834,0.0003905,0.0003953,0.0003978,0.0004022,0.0004014,
                                    0.0004046,0.0004057,0.0004057,0.0004058,0.0004054,0.0004016,0.0003992,0.0003913,0.0003861,0.0003795,0.0003702,0.0003614,0.0003533,0.0003430,0.0003292,0.0003159,0.0003002,0.0002866,
                                    0.0002709,0.0002563,0.0002424,0.0002281,0.0002130,0.0001966,0.0001791,0.0001636,0.0001465,0.0001298,0.0001134,0.0000980,0.0000833,0.0000702,0.0000581,0.0000472,0.0000382,0.0000301,
                                    0.0000233,0.0000176,0.0000130,0.0000095,0.0000068,0.0000048,0.0000032,0.0000021,0.0000013))
#Cargar datos para RNA
# df <- read_xlsx("ArchivoDatosSimulador.xlsx", sheet = "filtrosvsabs")
# Se cargan los datos teoricos del espectro de accion eritemal por UV-SSR source
# teoricoFPS <-read_xlsx("ArchivoDatosSimulador.xlsx", sheet = "teoricofps")
## Se cargan los datos teoricos de PPD action spectrun * P(λ)*UVA radiation source
# teoricoUVA <-read_xlsx("ArchivoDatosSimulador.xlsx", sheet = "teoricouva")


## Crear data a predecir

entradas <- data.frame(EMH = 6, ZnO =6)
rnMOD2<- load(file = 'rnMOD2.rda')
# Red neuronal para predicción
# set.seed(123)
# rn <- neuralnet(a290+a291+a292+a293+a294+a295+a296+a297+a298+a299+a300+
#                   a301+a302+a303+a304+a305+a306+a307+a308+a309+a310+a311+
#                     a312+a313+a314+a315+a316+a317+a318+a319+a320+a321+a322+
#                     a323+a324+a325+a326+a327+a328+a329+a330+a331+a332+a333+
#                     a334+a335+a336+a337+a338+a339+a340+a341+a342+a343+
#                     a344+a345+a346+a347+a348+a349+a350+a351+a352+a353+a354+
#                     a355+a356+a357+a358+a359+a360+a361+a362+a363+a364+a365+
#                     a366+a367+a368+a369+a370+a371+a372+a373+a374+a375+a376+a377+
#                     a378+a379+a380+a381+a382+a383+a384+a385+a386+a387+a388+
#                     a389+a390+a391+a392+a393+a394+a395+a396+a397+a398+a399+
#                     a400~ EMH+ZnO, data = df, hidden = c(4,8,5,8),
#                   threshold = 0.01)

# Prediccion de las Abs con la entrada de las cantidades de EMH y ZnO
absorbancias <- neuralnet::compute(rn,entradas)
# Se transforman los resultados a un DF 
absorbanciasDF <- data.frame(absorbancias$net.result)
# Se realiza la transpuesta del DF para tener los datos en columna 
absorbanciastDF <- data.frame(t(absorbanciasDF))
# Se renombra la columna por las letras Abs de absorbancias
colnames(absorbanciastDF)[1] <- 'Abs'
# Se crea otra columna a la matriz 
absorbanciastDF$lo <- c(290:400)
# Se une en una matriz los datos teoricos y los resultados de Abs predichoa de DF2P
matriz <- cbind(teoricoFPS, absorbanciastDF$Abs)
#Se renombra la columna de las absorbancias con las letras Abs
names(matriz)[3] <- "Abs"
# Se realiza el calculo del FPS segun ecuancion de la norma iso 24443
FPS <- round(mean(matriz$num)/mean((matriz$num*(10^(matriz$Abs*-1)))),0)
cat("El factor de proteccion solar es:", FPS)




###### Calculos UVA ########
## Se cargan los datos teoricos de PPD action spectrun * P(λ)*UVA radiation source
# teoricoUVA <-read_xlsx("ArchivoDatosSimulador.xlsx", sheet = "teoricouva")
# se unen los datos teoricos con los datos de Abs DE 320 a 400 nm
teoricoUVA <- cbind(teoricoUVA, as.data.frame(absorbanciastDF$Abs[31:111]))
#Se renombra la columna con las letras Abs de absorbancia 
names(teoricoUVA)[3] <-"Abs"
# Se define la constante C
C = 1
# Calculo del UVA segun la norma iso 24443
FPFUVA <- round(mean(teoricoUVA$numuva)/mean(teoricoUVA$numuva*(10^(teoricoUVA$Abs*-1*C))),0)
cat("El factor de proteccion UVA:", FPFUVA)


######Calclulo LOC 
# Con  el DF matriz se calcula el punto medio de los valores de Abs de la prediccion
matriz$pun_medio <- Reduce(function(a,b) 1*((a+b)/2), matriz$Abs,accumulate = T)
# Con los puntos medios calculados se calcula la suma acomulada que corresponde a la integral
matriz$integral <- Reduce(function(a,b) (a+b), matriz$pun_medio,accumulate = T)
# Se calcula la suma de los puntos medios y se determina el 90%
tot_punt_med <- sum(matriz$pun_medio)*0.90
tot_punt_med

#traer el valor de la longitud de onda critica
LOC <-  matriz %>% 
  filter(integral >= tot_punt_med) %>% 
  summarise_each(funs(
    minimo = min(.)
  ), Lo)

cat("La LOC:", LOC$minimo)


# Guardar la red neuronal 
# save(rn,file = 'rnMOD2.rda')


######################################### Validacion de la RN2 ####################################



fpsrn2 <- read.xlsx('ArchivoDatosSimulador.xlsx', sheet = 'Mod2')
fpsrn2 <- fpsrn2[1:3]

