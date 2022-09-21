rm(list = ls())

#install.packages('plantecophys')   #instalar el paquete "plantecophys". Solo se hace una sola vez
library(plantecophys) #Llamar al paquete "plantecophys" para que sus funciones queden a nuestra disposicion
                      #en el script. Hacerlo siempre que comencemos con un nuevo script y queramos utilizar este .paquete



#Comenzaremos con una curva modelo A:Ci para entender las funciones
load('/Users/nicolasraab/Downloads/acidata1.rda')

#ploteamos An (Photosyntesis vs COncentracion interna de CO2 en la hoja (Ci))
plot(x= acidata1$Ci, y= acidata1$Photo, xlab = "Ci (ppm)", ylab = "Photosynthesis umol CO2 m^-2 s^-1)") #Hace sentido?

#### #### #### #### #### #### #### 
#### Fitting A:Ci curves  #####
################################       

leaf_fitting <- fitaci(
  data = acidata1,
  varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci", PPFD = "PARi", Rd =
                    "Rd"),
  Tcorrect = TRUE,
  Patm = 103.1,   
  citransition = NULL,
  quiet = FALSE,
  startValgrid = TRUE,
  fitmethod = c("default"),   
  algorithm = "default",
  fitTPU = FALSE,
  alphag = 0,
  useRd = TRUE,
  PPFD = 1800,  
  Tleaf = 33,  
  alpha = 0.24,
  theta = 0.85,
  gmeso = NULL,
  EaV = 82620.87,
  EdVC = 0,
  delsC = 645.1013,
  EaJ = 39676.89,
  EdVJ = 2e+05,
  delsJ = 641.3615,
  GammaStar = NULL,
  Km = NULL,
  id = NULL
)

#Chequeemos nuestras variables
leaf_fitting$pars



#Usemos datos de Quercus robum provenientes de Wytham Woods
#Descargamos de Github una curva A:Ci de Quercus robur de Inlaterra
Ci_Oak <- read.csv("/Users/nicolasraab/Desktop/Biometeorologia/BiometeorologiaR/Oak_Ci.csv", header = TRUE)


#Chequemos nuestra curva
plot(x= Ci_Oak$Ci, y =Ci_Oak$Photo)

Oak_Fitting <- fitaci(
  data = Ci_Oak,
  varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci", PPFD = "PARi", Rd =
                    "Rd"),
  Tcorrect = TRUE,
  Patm = mean(Ci_Oak$Press),   
  citransition = NULL,
  quiet = FALSE,
  startValgrid = TRUE,
  fitmethod = c("default"),   
  algorithm = "default",
  fitTPU = FALSE,
  alphag = 0,
  useRd = TRUE,
  PPFD = mean(Ci_Oak$PARi),  
  Tleaf = mean(Ci_Oak$CTleaf),  
  alpha = 0.24,
  theta = 0.85,
  gmeso = NULL,
  EaV = 82620.87,
  EdVC = 0,
  delsC = 645.1013,
  EaJ = 39676.89,
  EdVJ = 2e+05,
  delsJ = 641.3615,
  GammaStar = NULL,
  Km = NULL,
  id = NULL
)

#Chequeamos los parametros obtenidos
Oak_Fitting$pars


####################################################################################
########## De nuestras mediciones en Quercus robur ######################################
########## Obtengamos los valores de ajuste para nuestro modelo de conductancia #######
###########################################################################################

#Descargar Archivo del IRGA con valores de fotosintesis y variables ambientales.
Photo_Oak <- read.csv("/Users/nicolasraab/Desktop/Biometeorologia/BiometeorologiaR/Fotosintesis_Conductancia_Oak.csv", header = TRUE)

#Determinamos las constantes para el modelode de Leuning Ball and Berry
Tau = 25
Leuning_factor <- Photo_Oak$Photo/((Photo_Oak$CO2R-Tau)*(1+Photo_Oak$VpdL/5))

plot(x= Leuning_factor, y = Photo_Oak$Cond) #Grafiquemos Conductancia 
Leuning_Factors <- lm(Photo_Oak$Cond~Leuning_factor)
abline(Leuning_Factors, col = "chartreuse4")

  
#Modelemos la fotosintesis en base a las constantes que obtuvimos de la curva A:Ci

Photo_Modelled <- Photosyn(
    VPD = Photo_Oak$VpdL,
    Ca = Photo_Oak$CO2R,
    PPFD = Photo_Oak$PARi,
    Tleaf = Photo_Oak$Tleaf,
    Patm = Photo_Oak$Press,
    RH = NULL,
    gsmodel = "BBLeuning",
    g1 = 5.74,
    g0 = 0.07404,
    gk = NULL,
    vpdmin = 1.5,
    D0 = 5,
    GS = NULL,
    BBmult = 5,
    alpha = 0.24,
    theta = 0.85,
    Jmax = 101.10,
    Vcmax = 56.1632151,
    gmeso = NULL,
    TPU = 20,
    alphag = 0,
    Rd0 = 0.97,
    Q10 = 1.92,
    Rd = 1,
    TrefR = 25,
    Rdayfrac = 1,
    EaV = 58550,
    EdVC = 2e+05,
    delsC = 629.26,
    EaJ = 29680,
    EdVJ = 2e+05,
    delsJ = 631.88,
    GammaStar = NULL,
    Km = NULL,
    Ci = Photo_Oak$Ci,
    Tcorrect = TRUE,
    returnParsOnly = FALSE,
    whichA = c("Ac")
  )
  
  
  
  #Comparemos nuestra fotosintesis observada vs fotosintesis modelada
   plot(x = Photo_Oak$Photo, y = Photo_Modelled$ALEAF)
   regresion_lineal <- lm(Photo_Modelled$ALEAF ~ Photo_Oak$Photo)
   abline(regresion_lineal, col = "red")
   
   summary(regresion_lineal) 
