rm(list = ls())

#library(readxl)
library(dplyr)


# my_data <- read_excel("/Users/nicolasraab/Dropbox/Tesis2/Raw Data/Datos Torre 2011/Año 2011/2011_2.xlsx", sheet = "2011y2012")
# 
# name(my_data$"LxE W/m2")
# 
# Eddy_covariance <- my_data %>% select(Año,Mes,Dia,Hora,Minutos,"LxE W/m2",u,v,T_centig,rhoa,"P kPa","HR (%)","NR (w/M2)","G (w/M2)")
# 
# 
# Eddy_covariance <- rename(Eddy_covariance, Presion_kPa = `P kPa`)
# Eddy_covariance <- rename(Eddy_covariance, Rn_Wm2 = `NR (w/M2)`)
# Eddy_covariance <- rename(Eddy_covariance, G_Wm2 = `G (w/M2)`)
# Eddy_covariance <- rename(Eddy_covariance, LE_Wm2 = `LxE W/m2`)
# Eddy_covariance <- rename(Eddy_covariance, HR = `HR (%)`)
# 
# Eddy_covariance$Presion_kPa <- as.numeric(Eddy_covariance$Presion_kPa) 
# Eddy_covariance$v <- as.numeric(Eddy_covariance$v)
# Eddy_covariance$rhoa <- as.numeric(Eddy_covariance$rhoa)
# 
# 
# write.csv(Eddy_covariance,file = "/Users/nicolasraab/Desktop/Biometeorologia/BiometeorologiaR/Data_LE.csv",
#           row.names = FALSE)

Estacion_Meteorologica <- read.csv("/Users/nicolasraab/Desktop/Biometeorologia/BiometeorologiaR/Data_LE.csv", header = TRUE)

##############
##Constantes##

Lambda <- 2450  #[kJ Kg-1] Calor latente de vaporizacion
Cp <- 1.006 #[kJ kg-1 K-1] Calor especifico del aire seco
epsilon <- 0.662 #mixing ratio vapor de agua/aire seco
R <- 8.31 #[J K-1 mol-1] Constante de los gases ideales
Mw <- 18*10^(-3) #[kg mol-1] Peso molecular del agua

#Constantes para la resistencia aerodinamica
h <- 0.12 #[m] altura de un pasto ideal
d <- 2/3 #[m] zero plane displacement
Zm <- 2.5 #[m] altura medicion velocidad del viento
Zh <- Zm #[m] altura medicion presion parcial de vapor
Zom <- 0.123*h #[m] altura de rugosidad para el momentum
Zoh <- 0.1*Zom #[m] altura de rugosidad para el intercambio de energia
k <- 0.4 #Constante de Von Karman

#Resistencia de la canopia
r_c <- 70 #[s m-1] Resistencia de la canopia para una superficie estandar (pasto)
 


##############################
#####Nuestro Tool Kit######
##############################

#Funcion para estima la velocidad del viento [m s-1]
u_viento <- function(a,b){
  u <- sqrt(a^2+b^2)
  return(u)
}


#funcion para la presion de vapor de agua a saturacion en kPa
e_s <- function(T_celsius){
es <- 0.6108*exp((17.27*T_celsius)/(T_celsius+237.3))
return(es)
}


#Creamos una funcion para Gamma
gamma <- function(Pa_kPa){
g <- Cp*Pa_kPa/(Lambda*epsilon)
return(g)
}


#Creamos una funcion para Delta (slope)
Delta <- function(T_celsius,es){
  slope <- (Lambda*1000*Mw*es)/(R*(T_celsius+237.3)^2)
  return(slope)
}


#Creamos una funcion para estimar la presion de vapor parcial en funcion
#de la T_celsius y HR%
e_a <- function(es,HR){
  e_a <- es*HR/100
  return(e_a)
  }

#Resistencia aerodinamica en s m-1
r_a <- function(u_viento){
  ra <- (log((Zm-d)/Zom)*log((Zh-d)/Zoh))/(k^2*u_viento)
  return(ra)
}

#Desarrollamos Nuestra ecuacion de Penman-Monteith

PM <- function(Delta,Rn,G,rho,es,ea,ra,gamma){
  
  Penman_Monteith <- (Delta*(Rn-G)+rho*Cp*(es-ea)*ra^(-1))/(Delta+gamma*(1+r_c/ra))
  return(Penman_Monteith)
}
  
#################################################



ET <- Estacion_Meteorologica %>% filter(Año == 2011 & Mes == 2 & Dia ==1)

#Agregamos Gamma a nuestro df
ET <- ET %>% mutate(Gamma = gamma(Presion_kPa), .after = 'Presion_kPa')
#Agregamos Presion de saturacion de vapor de agua df
ET <- ET %>% mutate(es = e_s(T_centig), .before = "HR")
#Agregamos Presion parcial de vapor de agua df
ET <- ET %>% mutate(ea = e_a(es,HR), .after = "es")
#Agregamos Delta (slope) al df
ET <- ET %>% mutate(delta = Delta(T_centig,es), .after = "Gamma")
#Agregamos la velocidad del viento a la df
ET <- ET%>% mutate(u_viento = u_viento(u,v), .after = "v")
#Agregamos resistencia aerodinamica a la df
ET <- ET %>% mutate(ra = r_a(u_viento), .after = "u_viento")


#Calculamos Penman-Monteith utilizando la ecuacion ya establecida en el script
ET <- ET %>% mutate(ET0 = PM(delta,Rn_Wm2,G_Wm2,rhoa,es,ea,ra,Gamma))

#Filtramos valores nocturnos y llevamos a 0
ET$ET0[ET$Rn_Wm2 <0] <- 0

#obtenemos kJ de evapotranspiracion (LE0) cada 30 min
ET <- ET %>% mutate(ET0_30min_kJ = ET0*30*60/1000, .after = "ET0") #[]

#TADAaaaaaaaaa
ET_dia <- sum(ET$ET0_30m)/Lambda


