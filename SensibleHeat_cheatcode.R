rm(list = ls())

#Constantes
rho = 1.2   #[kg m^-3] Densidad del aire a 20 C y nivel del mar
Cp = 1.012  #[1.012 J kg^-1 K^-1] Calor especifico del aire a 20 C




######## Estimacion de la resistencia aerodinamica ######
A = 0.60  #Taken from Table
n = 0.5   #Taken from Table
K = 21.5*1e-6


Velocity <- c(0.1, 1, 2, 3, 4, 5, 6)

vis = 15.1*1e-6   #viscosidad cinematica del aire

d = 0.10  #[m] Largo de la hoja

Re <- Velocity*d/vis  

Nu <- A*Re^n

r_h <- d/(K*Nu)



plot(x = Velocity, y = r_h, type = "line")
