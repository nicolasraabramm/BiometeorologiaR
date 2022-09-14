#comando para borrar todas las variables en "Environment"
rm(list = ls())




#Definimos ecuacion para calcular k_be apra una canopia con hojas
#con una distribucion elipsoidal
#Eq.15.4 (pp251 Campbell y Norman)
k_be <- function(x,psi){ #x y psi son los argumentos de la funcion
  
  tan2 <-(1-cos(2*psi))/(cos(2*psi)+1)  #Definimos funcion para calcular tan^2(psi) [sacada de https://www.wolframalpha.com/]
  
  epsilon_dist <- sqrt(x^2+tan2)/(x+1.774*(x+1.182)^(-0.733)) #estimacion de Kbe, segun la ecuacion
  
  return(epsilon_dist)  #output de la funcion
}
  


#Generamos un vector con valores de Azimuth de 0 a pi/2 
#(0 a 90 grados, o mediodia solar (pi/2))
  pi_vector <- seq(from = 0, to= pi/2, by = pi/20)
  
#Generemos distintos plots para distintos valores x en la ecuacion 15.4
  plot(x = pi_vector, y = k_be(1000,pi_vector), type = "line", col = "red", 
       xlim = c(0,pi/2), ylim = c(0,2), xlab = "Azimuth Angle (Rad)", ylab = "Ext. Coeff (K_be)")
  lines(x = pi_vector, y = k_be(3,pi_vector), col = "chartreuse4")
  lines(x = pi_vector, y = k_be(1,pi_vector), col = "violet")
  lines(x = pi_vector, y = k_be(0,pi_vector), col = "blue")
  
  legend(0,1.95, legend = c("x = 0","x = 1","x = 3", "x = 10000"), col = c("blue","violet","chartreuse4","red"),
         lty=1:1, cex=0.8)
  

# Generamos una ecuacion para la trasmisividad de la canopia 
  canopy_trans <- function(LAI,x,psi){
    
    tau <- exp(-k_be(x,psi)*LAI) #Eq. 15.1 (pp. 249 Campbell and Norman)
    
    return(tau)
    } 
  
# Graficamos la trasnmisividad de la canopia en base a un LAI = 1, distintos x 
# para varios angulos azimutales 
  
  plot(x = pi_vector, y = canopy_trans(LAI = 1,x=100000,pi_vector), type = "line", col = "red",
       xlim = c(0,pi/2), ylim = c(0,1), xlab = "Azimuth Angle (Rad)", ylab = "Transmisivity")
  lines(x = pi_vector, y = canopy_trans(LAI = 1,x=3,pi_vector), col = "chartreuse4")
  lines(x = pi_vector, y = canopy_trans(LAI = 1,x=1,pi_vector), col = "violet")
  lines(x = pi_vector, y = canopy_trans(LAI = 1,x=0,pi_vector), col = "blue")
  
  legend(0.9,1, legend = c("horizontal","x = 3","x = 1", "vertical"), col = c("red","chartreuse4","violet","blue"),
         lty=1:1, cex=0.8)
  
  
  
  
######################################################  
################## EJERCICIO 1 ########################   
######################################################  
  Cte_solar = 1361 #[W/m^2 constante solar]
  

  #Vector de 6 am a 6 pm con el fin de evitar radiaciones negativas (noche)
  time_of_day <- seq(from = 6, to =  18, by = 1)
  
  #Formula simplificada para estimar el angulo Zenital
  Zenith_angle <- -cos(time_of_day*(2*pi/24))*pi/2
  
  #Formula simplificada para estimar la radiacion incidente [w/m^2]
  Io <- -cos(time_of_day*(2*pi/24))*Cte_solar
 
  
  
  plot(x = time_of_day, y = Zenith_angle, type = "line")
  
  plot(x = time_of_day, Io*canopy_trans(LAI =1.2, x = 3, Zenith_angle), type = "line", col = "red",
       xlim = c(6,18), ylim = c(0,1500), xlab = "Time of the Day", ylab = "Radiation Intercepted [W/m2]")
  lines(x = time_of_day, Io*(1-canopy_trans(LAI =1.2, x = 3, Zenith_angle)), col = "chartreuse4")
  
  legend(6,1500, legend = c("Canopy","Soil"), col = c("chartreuse4","red"),
         lty=1:1, cex=0.8)
  
  
  
  ######################################################  
  ################## EJERCICIO 2 ########################   
  ######################################################  
  
  
  #Vector para LAI a lo largo del perfil de la canopia
  LAI <- c(0.1,0.25,0.5,0.75,1)
  
  #Vector de valores x a lo largo del perfil de la canopia
  x_value <- c(0.1, 0.5, 1, 3, 5, 10000)

  
  intercepted <- matrix(ncol = length(Zenith_angle), nrow = length(LAI), data = NA)
  rad_intercepted <- matrix(ncol = length(Zenith_angle), nrow = length(LAI), data = NA)
  rad_transmited <- matrix(ncol = length(Zenith_angle), nrow = length(LAI), data = NA)
  
  for(i in 1:length(LAI)){
    for(j in 1:length(Zenith_angle)){
    
      intercepted[i,j] <- 1-canopy_trans(psi = Zenith_angle[j], x_value[i], LAI[i])
      
      if (i == 1){
        rad_intercepted[i,j] <- Io[j]*intercepted[1,j]
        rad_transmited[i,j] <- Io[j]-rad_intercepted[i,j]
        
      } else{
        rad_intercepted[i,j] <- rad_transmited[i-1,j]*intercepted[i,j]
        rad_transmited[i,j] <- rad_transmited[i-1,j]-rad_intercepted[i,j]
         
        if(rad_transmited[i,j] <0){rad_transmited[i,j] = 0}
         
        
        }
  }
  }
  
  plot(x = time_of_day, y = rad_intercepted[1,], col = "red", type = "line", 
       xlim = c(6,18), ylim = c(0,1500),
       xlab = "Time of the Day",
       ylab = "Rad Intercepted (W/m2)")
  lines(x = time_of_day, y = rad_intercepted[2,], col = "chartreuse4")
  lines(x = time_of_day, y = rad_intercepted[3,], col = "violet")
  lines(x = time_of_day, y = rad_intercepted[4,], col = "blue")
  
  
  
