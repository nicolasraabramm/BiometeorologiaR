#comando para borrar todas las variables en "Environment"
rm(list = ls())


#Declaramos nuestras constantes
T_ave = 10 #[C]

T_max = 25 #[C]
T_min = 5 #[C]
A_0 = (T_max-T_min)/2


rho_s = 1.6   #[Mg m^-3 Densidad del suelo]
K_s = 0.4     #[W m^-1 K^-1 conductancia del suelo]
C_s = 1.1     #[MJ m^-3 K^-1 Capacidad Termica del suelo]
k = K_s/(rho_s*C_s)

#declaramos w que nos da el periodo de la funcion
w = 2*pi/(24)

D = sqrt(2*k/w)




hours = seq(from= 0, to =24, by = 1) #[h] horas (generarmos un vector de horas que vayan de 0 a 24)
depth = seq(from = 0, to = 5, by = 0.1) # [m] profundidad (generamos un vector de profundidad de 0 a 3 m con intervalos de 0.1 m)



#generamos un vector de datos nulos para rellenarlos con los valores de T_soil por hora
Temp_0m <- matrix(nrow = 1, ncol = length(hours), data = NA) 

#Generaremos nuestro primer loop
for(i in 1:length(hours)){
  
  Temp_0m[i] <- T_ave + A_0*sin(w*(hours[i]-8))   #eq. 8.5 Campbell y Norman
  
  }


#realizaremos nuestro primer grafico!!!!!!!
plot(x = hours,  y = Temp_0m)


##DOBLE LOOP########

#Aca generamos una matriz de numero de filas en funcion de la profundidad, y numero de columnas en funcion de las horas
#y la poblamos de valores nulos -> NA 
Temp <- matrix(nrow = length(depth), ncol = length(hours), data = NA)


#Generamos un doble loop
for (i in 1:length(depth)){
  for(j in 1:length(hours)){
    

    Temp[i,j] <- T_ave + A_0 *exp(-depth[i]/D)*sin(w*(hours[j]-8)-depth[i]/D) #eq 8.6 Campbell y Norman
    
    
    }
}

###GRAFICO DE TEMPERATURA EN FUNCION DEL TIEMPO###

#Varias lineas en un solo grafico
plot(x = hours, y = Temp[1,], type = 'l', col = 'red', ylim = c(0,20), xlim = c(0,24), xlab ="Hours (H)", ylab ="Temperature (C)")
lines(x = hours, y = Temp[6,], col = "chartreuse4") #Segundo grafico para profunidad 0.5 m
lines(x = hours, y = Temp[11,], col = "blue") #Tercer grafico para profundidad de 1 m
#Add legend to the plot
legend(0,20, legend = c("0 m","0.5 m","1 m"), col = c("red","chartreuse4","blue"),lty=1:1, cex=0.8, title = "Depth")



###GRAFICO DE TEMPERATURA EN FUNCION DE LA PROFUNIDAD PARA DISTINTAS HORAS###

plot(x = Temp[,1], y = -depth, type = "l", col = "red", xlim = c(0,30), ylim = c(-5,0),
     xlab = "Temperature (C)", ylab = "Depth (m)")  #Linea para las 12 de la Noche
lines(x = Temp[,13], y= -depth, col= "chartreuse4" )  #Linea para las 12 de la tarde 
lines(x = Temp[,21], y = -depth, col = 'blue') #Linea para las 8 de la noche
#Add legend 
legend(20,-0.5, legend = c("11:59 pm","12:01 pm"," 8:00 pm"), col = c("red","chartreuse4","blue"),lty=1:1, cex=0.8, title = "Time of the Day")



#####################
#####Grafico 3D######
#####################


persp(x=depth,y = hours, z = Temp, theta = 120,phi = 10, expand = .5, col=c('#bbddff', '#ddbbff'),
      shade  =  0.2)






