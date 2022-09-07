#comando para borrar todas las variables en "Environment"
rm(list = ls())

T_ave = 10 #[C]

T_max = 25 #[C]
T_min = 5 #[C]
A_0 = (T_max-T_min)/2


  

w = 2*pi/(24)

rho_s = 1.6   #[Mg m^-3]
K_s = 0.4     #[W m^-1 K^-1]
C_s = 1.1     #[MJ m^-3 K^-1]
k = K_s/(rho_s*C_s)

D = sqrt(2*k/w)

hours = c(0:24) #[h] horas
depth = seq(from = 0, to = 5, by = 0.1) # [m] profundidad

Temp <- matrix(nrow = length(depth), ncol = length(hours), data = NA)

for (i in 1:length(depth)){
  for(j in 1:length(hours)){
    
    if (depth[i] == 0){
    Temp[i,j] <-  T_ave + A_0*sin(w*(hours[j]-8))
    }
    
    Temp[i,j] <- T_ave + A_0 *exp(-depth[i]/D)*sin(w*(hours[j]-8)-depth[i]/D)
    
    
    }
}


Temp_0 <- T_ave + A_0*sin(w*(hours-0))
Temp_8 <- T_ave + A_0*sin(w*(hours-8))

plot(x = hours, y = Temp_0, xlab ="Hours (H)", ylab ="Temperature (C)")
lines(x = hours, y = Temp_8)


plot(x = hours, y = Temp[1,], type = 'l', col = 'red', ylim = c(0,20), xlim = c(0,24), xlab ="Hours (H)", ylab ="Temperature (C)")
lines(x = hours, y = Temp[11,], col = 'blue')
lines(x = hours, y = Temp[6,], col = "chartreuse4")
legend(0,20, legend = c("0 m","0.5 m","1 m"), col = c("red","chartreuse4","blue"),lty=1:1, cex=0.8, title = "Depth")



plot(x = Temp[,1], y = -depth, type = "l", col = "red", xlim = c(0,30), ylim = c(-5,0),
     xlab = "Temperature (C)", ylab = "Depth (m)")
lines(x = Temp[,21], y = -depth, col = 'blue')
lines(x = Temp[,13], y= -depth, col= "chartreuse4" )
legend(20,-0.5, legend = c("11:59 pm","12:01 pm"," 8:00 pm"), col = c("red","chartreuse4","blue"),lty=1:1, cex=0.8, title = "Time of the Day")





persp(x=depth,y = hours, z = Temp, theta = 120,phi = 10, expand = .5, col=c('#bbddff', '#ddbbff'),
      shade  =  0.2)






