rm(list = ls())

#install.packages("geoR")
library(geoR)

#Cargar ejemplo de datos para la Region de Parana (Brasil); 
#el set de datos viene en el paquete geoR
data(parana)

#Generamos un Resumen de los datos
summary(parana)

#Generamos un plot con los 4 cuartiles de las estaciones
plot(parana)


#Generamos un variograma: El veriograma toma la distancia entre dos estaciones vs 
# 1/2 varianza de la variable a la misma distancia

#hint = la distancia maxima del variograma es la mitad de la distancia maxima entre los puntos
#mas lejanos
plot(variog(parana, option = "cloud", max.dist = 309.75))



#Tenemos que agrupar nuestras varianzas en secciones o "bins"
vario_manual <- variog(parana, trend = "cte",
                       max.dist=309.75,
                       uvec=seq(0.0,309.75,length.out = 10)) 


plot(vario_manual)

#Comparamos con una regresion lineal para el valor esperado mu
plot(variog(parana, trend = "1st", max.dist = 309.75, 
            uvec=seq(0.0,309.75,length.out = 10)))

#Comparamos con una regresion cuadratica para el valor esperado mu
plot(variog(parana, trend = "2nd", max.dist = 309.75, 
            uvec=seq(0.0,309.75,length.out = 10)))

vario_model <- variog(parana, trend = "2nd", max.dist = 309.75, 
                      uvec=seq(0.0,309.75,length.out = 10))

###Generamos un grafico para investigar la anisotropia de los datos
plot(variog4(parana, trend = "2nd", max.dist = 309.75), omnidirectional = T)


####################################################################################
#####   Probamos distintos tipos de regresiones y vemos cual se ajusta mejor #######
#####################################################################################

#Regresion exponencial con intercepto = 0
exp_fit_fix <- variofit(vario_model, cov.model = "exp", fix.nugget = T)

#Regresion exponencial con intercepto libre
exp_fit_nofix <- variofit(vario_model, cov.model = "exp", fix.nugget = F)

#Regresion esferica con intercepto = 0
sph_fit_fix <- variofit(vario_model, cov.model = "sph", fix.nugget = T)

#Regresion esferica con intercepto libre
sph_fit_nofix <- variofit(vario_model, cov.model = "sph", fix.nugget = F)


###Visualizacion de los datos
plot(vario_model,pch=16)
lines(exp_fit_fix,col="green",lwd=4, lty = 1)
lines(exp_fit_nofix,col="red",lwd=4, lty = 2)
lines(sph_fit_fix,col="blue",lwd=4, lty = 3) 
lines(sph_fit_nofix,col="black",lwd=4, lty = 4) 



#Seleccion del mejor modelo ### Anotamos los valores ###
(exp_SSQ_fix <- summary(exp_fit_fix)$sum.of.squares)

(exp_SSQ_nofix <- summary(exp_fit_nofix)$sum.of.squares)

(sph_SSQ_fix <- summary(sph_fit_fix)$sum.of.squares)

(sph_SSQ_nofix <- summary(sph_fit_nofix)$sum.of.squares)


####GENERACION DE UNA GRILLA PARA LLENAR######
prediction_grid <- expand.grid(seq(0, 800, length.out = 100), seq(0, 800, length.out = 100))
plot(prediction_grid)

#APLICAMOS EL MODELO SELECCIONADO
krig_rain <- krige.conv(parana, loc=prediction_grid,
                        krige=krige.control(obj.model=exp_fit_nofix))

#Graficamos nuestros datos de la prediccion
image(krig_rain,col=heat.colors(100))




