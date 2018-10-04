### xNriyB3ufAxdRmZgpq_p  ##
#ab1

#limpiar el enviroment
rm(list = ls())

# los 0s aceptados antes de expresas una cifra en notaci?n cient?fica
options("scipen"=100, "digits"=4)

###librerias
suppressMessages(library(plotly)) # 
suppressMessages(library(Quandl)) # Descarga de Precios
suppressMessages(library(PortfolioAnalytics)) ##Teoria modenar de portafolio
suppressMessages(library(ROI)) # optimización para ele portafolio 
suppressMessages(library(knitr))  
suppressMessages(library(xlsx))
suppressMessages(library(plyr))
suppressMessages(library(readxl)) 
suppressMessages(library(kableExtra)) # Tablas en HTML
options(knitr.table.format = "html") 

# Cargar el token de QUANDL
Quandl.api_key("xNriyB3ufAxdRmZgpq_p")
df
# Funcion para descagar precios
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  
  # Peticion para descargar precios
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}
#tk<-(read.xlsx(file="C:/Users/perezka/OneDrive - HP/Karla/Karla/Trading/myst_lab2_KarlaPerez/Etf_farma.xlsx", sheetName="Holdings",
#                            colIndex=1,startRow=10,endRow=54,header=FALSE))

tk<-(read.xlsx(file="C:/Users/perezka/OneDrive - HP/Karla/Karla/Trading/myst_lab2_KarlaPerez/Etf_farma.xlsx", sheetName="Holdings",
               colIndex=1,startRow=10,endRow=54,header=TRUE))
#tk<- read_excel("C:/Users/perezka/OneDrive - HP/Karla/Karla/Trading/myst_lab2_KarlaPerez/Etf_data.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

tkt_1<-as.character(tk[,1])


cs <- c("date", "adj_close")
Capital_Inicial <- 10000
fs = c("2016-01-20","2018-01-20") # por la c es vector de caracteres 

# Descargar Precios y Calcular rendimientos
Datos <- list()

for(i in 1:length(tkt_1))
  Datos[[i]] <- Bajar_Precios(Columns=cs, Ticker=tkt_1[i], Fecha_In=fs[1], Fecha_Fn=fs[2])

names(Datos) <- tkt_1
# ordenamos las fecha para que esten de pasadas a actuales
for (i in 1: length(tkt_1)){
  Datos[[i]] <- Datos[[i]][order(Datos[[i]][,1]),]
}



##-------------------
longitudes<-c()

for (i in 1:length(Datos)){
  longitudes[i]<-length(Datos[[i]]$date)
}

#maximo<-max(longitudes)
#completos<-which(longitudes==maximo)


#Solución longitudes------------------------------------
#Se obtienen los números en el vector y la frequencia de cada uno
longs<-count(longitudes)
# se obtien el valor que tuvo mayor frequencia
lon<-longs[which.max(longs$freq),1]
#se guardan los datos que tienen la misma freq
completos<-which(longitudes==lon)

#--------------------------------------------------------

DatosN <-Datos[completos]
#Vector  para almacenar las columnas
columnas<-c()
nuevos<-c()

#función para repetir la función 
Precios<-do.call(cbind,DatosN)

#Creamos el vector de columnas con los nombre
for (i in 1:length(tkt_1)){
  nuevos[i]<-paste(tkt_1[i],".adj_close",sep = "")
}


#extraemos 1 renglon poara obtner los nombres de las columnas
nombres<-colnames(Precios[1,(names(Precios) %in% nuevos)])


Precios<-Precios[,(names(Precios) %in% nuevos)]
row.names(Precios)<-Datos[[1]]$date

#reasignar
tk_completos <- as.character(tkt_1[completos])
colnames(Precios) <- tk_completos

###### Parte 2
  
Historico<-data.frame("Date" = row.names(Precios),
                      "Precios" = Precios[,1],
                      "R_Precio" = 0,
                      "R_Activo" = 0,
                      "R_Cuenta" = 0,
                      "Capital" = 0,
                      "Flotante" = 0,
                      "Balance" = 0,"Titulos" = 0 ,"Titulos_a" = 0,
                      "Operacion" = NA, "Comisiones" = 0,"Comisiones_a"= 0, "Mensaje" = NA)

#Date fecha
#Precio del activo 
#R_Precio, rendimiento diario del precio(diario, de un dia a otro)
#R_activo, rendimiento acumulado del precio 
#Balance, valor del portafolio, capital mas flotante
#titulos  ejecutados
#titulos_a titulos acumulados
#R_cuenta, balance+ capital 
#operaciones, indica si son de compra o de venta(1 compra, 0 mantengo, -1 venta)
#Comisiones 0.0025  por el valor de la transacción
# Mensaje, Texto que indique la decisión o  que informe algo ocurrido
# Flotante, el valor de los titulos acumulados por el precio del activo en el dia  pecio diario por titulos acumulados

Regla0_R <- -0.015 # Considera una oportunidad de compra  cuando el activo empiece a bajar su rendimiento de -3% o menor 
Regla1_I <- 0.1 # Porcentaje de capital que se utiliza para la operación inicial 
Regla2_P <- 0.25 # Se utiliza  el =% del L capital restante en cada compra
Regla3_W <- tk_completos # se realiza la misma estrategia para todos los activos en el portafolio 
Regla4_C <- -0.0025 #comisiones pagadas
Regla5_K <- 1000000 #Capital inicial 


# -- ----------------------------------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #

# -- Calcular los Titulos de posicion inicial
Historico$Titulos[1] <- (Regla5_K*Regla1_I)%/%Historico$Precio[1]
Historico$Titulos_a[1] <- Historico$Titulos[1]

# -- Se calculan comisiones iniciales
Historico$Comisiones[1] <- Historico$Titulos[1]*Historico$Precio[1]*Regla4_C
Historico$Comisiones_a[1] <-Historico$Comisiones[1]

# -- Calcular el Balance y flotante
#Historico$Balance[1] <-Historico$Titulos_a[1]*Historico$Precios[1] ya no por el cambio de flotante
Historico$Flotante[1] <- Historico$Precios[1]*Historico$Titulos


# -- Todo remanente se dejar? registrado en la cuenta de efectivo.
Historico$Capital[1] <- Regla5_K-Historico$Flotante[1]-Historico$Comisiones[1]
Historico$Balance[1] <- Historico$Flotante[1]+Historico$Capital[1]
# -- Iniciamos con una postura de mantener.
Historico$Operacion[1] <- "Posicion Inicial"

# -- El rendimiento de capital en el tiempo 1 es 0
Historico$R_Cuenta[1] <- 0

# -- Mensaje inicial
Historico$Mensaje[1] <- "Inicializacion de cartera"

# -- Calcular R_Precio
Historico$R_Precio <- round(c(0, diff(log(Historico$Precio))),4)

# calcular r _activo 
Postura_inicial <- Regla5_K%/% Historico$Precios[1]  # la diferencia entre  invertir todo el dinero desde el principio esperar a cuando se den señales de compre o venta

# -- Calcular R_Activo
for(i in 1:length(Historico$Date)){
  Historico$R_Activo[i] <-  (Postura_inicial*Historico$Precios[i])/(Postura_inicial*Historico$Precios[1])-1

}


# -- ------------------------------------ -- #
# -- ------------------------------------ -- #
# -- ------------------------------------ -- #

for(i in 2:length(Historico$Date)){
#for(i in 2:20){

  if(Historico$R_Precio[i] <= Regla0_R){  # si se cumple la regla y hay señal

      if(Historico$Capital[i-1] > 0){  # Si hay capital
          Historico$Capital[i]<- Historico$Capital[i-1]
        if(Historico$Capital[i]*Regla2_P > Historico$Precio[i]){ # Si Capital minimo
  
            Historico$Operacion[i] <- 1
            Historico$Titulos[i]   <- (Historico$Capital[i]*Regla2_P)%/%Historico$Precio[i]
            compra <- Historico$Precio[i]*Historico$Titulos[i]
            Historico$Comisiones[i] <- compra*Regla4_C
            Historico$Comisiones_a[i] <- Historico$Comisiones_a[i-1]+Historico$Comisiones[i]
            Historico$Titulos_a[i] <- Historico$Titulos_a[i-1]+Historico$Titulos[i]
            Historico$Mensaje[i] <- "Compra Exitosa"
            Historico$Flotante[i] <- Historico$Precios[i]* Historico$Titulos_a[i]
            Historico$Capital[i]<- Historico$Capital[i-1]- Historico$Titulos[i]*Historico$Precios[i]-Historico$Comisiones[i] #calcula el capital  despues del movimiento del dia i 
            Historico$Balance[i] <- Historico$Capital[i]+ Historico$Flotante[i]
            Historico$R_Cuenta[i]<- Historico$Balance[i]/Regla5_K -1
        
  
        } # cierre si hay capital
        else {
          # no alcanzó el capital 
          Historico$Mensaje[i] <- "Si hubo capital, pero no alcanzó "
          Historico$Operacion[i] <- 0
          Historico$Titulos[i] <- 0 
          Historico$Titulos_a [i] <- Historico$Titulos_a[i-1]
          Historico$Comisiones[i] <- 0
          Historico$Comisiones_a[i]<- Historico$Comisiones_a[i-1]
          Historico$Flotante[i] <- Historico$Titulos_a[i]*Historico$Precios[i]
          Historico$Capital[i]<- Historico$Capital[i-1]- Historico$Titulos[i]*Historico$Precios[i]-Historico$Comisiones[i] #calcula el capital  despues del movimiento del dia i 
          Historico$Balance[i] <- Historico$Capital[i]+ Historico$Flotante[i]
          Historico$R_Cuenta[i]<- Historico$Balance[i]/Regla5_K -1
          
          
        }} #cierre no alcanzó el capital
        
  
      
      else { # No hubo capital
         Historico$Mensaje[i] <- "No hubo Capital "
         Historico$Operacion[i] <- 0
         Historico$Titulos[i] <- 0 
         Historico$Titulos_a [i] <- Historico$Titulos_a[i-1]
         Historico$Flotante[i] <- Historico$Titulos_a[i]*Historico$Precios[i]
         Historico$Comisiones[i]<- 0
         Historico$Comisiones_a[i] <-  Historico$Comisiones_a[i-1]
         Historico$Flotante[i] <- Historico$Titulos_a[i]*Historico$Precios[i]
         Historico$Capital[i]<- Historico$Capital[i-1]- Historico$Titulos[i]*Historico$Precios[i]-Historico$Comisiones[i] #calcula el capital  despues del movimiento del dia i 
         Historico$Balance[i] <- Historico$Capital[i]+ Historico$Flotante[i]
         Historico$R_Cuenta[i]<- Historico$Balance[i]/Regla5_K -1
         
      }}
  
  
    
    else { # Sin señal

      Historico$Titulos[i] <- 0 # no hubo movimiento
      Historico$Titulos_a[i] <- Historico$Titulos[i]+Historico$Titulos_a[i-1]
      Historico$Operacion[i] <- 0  #mantiene
      Historico$Comisiones[i] <- 0
      Historico$Comisiones_a[i] <- Historico$Comisiones_a[i-1]+Historico$Comisiones[i]
      Historico$Mensaje[i] <- "No hubo señal de compra "
      Historico$Flotante[i] <- Historico$Titulos_a[i]*Historico$Precios[i]
      Historico$Capital[i]<- Historico$Capital[i-1]- Historico$Titulos[i]*Historico$Precios[i]-Historico$Comisiones[i] #calcula el capital  despues del movimiento del dia i 
      Historico$Balance[i] <- Historico$Capital[i]+ Historico$Flotante[i]
      Historico$R_Cuenta[i]<- Historico$Balance[i]/Regla5_K -1
      
      
      
    } #cierre no hubo señal
  
    
  
  
  
  

    } #cierre for

plot_ly(Historico) %>% # signo para agregar   las capas que se quieran 
  
  
  add_trace(x = ~Date, y = ~round(R_Activo,4), type = 'scatter', mode = 'lines', name = 'Activo',
            
            line = list(color = 'red')) %>%
  
  add_trace(x = ~Date, y = ~round(R_Cuenta,4), type = 'scatter', mode = 'lines', name = 'Cuenta',
            
            line = list(color = 'blue')) %>% 
  
  layout(title = "Rend del activo VS Rend de la cuenta",
         
         xaxis = list(title = "Fechas", showgrid = T),
         
         yaxis = list(title = "Rendimiento"), 
         
         legend = list(orientation = 'h', y = -0.25, x = 0.5))

###---- Metrico efectividad
sharpe_A <- SharpeRatio(R=xts(x = Historico$R_Activo,order.by = as.Date(Historico$Date)),Rf =0.0225, FUN ="StdDev" )
sharpe_C <- SharpeRatio(R=xts(x = Historico$R_Cuenta,order.by = as.Date(Historico$Date)),Rf =0.0225, FUN ="StdDev" )
  
sortino_a <- SortinoRatio(R=xts(x = Historico$R_Activo,order.by = as.Date(Historico$Date)),MAR =0.0225)
sortino_c <-SortinoRatio(R=xts(x = Historico$R_Cuenta,order.by = as.Date(Historico$Date)),MAR =0.0225)