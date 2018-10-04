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
Historico <- c()
sortino_a <- c()
sharpe_A <- c()
sharpe_C <- c()
sortino_c <- c()
for (aux in 1:length(tk_completos)){

  Historico[[aux]]<-data.frame("Date" = row.names(Precios),
                      "Precios" = Precios[,aux],
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
Historico[[aux]]$Titulos[1] <- (Regla5_K*Regla1_I)%/%Historico[[aux]]$Precio[1]
Historico[[aux]]$Titulos_a[1] <- Historico[[aux]]$Titulos[1]

# -- Se calculan comisiones iniciales
Historico[[aux]]$Comisiones[1] <- Historico[[aux]]$Titulos[1]*Historico[[aux]]$Precio[1]*Regla4_C
Historico[[aux]]$Comisiones_a[1] <-Historico[[aux]]$Comisiones[1]

# -- Calcular el Balance y flotante
#Historico[[aux]]$Balance[1] <-Historico[[aux]]$Titulos_a[1]*Historico[[aux]]$Precios[1] ya no por el cambio de flotante
Historico[[aux]]$Flotante[1] <- Historico[[aux]]$Precios[1]*Historico[[aux]]$Titulos


# -- Todo remanente se dejar? registrado en la cuenta de efectivo.
Historico[[aux]]$Capital[1] <- Regla5_K-Historico[[aux]]$Flotante[1]-Historico[[aux]]$Comisiones[1]
Historico[[aux]]$Balance[1] <- Historico[[aux]]$Flotante[1]+Historico[[aux]]$Capital[1]
# -- Iniciamos con una postura de mantener.
Historico[[aux]]$Operacion[1] <- "Posicion Inicial"

# -- El rendimiento de capital en el tiempo 1 es 0
Historico[[aux]]$R_Cuenta[1] <- 0

# -- Mensaje inicial
Historico[[aux]]$Mensaje[1] <- "Inicializacion de cartera"

# -- Calcular R_Precio
Historico[[aux]]$R_Precio <- round(c(0, diff(log(Historico[[aux]]$Precio))),4)

# calcular r _activo 
Postura_inicial <- Regla5_K%/% Historico[[aux]]$Precios[1]  # la diferencia entre  invertir todo el dinero desde el principio esperar a cuando se den señales de compre o venta

# -- Calcular R_Activo
for(i in 1:length(Historico[[aux]]$Date)){
  Historico[[aux]]$R_Activo[i] <-  (Postura_inicial*Historico[[aux]]$Precios[i])/(Postura_inicial*Historico[[aux]]$Precios[1])-1
  
}


# -- ------------------------------------ -- #
# -- ------------------------------------ -- #
# -- ------------------------------------ -- #

for(i in 2:length(Historico[[aux]]$Date)){
  #for(i in 2:20){
  
  if(Historico[[aux]]$R_Precio[i] <= Regla0_R){  # si se cumple la regla y hay señal
    
    if(Historico[[aux]]$Capital[i-1] > 0){  # Si hay capital
      Historico[[aux]]$Capital[i]<- Historico[[aux]]$Capital[i-1]
      if(Historico[[aux]]$Capital[i]*Regla2_P > Historico[[aux]]$Precio[i]){ # Si Capital minimo
        
        Historico[[aux]]$Operacion[i] <- 1
        Historico[[aux]]$Titulos[i]   <- (Historico[[aux]]$Capital[i]*Regla2_P)%/%Historico[[aux]]$Precio[i]
        compra <- Historico[[aux]]$Precio[i]*Historico[[aux]]$Titulos[i]
        Historico[[aux]]$Comisiones[i] <- compra*Regla4_C
        Historico[[aux]]$Comisiones_a[i] <- Historico[[aux]]$Comisiones_a[i-1]+Historico[[aux]]$Comisiones[i]
        Historico[[aux]]$Titulos_a[i] <- Historico[[aux]]$Titulos_a[i-1]+Historico[[aux]]$Titulos[i]
        Historico[[aux]]$Mensaje[i] <- "Compra Exitosa"
        Historico[[aux]]$Flotante[i] <- Historico[[aux]]$Precios[i]* Historico[[aux]]$Titulos_a[i]
        Historico[[aux]]$Capital[i]<- Historico[[aux]]$Capital[i-1]- Historico[[aux]]$Titulos[i]*Historico[[aux]]$Precios[i]-Historico[[aux]]$Comisiones[i] #calcula el capital  despues del movimiento del dia i 
        Historico[[aux]]$Balance[i] <- Historico[[aux]]$Capital[i]+ Historico[[aux]]$Flotante[i]
        Historico[[aux]]$R_Cuenta[i]<- Historico[[aux]]$Balance[i]/Regla5_K -1
        
        
      } # cierre si hay capital
      else {
        # no alcanzó el capital 
        Historico[[aux]]$Mensaje[i] <- "Si hubo capital, pero no alcanzó "
        Historico[[aux]]$Operacion[i] <- 0
        Historico[[aux]]$Titulos[i] <- 0 
        Historico[[aux]]$Titulos_a [i] <- Historico[[aux]]$Titulos_a[i-1]
        Historico[[aux]]$Comisiones[i] <- 0
        Historico[[aux]]$Comisiones_a[i]<- Historico[[aux]]$Comisiones_a[i-1]
        Historico[[aux]]$Flotante[i] <- Historico[[aux]]$Titulos_a[i]*Historico[[aux]]$Precios[i]
        Historico[[aux]]$Capital[i]<- Historico[[aux]]$Capital[i-1]- Historico[[aux]]$Titulos[i]*Historico[[aux]]$Precios[i]-Historico[[aux]]$Comisiones[i] #calcula el capital  despues del movimiento del dia i 
        Historico[[aux]]$Balance[i] <- Historico[[aux]]$Capital[i]+ Historico[[aux]]$Flotante[i]
        Historico[[aux]]$R_Cuenta[i]<- Historico[[aux]]$Balance[i]/Regla5_K -1
        
        
      }} #cierre no alcanzó el capital
    
    
    
    else { # No hubo capital
      Historico[[aux]]$Mensaje[i] <- "No hubo Capital "
      Historico[[aux]]$Operacion[i] <- 0
      Historico[[aux]]$Titulos[i] <- 0 
      Historico[[aux]]$Titulos_a [i] <- Historico[[aux]]$Titulos_a[i-1]
      Historico[[aux]]$Flotante[i] <- Historico[[aux]]$Titulos_a[i]*Historico[[aux]]$Precios[i]
      Historico[[aux]]$Comisiones[i]<- 0
      Historico[[aux]]$Comisiones_a[i] <-  Historico[[aux]]$Comisiones_a[i-1]
      Historico[[aux]]$Flotante[i] <- Historico[[aux]]$Titulos_a[i]*Historico[[aux]]$Precios[i]
      Historico[[aux]]$Capital[i]<- Historico[[aux]]$Capital[i-1]- Historico[[aux]]$Titulos[i]*Historico[[aux]]$Precios[i]-Historico[[aux]]$Comisiones[i] #calcula el capital  despues del movimiento del dia i 
      Historico[[aux]]$Balance[i] <- Historico[[aux]]$Capital[i]+ Historico[[aux]]$Flotante[i]
      Historico[[aux]]$R_Cuenta[i]<- Historico[[aux]]$Balance[i]/Regla5_K -1
      
    }}
  
  
  
  else { # Sin señal
    
    Historico[[aux]]$Titulos[i] <- 0 # no hubo movimiento
    Historico[[aux]]$Titulos_a[i] <- Historico[[aux]]$Titulos[i]+Historico[[aux]]$Titulos_a[i-1]
    Historico[[aux]]$Operacion[i] <- 0  #mantiene
    Historico[[aux]]$Comisiones[i] <- 0
    Historico[[aux]]$Comisiones_a[i] <- Historico[[aux]]$Comisiones_a[i-1]+Historico[[aux]]$Comisiones[i]
    Historico[[aux]]$Mensaje[i] <- "No hubo señal de compra "
    Historico[[aux]]$Flotante[i] <- Historico[[aux]]$Titulos_a[i]*Historico[[aux]]$Precios[i]
    Historico[[aux]]$Capital[i]<- Historico[[aux]]$Capital[i-1]- Historico[[aux]]$Titulos[i]*Historico[[aux]]$Precios[i]-Historico[[aux]]$Comisiones[i] #calcula el capital  despues del movimiento del dia i 
    Historico[[aux]]$Balance[i] <- Historico[[aux]]$Capital[i]+ Historico[[aux]]$Flotante[i]
    Historico[[aux]]$R_Cuenta[i]<- Historico[[aux]]$Balance[i]/Regla5_K -1
    
    
    
  } #cierre no hubo señal
  
  
  
  
  
  
  
} #cierre for
# 
# plot_ly(Historico[[aux]]) %>% # signo para agregar   las capas que se quieran 
#   
#   
#   add_trace(x = ~Date, y = ~round(R_Activo,4), type = 'scatter', mode = 'lines', name = 'Activo',
#             
#             line = list(color = 'red')) %>%
#   
#   add_trace(x = ~Date, y = ~round(R_Cuenta,4), type = 'scatter', mode = 'lines', name = 'Cuenta',
#             
#             line = list(color = 'blue')) %>% 
#   
#   layout(title = "Rend del activo VS Rend de la cuenta",
#          
#          xaxis = list(title = "Fechas", showgrid = T),
#          
#          yaxis = list(title = "Rendimiento"), 
#          
#          legend = list(orientation = 'h', y = -0.25, x = 0.5))

###---- Metrico efectividad
sharpe_A[[aux]] <- SharpeRatio(R=xts(x = Historico[[aux]]$R_Activo,order.by = as.Date(Historico[[aux]]$Date)),Rf =0.0225, FUN ="StdDev" )
sharpe_C[[aux]] <- SharpeRatio(R=xts(x = Historico[[aux]]$R_Cuenta,order.by = as.Date(Historico[[aux]]$Date)),Rf =0.0225, FUN ="StdDev" )

sortino_a[[aux]] <- SortinoRatio(R=xts(x = Historico[[aux]]$R_Activo,order.by = as.Date(Historico[[aux]]$Date)),MAR =0.0225)
sortino_c[[aux]] <-SortinoRatio(R=xts(x = Historico[[aux]]$R_Cuenta,order.by = as.Date(Historico[[aux]]$Date)),MAR =0.0225)
}

#Portafolio
