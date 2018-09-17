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

tk<-(read.xlsx(file="C:/Users/perezka/OneDrive - HP/Karla/Karla/Trading/myst_lab2_KarlaPerez/Etf_data.xlsx", sheetName="Holdings",
               colIndex=1,startRow=1,endRow=66,header=TRUE))
#tk<- read_excel("C:/Users/perezka/OneDrive - HP/Karla/Karla/Trading/myst_lab2_KarlaPerez/Etf_data.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

tkt_1<-as.character(tk[,1])


cs <- c("date", "adj_close")
Capital_Inicial <- 10000
fs = c("2015-09-01","2016-09-01") # por la c es vector de caracteres 

# Descargar Precios y Calcular rendimientos
Datos <- list()

for(i in 1:length(tkt_1))
  Datos[[i]] <- Bajar_Precios(Columns=cs, Ticker=tkt_1[i], Fecha_In=fs[1], Fecha_Fn=fs[2])

names(Datos) <- tkt_1
##-------------------
longitudes<-c()

for (i in 1:length(Datos)){
  longitudes[i]<-length(Datos[[i]]$date)
}

maximo<-max(longitudes)

completos<-which(longitudes==maximo)

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
                      "Balance" = 0,"Titulos" = 0 ,"Titulos_a" = 0,
                      "Operacion" = NA, "Comisiones" = 0, "Mensaje" = NA)

#Date fecha
#Precio del activo 
#R_Precio, rendimiento diario del precio(diario, de un dia a otro)
#R_activo, rendimiento acumulado del precio 
#Balance, valor del portafolio
#titulos  acciones que se tienen 
#titulos_a titulos acumulados
#R_cuenta, balance+ capital 
#operaciones, indica si son de compra o de venta(1 compra, 0 mantengo, -1 venta)
#Comisiones 0.0025  por el valor de la transacción
# Mensaje, Texto que indique la decisión o  que informe algo ocurrido

Regla0_R <- -0.03 # Considera una oportunidad de compra  cuando el activo empiece a bajar su rendimiento de -3% o menor 
Regla1_I <- 0.2 # Porcentaje de capital que se utiliza para la operación inicial 
Regla2_P <- 0.25 # Se utiliza  el =% del L capital restante en cada compra
Regla3_W <- tk_completos # se realiza la misma estrategia para todos los activos en el portafolio 
Regla4_C <- -0.0025 #comisiones pagadas
Regla5_K <- 1000000 #Capital inicial 


# -- ----------------------------------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #

# -- Calcular los Titulos de posicion inicial
Historico$Titulos[1] <- (Regla5_K*Regla1_I)%/%Historico$Precio[1]

# -- Se calculan comisiones iniciales
Historico$Comisiones[1] <- Historico$Titulos[1]*Historico$Precio[1]*Regla4_C

# -- Calcular el Balance
Historico$Balance[1] <- Historico$Titulos[1]*Historico$Precio[1]

# -- Todo remanente se dejar? registrado en la cuenta de efectivo.
Historico$Capital[1] <- Regla5_K-Historico$Balance[1]-Historico$Comisiones[1]

# -- Iniciamos con una postura de mantener.
Historico$Operacion[1] <- "Posicion Inicial"

# -- El rendimiento de capital en el tiempo 1 es 0
Historico$R_Cuenta[1] <- 0

# -- Mensaje inicial
Historico$Mensaje[1] <- "Inicializacion de cartera"

# -- Calcular R_Precio
Historico$R_Precio <- round(c(0, diff(log(Historico$Precio))),4)

# -- Calcular R_Activo
for(i in 1:length(Historico$Date)){
  Historico$R_Activo[i] <- round((Historico$Precio[i]/Historico$Precio[1])-1,2)
}

# -- ------------------------------------ -- #
# -- ------------------------------------ -- #
# -- ------------------------------------ -- #

for(i in 2:length(Historico$Date)){
  
  if(Historico$R_Precio[i] <= Regla0_R){ # Generar Se?al
    
    # Establecer capital actual, inicialmente, igual al capital anterior
    Historico$Capital[i] <- Historico$Capital[i-1]
    
    if(Historico$Capital[i] > 0){ # Si hay capital
      
      if(Historico$Capital[i]*Regla2_P > Historico$Precio[i]){ # Si Capital minimo
        
        Historico$Operacion[i] <- "Compra"
        Historico$Titulos[i]   <- (Historico$Capital[i]*Regla2_P)%/%Historico$Precio[i]
        
        compra <- Historico$Precio[i]*Historico$Titulos[i]  
        Historico$Comisiones[i] <- compra*Regla4_C
        
        Historico$Titulos_a[i] <- Historico$Titulos[i-1]+Historico$Titulos[i]
        
        
      }
      
    }
    else { # No hubo capital
      
      
    }
    
    
  }
  else { # Sin se?al
    
  }
  
}











# 
# for(i in 1:length(tk))
#   Datos[[i]]$adj_close_r <- c(0, diff(log(Datos[[i]]$adj_close)))
# 
# Rends <- xts(x = cbind(Datos[[1]]$adj_close_r, Datos[[2]]$adj_close_r, Datos[[3]]$adj_close_r),
#              order.by = Datos[[1]]$date)[-1]
# names(Rends) <- tk
# Port1 <- portfolio.spec(assets=tk)
# # Agregando restricciones
# 
# Port1 <- add.constraint(portfolio=Port1, type="full_investment")
# Port1 <- add.constraint(portfolio=Port1, type="box", min=c(0.01, 0.01, 0.01), max=c(0.7, 0.7, 0.7))
# Port1 <- add.objective(portfolio = Port1, type = "return", name = "mean")
# Port1 <- optimize.portfolio(R=Rends, portfolio = Port1, optimize_method = "r", trace = TRUE, search_size =5000)
# Portafolios <- vector("list", length = length(Port1$random_portfolio_objective_results))
# 
# for (i in 1:length(Port1$random_portfolio_objective_results)){
#   Portafolios[[i]]$Pesos <- Port1$random_portfolio_objective_results[[i]]$weights #para indexar listas se utliza [[]], ese elemento del portafolio se va creando desde el for
#   Portafolios[[i]]$Medias <- Port1$random_portfolio_objective_results[[i]]$objective_measures$mean
#   Portafolios[[i]]$Vars <- var.portfolio(R = Port1$R, weights = Portafolios[[i]]$Pesos)
#   names(Portafolios[[i]]$Medias) <- NULL
# }
# 
#  #creamos un dataframe para poder almacenar todo tipo de valores 
#  df_Portafolios <- data.frame(matrix(nrow=length(Port1$random_portfolio_objective_results),ncol=3, data=0))
#  colnames(df_Portafolios) <- c("Rend", "Var", "Clase")
#  for (i in 1:length(Port1$random_portfolio_objective_results)){
#    df_Portafolios$Rend[i] <- round(Portafolios[[i]]$Medias*252,4)
#    df_Portafolios$Var[i] <- round(sqrt(Portafolios[[i]]$Vars)*sqrt(252),4)
#    df_Portafolios$Clase[i] <- "No Frontera"
#    for(k in 1:length(tk)){
#      df_Portafolios[i,paste("Peso_",tk[k],sep="")] <- Portafolios[[i]]$Pesos[k]
#      df_Portafolios[i,paste("Titulos_ini_", tk[k], sep="")] <-
#        (Capital_Inicial*Portafolios[[i]]$Pesos[k])%/%Datos[[k]]$adj_close[1]
#    }
#  }
#  
#  # grafica en el eje x la varianza, y, rend
#  Plot_portafolios <- plot_ly(x=df_Portafolios$Var, y=df_Portafolios$Rend, type='scatter', mode='markers',
#                              name = "Portafolios", marker = list(color="grey", size=7), hoverinfo='text', 
#                              text = ~paste('Rendimiento: ', paste(df_Portafolios$Rend*100, "%") ,
#                                            '<br> Riesgo: ', paste(df_Portafolios$Var*100, "%") )) %>% 
#    layout(title = "Portafolios (Markowitz)",
#           xaxis = list(title = "Riesgo (Desviaci?n Est?ndar Anualizada)",
#                        showgrid = F),
#          yaxis = list(title = "Valor Esperado (Rendimiento Anualizado)"),
#           legend = list(orientation = 'h', y = -0.25))
# # Plot_portafolios # hasta aqui se grafica
#  
# # # Se seleccionan los mejores portafolios que cumplan  con los diferentes requerimientos < var, > rendm y  >ratio Sharpe
#  Port_1 <- df_Portafolios[which.max(df_Portafolios$Rend),]
#  # Portafolio con m?nima varianza
#  Port_2 <- df_Portafolios[which.min(df_Portafolios$Var),] # aunque dice var,son desviaciones
#  # Tasa libre de riesgo
#  rf <- 0.0253
#  # Rendimiento de portafolio
#  rp <- df_Portafolios$Rend
# # # Varianza de portafolio
#  sp <- df_Portafolios$Var
#  # Indice de sharpe
#  sharpe <- (rp-rf)/sp
#  # Portafolio con m?ximo Sharpe ratio 
#  Port_3 <- df_Portafolios[which.max(sharpe),]
#  Ports <- cbind(rbind(Port_1, Port_2, Port_3),
#                 "Portafolio" = c("Maximo Rendimiento","Minima Varianza","Maximo Sharpe Ratio"))
#  Plot_portafolios <- plot_ly(x=df_Portafolios$Var, y=df_Portafolios$Rend, type='scatter', mode='markers',
#                              name = "Portafolios", marker = list(color="grey", size=7), hoverinfo='text', 
#                              text = ~paste('Rendimiento: ', paste(df_Portafolios$Rend*100, "%") ,
#                                            '<br> Riesgo: ', paste(df_Portafolios$Var*100, "%") )) %>% 
#    layout(title = "Portafolios (Markowitz)",
#           xaxis = list(title = "Riesgo (Desviaci?n Est?ndar Anualizada)",
#                        showgrid = F),
#           yaxis = list(title = "Valor Esperado (Rendimiento Anualizado)"),
#           legend = list(orientation = 'h', y = -0.25)) %>%
#    add_trace(x = ~Ports$Var[1], y = ~Ports$Rend[1], name = Ports$Portafolio[1],
#              mode = 'marker', marker = list(color="green", size=10)) %>%
#    add_trace(x = ~Ports$Var[2], y = ~Ports$Rend[2], name = Ports$Portafolio[2],
#              mode = 'marker', marker = list(color="blue", size=10)) %>%
#    add_trace(x = ~Ports$Var[3], y = ~Ports$Rend[3], name = Ports$Portafolio[3],
#              mode = 'marker', marker = list(color="orange", size=10))
#  Plot_portafolios
#  # Pesos y titulos iniciales, de todos los activos, para los 3 portafolios
#  Pesos_Titulos <- Ports[,-c(1,2,3)]
# # # Encontrar las columnas cuyo nombre contenga "Titulos_ini", con esas encontraremos m?s f?cil los t?tulos
# # # por portafolio por activo
#  Ind <- grep(pattern = "Titulos_ini",x = colnames(Pesos_Titulos)) # la función de grep es buscar matches de argumentos 
#  Historicos_Ports <- data.frame("Date" = Datos[[1]]$date)
#  # Crear data frame que contendr? los datos finales de cada estrategia
#  for(i in 1:length(Ports[,1])) {
#    Historicos_Ports[[paste("Portafolio_",i,sep="")]] <- 
#      (Datos[[1]]$adj_close*Pesos_Titulos[i,Ind[1]]  + 
#         Datos[[2]]$adj_close*Pesos_Titulos[i,Ind[2]] +
#         Datos[[3]]$adj_close*Pesos_Titulos[i,Ind[3]])
#  }
#  
# # 
# # #####  Se utiliza esta parte más bien para series de tiempo
# # 
#  plot_ly(Historicos_Ports) %>%
#    add_trace(x = ~Date, y = ~round(Portafolio_1,2), type = 'scatter', mode = 'lines', name = 'Máximo Rendimiento',
#              line = list(color = 'red'), hoverinfo = "text", text = ~paste('Port_1',round(Portafolio_1,2))) %>%
#    add_trace(x = ~Date, y = ~round(Portafolio_2,2), type = 'scatter', mode = 'lines', name = 'Mínima Varianza',
#              line = list(color = 'blue'), hoverinfo = "text", text = ~paste('Port_2',round(Portafolio_2,2)))  %>%
#    add_trace(x = ~Date, y = ~round(Portafolio_3,2), type = 'scatter', mode = 'lines', name = 'Máximo Sharpe Ratio',
#              line = list(color = 'green'), hoverinfo = "text", text = ~paste('Port_3',round(Portafolio_3,2)))%>% 
#    layout(title = "3 Portafolios distintos objetivos",
#           xaxis = list(title = "Fechas", showgrid = T),
#           yaxis = list(title = "Balance"), 
          legend = list(orientation = 'h', y = -0.25, x = 0.5)) 