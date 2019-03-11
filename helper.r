library("dplyr")

#Para temperatura maxima
meteo = read.csv("www/para_app.csv")
prueba <- mutate(meteo, Fecha = paste(Anio, Mes, 15, sep = '-'))
prueba = mutate(prueba, Fecha = as.Date(Fecha))
prueba$Ch_Mes <- factor(prueba$Mes) 
levels(prueba$Ch_Mes) <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DEC")
prueba$char = as.character(prueba$Ch_Mes)
cristina = group_by(prueba,Departamento ,Mes)
cristina = summarise(cristina, Txclim = mean(Tmaxp, na.rm = T), Ds = sd(Tmaxp, na.rm = T), Tnclim = mean(Tminp, na.rm = T), de = sd(Tminp, na.rm = T))
cristina = mutate(cristina, High = Txclim + Ds, Low = Txclim - Ds, alto = Tnclim + de , bajo = Tnclim - de)
cristina$Ch_Mes <- factor(cristina$Mes) 
levels(cristina$Ch_Mes) <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DEC")
cristina$char = as.character(cristina$Ch_Mes)
var_ko = group_by(prueba,Departamento)
var_fe = summarise(var_ko, 
                   Media = mean(Tmaxp, na.rm = T),
                   Max = max(Tmaxp, na.rm = T), 
                   Min = min(Tmaxp, na.rm = T),
                   Mediana = median(Tmaxp, na.rm = T),
                   Desv.est = sd(Tmaxp,na.rm = T), 
                   Q1 = quantile(Tmaxp,0.25, na.rm = T),
                   Q3 = quantile(Tmaxp, 0.75, na.rm = T),
                   numtotal = length(Tmaxp),
                   numNA = sum(is.na(Tmaxp)),
                   Porc_na = (numNA/numtotal)*100)
din = rep('Temperatura maxima',each = 5)
var_fe$Variable = paste0(din)
var_fe = select(var_fe, Variable,Departamento,Media, Max, Min, Mediana, Desv.est, Q1, Q3)

var_fi = summarise(var_ko, 
                   Media = mean(Tminp, na.rm = T),
                   Max = max(Tminp, na.rm = T), 
                   Min = min(Tminp, na.rm = T),
                   Mediana = median(Tminp, na.rm = T),
                   Desv.est = sd(Tminp,na.rm = T), 
                   Q1 = quantile(Tminp,0.25, na.rm = T),
                   Q3 = quantile(Tminp, 0.75, na.rm = T),
                   numtotal = length(Tminp),
                   numNA = sum(is.na(Tminp)),
                   Porc_na = (numNA/numtotal)*100)
dina = rep('Temperatura minima',each = 5)
var_fi$Variable = paste0(dina)
var_fi = select(var_fi, Variable,Departamento,Media, Max, Min, Mediana, Desv.est, Q1, Q3)

var_vi = summarise(var_ko, 
                   Media = mean(Precs, na.rm = T),
                   Max = max(Precs, na.rm = T), 
                   Min = min(Precs, na.rm = T),
                   Mediana = median(Precs, na.rm = T),
                   Desv.est = sd(Precs,na.rm = T), 
                   Q1 = quantile(Precs,0.25, na.rm = T),
                   Q3 = quantile(Precs, 0.75, na.rm = T),
                   numtotal = length(Precs),
                   numNA = sum(is.na(Precs)),
                   Porc_na = (numNA/numtotal)*100)
dinm = rep('Precipitacion',each = 5)
var_vi$Variable = paste0(dinm)
var_vi = select(var_vi,Variable,Departamento, Media, Max, Min, Mediana, Desv.est, Q1, Q3)
stn = c("Junin", "Huanuco", "Ancash", "Lima" , "Ucayali", "Tacna", "Moquegua","Arequipa","Puno","Madre de Dios","Cusco")
Graf = c("Temperatura", "Precipitacion")
adriana = read.csv("www/metadata.csv")
