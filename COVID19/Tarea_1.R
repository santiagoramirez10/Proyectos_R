datos<-read.csv("C:\\Users\\santi\\Desktop\\Tarea_1\\Salida_Datos_Abiertos.csv")
for (i in length(datos)){
  datos[i]<-c(datos[i])
}  

convertir <- function(columna) {
  j=1
  casos_unicos<-c()
  for (i in 1:length(datos[[columna]])){
    if ((datos[[columna]][[i]] %in% casos_unicos)==FALSE){
      casos_unicos[[j]]=datos[[columna]][[i]]
      j=j+1
    }
  }
  lista<-c()
  for (k in casos_unicos){
    pos=which(datos[[columna]]==k)
    for (l in pos){
      lista[l]=pos[1]
    }
  }
  resultados<-c()
  resultados[[1]]=lista
  resultados[[2]]=casos_unicos
  return(resultados)
}

#No numericos
fecha_hoy_casos=convertir(1)[[1]]
Fecha_Not=convertir(3)[[1]]
Departamento_nom=convertir(5)[[1]]
Ciudad_municipio_nom=convertir(7)[[1]]
Sexo=convertir(10)[[1]]
Fuente_tipo_contagio=convertir(11)[[1]]
Ubicacion=convertir(12)[[1]]
Estado=convertir(13)[[1]]
Pais_viajo_1_cod=convertir(14)[[1]]
Pais_viajo_1_nom=convertir(15)[[1]]
Recuperado=convertir(16)[[1]]
Fecha_inicio_sintomas=convertir(17)[[1]]
Fecha_muerte=convertir(18)[[1]]
Fecha_diagnostico=convertir(19)[[1]]
Fecha_recuperado=convertir(20)[[1]]
Tipo_recuperacion=convertir(21)[[1]]
nom_grupo=convertir(23)[[1]]

#Numericos:
Caso=datos[[2]]
Departamento=datos[[4]]
Ciudad_municipio=datos[[6]]
Edad=datos[[8]]
unidad_medida=datos[[9]]
per_etn_=datos[[22]]

histograma <- function(conjunto,tipo,clases){
  if (clases==""){
    clases=2246
  }
  else{
  }
  largo=length(conjunto)
  conjunto=sort(conjunto)
  ancho=ceiling((conjunto[[largo]]-conjunto[[1]])/clases)
  minimo=conjunto[[1]]-1
  maximo=conjunto[[largo]]+1
  limites<-c()
  a=1
  while (maximo>minimo){
    limites[[a]]=minimo+ancho
    a=a+1
    minimo=limites[[a-1]]
  }
  frecuencia<-c()
  b=1
  conteo=0
  for (i in 1:length(conjunto)){
    lim_sup=limites[[b]]
    if (lim_sup>=conjunto[[i]]){
      conteo=conteo+1
    }
    else{
      frecuencia<- c(frecuencia,conteo)
      b=b+1
      conteo=0
    }
  }
  frecuencia<- c(frecuencia,conteo)
  relativa<-c()
  densidad<-c()
  for (i in 1:length(frecuencia)){
    f=frecuencia[[i]]
    relativa<- c(relativa,f/length(conjunto))
    densidad<- c(densidad,f/(length(conjunto)*ancho))
  }
  if (tipo=="Frecuencia"){
    barplot(frecuencia)
  }
  else if (tipo=="Relativa"){
    barplot(relativa)
  }
  else if (tipo=="Densidad"){
    barplot(densidad)
  }
  else{
    print("Seleccione entre Frecuencia, Relativa o Densidad")
  }
}

puntob <- function(ciudades,edades){
  edad<-c()
  for (i in 1:length(ciudades)){
    if (ciudades[[i]]=="MEDELLIN"){
      edad<- c(edad,edades[[i]])
    }
  }
  return(edad)
}

edad=puntob(datos[[7]],datos[[8]])
clases=5
histograma(edad,"Densidad",clases)
hist(edad,freq=FALSE,breaks=clases)

puntoc <- function(ciudades,edades,estado){
  fallecidos<-c()
  for (i in 1:length(ciudades)){
    if (ciudades[[i]]=="MEDELLIN"&(estado[[i]]=="Fallecido")){
      fallecidos<- c(fallecidos,edades[[i]])
    }
  }
  return(fallecidos)
}

fallecido=puntoc(datos[[7]],datos[[8]],datos[[13]])
histograma(fallecido,"Densidad",clases)
hist(fallecido,freq=FALSE,breaks=clases)