## ----setuppack----------------------
## Carga el paquete foreign, el cual auxiliará para cargar los datos en diferentes formatos
#(DBF, SPSS, STATA, SAS)
library(foreign)
## Carga el paquete survey. Esta librería sirve para el cálculo del diseño muestral
library(survey)
## Librería que permite hacer un ordenamiento de la tabla según el ingreso
library(doBy)
## Librería que incluye la función para el cálculo del GINI
library(reldist)
## Opción para tratar los casos de los estratos con una sola una UPM
options(survey.lonely.psu = "adjust")


## -----------------------------------
# Ingreso corriente promedio trimestral por hogar en deciles de
# hogares y su coeficiente de GINI
# 2022
## Limpia la pantalla de tablas o basura de un ejercicio anterior
#rm(list = ls())
## Carga librerías
library(foreign)
library(doBy)
library(reldist)
## Cuadro de sección 2 tabulados básicos ENIGH 2022
## Establece el directorio donde se encuentra la base de datos
# #setwd("D:/ENIGH_2022")
## Abre la tabla concentradohogar
Conc<- read.dbf("concentradohogar.dbf",as.is = T)
## Selecciona las variables de interés
Conc <- Conc [ c("folioviv", "foliohog", "ing_cor", "ingtrab", "trabajo", "negocio",
"otros_trab", "rentas", "utilidad", "arrenda", "transfer", "jubilacion",
"becas", "donativos", "remesas", "bene_gob", "transf_hog", "trans_inst",
"estim_alqu", "otros_ing", "factor", "upm", "est_dis")]

# Create a flag for numbering households
Conc$Nhog <- 1

## Se define la columna de los deciles
Numdec<-c("Total", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X")


## -----------------------------------
## Hogares

## Deciles de hogares
## Deja activa la tabla Conc
attach(Conc)
## Ordena Conc de acuerdo a ing_cor, folioviv, foliohog
Conc<- orderBy (~+ing_cor+folioviv+foliohog, data=Conc)

## Suma todos los factores y guarda el valor en el vector tot_hogares
tot_hogares <- sum(factor)

## Se divide la suma de factores entre diez para sacar el tamaño del decil
## (se debe de truncar el resultado quitando los decimales)
tam_dec<-trunc(tot_hogares/10)

## Muestra la suma del factor en variable hogar
Conc$tam_dec<-tam_dec

## Creación de deciles de hogares
## Se renombra la tabla concentrado a BD1
BD1 <- Conc

## Dentro de la tabla BD1 se crea la variable MAXT y se le asignan los
## valores que tiene el ing_cor.
BD1$MAXT <- BD1$ing_cor

## Se ordena de menor a mayor según la variable MAXT
BD1 <- BD1[with(BD1, order(rank(MAXT))),]

## Se aplica la función cumsum, suma acumulada a la variable factor
BD1$ACUMULA <- cumsum(BD1$factor)

## Entra a un ciclo donde genera los deciles 1 a 10
for(i in 1:9)
{
a1<-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1,]$factor
BD1<-rbind(BD1[1:(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),],
BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1):dim(BD1[1])[1],])
b1<-tam_dec*i-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1],]$ACUMULA
BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}


BD1$ACUMULA2<-cumsum(BD1$factor)
BD1$DECIL <- 0
BD1[(BD1$ACUMULA2<=tam_dec),]$DECIL <- 1


for(i in 1:9)
{
BD1[((BD1$ACUMULA2>tam_dec*i)&(BD1$ACUMULA2<=tam_dec*(i+1))),]$DECIL <- (i+1)
}
BD1[BD1$DECIL%in%"0",]$DECIL <- 10


## Total de hogares
x <- tapply(BD1$factor,BD1$Nhog,sum)

## Deciles
y <- tapply(BD1$factor,BD1$DECIL,sum)
## Se calcula el promedio de ingreso para el total y para cada uno de los deciles
ing_cormed_t <- tapply(BD1$factor*BD1$ing_cor,BD1$Nhog,sum)/x
ing_cormed_d <- tapply(BD1$factor*BD1$ing_cor,BD1$DECIL,sum)/y


## -----------------------------------
## Guarda los resultados en un data frame
prom_rub <- data.frame (c(ing_cormed_t,ing_cormed_d))
## Agrega el nombre a las filas
row.names(prom_rub) <- Numdec
## Cálculo del coeficiente de GINI
deciles_hog_ingcor <- data.frame(hogaresxdecil=c(x,x,x,x,x,x,x,x,x,x),
ingreso=c(ing_cormed_d[1],ing_cormed_d[2],
ing_cormed_d[3],ing_cormed_d[4],
ing_cormed_d[5],ing_cormed_d[6],
ing_cormed_d[7],ing_cormed_d[8],
ing_cormed_d[9],ing_cormed_d[10]))

## Se efectúa la función GINI y se guarda en el vector
a<-gini(deciles_hog_ingcor$ingreso,weights=deciles_hog_ingcor$hogares)
## Se renombran las variables (columnas)
names(prom_rub) <- c("INGRESO CORRIENTE")
names(a) <- "GINI"
## Muestra el resultado en pantalla
round(prom_rub) |> 
  gt::gt()
round(as.data.frame(a),3) |> 
  gt::gt()



## -----------------------------------
## Limpia la pantalla de tablas o basura de un ejercicio anterior
rm(list = ls())
## Establece el directorio donde se encuentra la base de datos
##setwd("D:/ENIGH_2022")
## Abre la tabla concentradohogar
Conc <- read.dbf("concentradohogar.dbf",as.is = T)
## Se seleccionan las variables de interés
Conc <- Conc[ c("folioviv", "foliohog", "ing_cor", "ingtrab", "trabajo",
"negocio", "otros_trab", "rentas", "utilidad", "arrenda",
"transfer", "jubilacion", "becas", "donativos", "remesas",
"bene_gob", "transf_hog", "trans_inst", "estim_alqu",
"otros_ing", "factor","upm","est_dis",
#missing
"ubica_geo")]
## Se crea una variable para agregar la entidad federativa
Conc$entidad <- substr(Conc$ubica_geo,1,2)

## Se define la columna con el nombre de las entidades federativas
Entidades <- c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California",
"Baja California Sur", "Campeche", "Coahuila de Zaragoza", "Colima",
"Chiapas", "Chihuahua", "Ciudad de México", "Durango", "Guanajuato",
"Guerrero", "Hidalgo", "Jalisco", "Estado de México", "Michoacán de Ocampo",
"Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro",
"Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco",
"Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave", "Yucatán",
"Zacatecas")
## Hogares
## Se crea una bandera para numerar los hogares
Conc$Nhog <- 1
## Se carga el diseño muestral
mydesign <- svydesign(id=~upm,strata=~est_dis,data=Conc,weights=~factor)


## -----------------------------------
## Ingreso corriente
Ming_corTot <- svyratio(~ing_cor,denominator=~Nhog,mydesign) # Total
Ming_corEnt <- svyby(~ing_cor,denominator=~Nhog,by=~entidad ,mydesign,svyratio) # Nacional
## Ingreso del trabajo
MingtrabTot <- svyratio(~ingtrab,denominator=~Nhog,mydesign)
MingtrabEnt <- svyby(~ingtrab,denominator=~Nhog,by=~entidad ,mydesign,svyratio)
## Ingreso del trabajo subordinado
MtrabajoTot <- svyratio(~trabajo,denominator=~Nhog,mydesign)
MtrabajoEnt <- svyby(~trabajo,denominator=~Nhog,by=~entidad ,mydesign,svyratio)
## Ingreso del trabajo independiente
MnegocioTot <- svyratio(~negocio,denominator=~Nhog,mydesign)
MnegocioEnt <- svyby(~negocio,denominator=~Nhog,by=~entidad ,mydesign,svyratio)

## Ingreso de otros trabajos
Motros_trabTot <- svyratio(~otros_trab,denominator=~Nhog,mydesign)
Motros_trabEnt <- svyby(~otros_trab,denominator=~Nhog,by=~entidad ,mydesign,svyratio)
## Renta de la propiedad
MrentasTot <- svyratio(~rentas,denominator=~Nhog,mydesign)
MrentasEnt <- svyby(~rentas,denominator=~Nhog,by=~entidad ,mydesign,svyratio)
## Ingresos de sociedades
MutilidadTot <- svyratio(~utilidad,denominator=~Nhog,mydesign)
MutilidadEnt <- svyby(~utilidad,denominator=~Nhog,by=~entidad ,mydesign,svyratio)
## Arrendamiento
MarrendaTot <- svyratio(~arrenda,denominator=~Nhog,mydesign)
MarrendaEnt <- svyby(~arrenda,denominator=~Nhog,by=~entidad ,mydesign,svyratio)
## Transferencias
MtransferTot <- svyratio(~transfer,denominator=~Nhog,mydesign)
MtransferEnt <- svyby(~transfer,denominator=~Nhog,by=~entidad ,mydesign,svyratio)
## Jubilación
MjubilacionTot <- svyratio(~jubilacion,denominator=~Nhog,mydesign)
MjubilacionEnt <- svyby(~jubilacion,denominator=~Nhog,by=~entidad ,mydesign,svyratio)
## Becas
MbecasTot <- svyratio(~becas,denominator=~Nhog,mydesign)
MbecasEnt <- svyby(~becas,denominator=~Nhog,by=~entidad ,mydesign,svyratio)
## Donativos
MdonativosTot <- svyratio(~donativos,denominator=~Nhog,mydesign)
MdonativosEnt <- svyby(~donativos,denominator=~Nhog,by=~entidad ,mydesign,svyratio)
## Remesas
MremesasTot <- svyratio(~remesas,denominator=~Nhog,mydesign)
MremesasEnt <- svyby(~remesas,denominator=~Nhog,by=~entidad ,mydesign,svyratio)
## Beneficios de gobierno
Mbene_gobTot <- svyratio(~bene_gob,denominator=~Nhog,mydesign)
Mbene_gobEnt <- svyby(~bene_gob,denominator=~Nhog,by=~entidad ,mydesign,svyratio)
## Transferencias de hogares
Mtransf_hogTot <- svyratio(~transf_hog,denominator=~Nhog,mydesign)
Mtransf_hogEnt <- svyby(~transf_hog,denominator=~Nhog,by=~entidad ,mydesign,svyratio)
## Transferencias de instituciones
Mtrans_instTot <- svyratio(~trans_inst,denominator=~Nhog,mydesign)
Mtrans_instEnt <- svyby(~trans_inst,denominator=~Nhog,by=~entidad ,mydesign,svyratio)
## Estimación del alquiler
Mestim_alquTot <- svyratio(~estim_alqu,denominator=~Nhog,mydesign)
Mestim_alquEnt <- svyby(~estim_alqu,denominator=~Nhog,by=~entidad ,mydesign,svyratio)
## Otros ingresos
Motros_ingTot <- svyratio(~otros_ing,denominator=~Nhog,mydesign)
Motros_ingEnt <- svyby(~otros_ing,denominator=~Nhog,by=~entidad ,mydesign,svyratio)


## -----------------------------------
## Estimaciones
ES_Ming_corTot <- Ming_corTot[[1]]
ES_Ming_corEnt <- Ming_corEnt[[2]]
ES_MingtrabTot <- MingtrabTot[[1]]
ES_MingtrabEnt <- MingtrabEnt[[2]]
ES_MtrabajoTot <- MtrabajoTot[[1]]
ES_MtrabajoEnt <- MtrabajoEnt[[2]]
ES_MnegocioTot <- MnegocioTot[[1]]
ES_MnegocioEnt <- MnegocioEnt[[2]]
ES_Motros_trabTot <- Motros_trabTot[[1]]
ES_Motros_trabEnt <- Motros_trabEnt[[2]]
ES_MrentasTot <- MrentasTot[[1]]
ES_MrentasEnt <- MrentasEnt[[2]]
ES_MutilidadTot <- MutilidadTot[[1]]
ES_MutilidadEnt <- MutilidadEnt[[2]]
ES_MarrendaTot <- MarrendaTot[[1]]
ES_MarrendaEnt <- MarrendaEnt[[2]]
ES_MtransferTot <- MtransferTot[[1]]
ES_MtransferEnt <- MtransferEnt[[2]]
ES_MjubilacionTot <- MjubilacionTot[[1]]
ES_MjubilacionEnt <- MjubilacionEnt[[2]]
ES_MbecasTot <- MbecasTot[[1]]
ES_MbecasEnt <- MbecasEnt[[2]]
ES_MdonativosTot <- MdonativosTot[[1]]
ES_MdonativosEnt <- MdonativosEnt[[2]]
ES_MremesasTot <- MremesasTot[[1]]
ES_MremesasEnt <- MremesasEnt[[2]]
ES_Mbene_gobTot <- Mbene_gobTot[[1]]
ES_Mbene_gobEnt <- Mbene_gobEnt[[2]]
ES_Mtransf_hogTot <- Mtransf_hogTot[[1]]
ES_Mtransf_hogEnt <- Mtransf_hogEnt[[2]]
ES_Mtrans_instTot <- Mtrans_instTot[[1]]
ES_Mtrans_instEnt <- Mtrans_instEnt[[2]]
ES_Mestim_alquTot <- Mestim_alquTot[[1]]
ES_Mestim_alquEnt <- Mestim_alquEnt[[2]]
ES_Motros_ingTot <- Motros_ingTot[[1]]
ES_Motros_ingEnt <- Motros_ingEnt[[2]]


## -----------------------------------
## Error estándar
SE_Ming_corTot <- SE(Ming_corTot)
SE_Ming_corEnt <- SE(Ming_corEnt)
SE_MingtrabTot <- SE(MingtrabTot)
SE_MingtrabEnt <- SE(MingtrabEnt)
SE_MtrabajoTot <- SE(MtrabajoTot)
SE_MtrabajoEnt <- SE(MtrabajoEnt)
SE_MnegocioTot <- SE(MnegocioTot)
SE_MnegocioEnt <- SE(MnegocioEnt)
SE_Motros_trabTot <- SE(Motros_trabTot)

SE_Motros_trabEnt <- SE(Motros_trabEnt)
SE_MrentasTot <- SE(MrentasTot)
SE_MrentasEnt <- SE(MrentasEnt)
SE_MutilidadTot <- SE(MutilidadTot)
SE_MutilidadEnt <- SE(MutilidadEnt)
SE_MarrendaTot <- SE(MarrendaTot)
SE_MarrendaEnt <- SE(MarrendaEnt)
SE_MtransferTot <- SE(MtransferTot)
SE_MtransferEnt <- SE(MtransferEnt)
SE_MjubilacionTot <- SE(MjubilacionTot)
SE_MjubilacionEnt <- SE(MjubilacionEnt)
SE_MbecasTot <- SE(MbecasTot)
SE_MbecasEnt <- SE(MbecasEnt)
SE_MdonativosTot <- SE(MdonativosTot)
SE_MdonativosEnt <- SE(MdonativosEnt)
SE_MremesasTot <- SE(MremesasTot)
SE_MremesasEnt <- SE(MremesasEnt)
SE_Mbene_gobTot <- SE(Mbene_gobTot)
SE_Mbene_gobEnt <- SE(Mbene_gobEnt)
SE_Mtransf_hogTot <- SE(Mtransf_hogTot)
SE_Mtransf_hogEnt <- SE(Mtransf_hogEnt)
SE_Mtrans_instTot <- SE(Mtrans_instTot)
SE_Mtrans_instEnt <- SE(Mtrans_instEnt)
SE_Mestim_alquTot <- SE(Mestim_alquTot)
SE_Mestim_alquEnt <- SE(Mestim_alquEnt)
SE_Motros_ingTot <- SE(Motros_ingTot)
SE_Motros_ingEnt <- SE(Motros_ingEnt)



## -----------------------------------

## Coeficiente de variación
CV_Ming_corTot <- cv(Ming_corTot)
CV_Ming_corEnt <- cv(Ming_corEnt)
CV_MingtrabTot <- cv(MingtrabTot)
CV_MingtrabEnt <- cv(MingtrabEnt)
CV_MtrabajoTot <- cv(MtrabajoTot)
CV_MtrabajoEnt <- cv(MtrabajoEnt)
CV_MnegocioTot <- cv(MnegocioTot)
CV_MnegocioEnt <- cv(MnegocioEnt)
CV_Motros_trabTot <- cv(Motros_trabTot)
CV_Motros_trabEnt <- cv(Motros_trabEnt)
CV_MrentasTot <- cv(MrentasTot)
CV_MrentasEnt <- cv(MrentasEnt)
CV_MutilidadTot <- cv(MutilidadTot)
CV_MutilidadEnt <- cv(MutilidadEnt)
CV_MarrendaTot <- cv(MarrendaTot)
CV_MarrendaEnt <- cv(MarrendaEnt)
CV_MtransferTot <- cv(MtransferTot)
CV_MtransferEnt <- cv(MtransferEnt)
CV_MjubilacionTot <- cv(MjubilacionTot)
CV_MjubilacionEnt <- cv(MjubilacionEnt)
CV_MbecasTot <- cv(MbecasTot)
CV_MbecasEnt <- cv(MbecasEnt)
CV_MdonativosTot <- cv(MdonativosTot)

CV_MdonativosEnt <- cv(MdonativosEnt)
CV_MremesasTot <- cv(MremesasTot)
CV_MremesasEnt <- cv(MremesasEnt)
CV_Mbene_gobTot <- cv(Mbene_gobTot)
CV_Mbene_gobEnt <- cv(Mbene_gobEnt)
CV_Mtransf_hogTot <- cv(Mtransf_hogTot)
CV_Mtransf_hogEnt <- cv(Mtransf_hogEnt)
CV_Mtrans_instTot <- cv(Mtrans_instTot)
CV_Mtrans_instEnt <- cv(Mtrans_instEnt)
CV_Mestim_alquTot <- cv(Mestim_alquTot)
CV_Mestim_alquEnt <- cv(Mestim_alquEnt)
CV_Motros_ingTot <- cv(Motros_ingTot)
CV_Motros_ingEnt <- cv(Motros_ingEnt)


## -----------------------------------
## Límite inferior
LI_Ming_corTot <- confint(Ming_corTot,level=0.90)[,1]
LI_Ming_corEnt <- confint(Ming_corEnt,level=0.90)[,1]
LI_MingtrabTot <- confint(MingtrabTot,level=0.90)[,1]
LI_MingtrabEnt <- confint(MingtrabEnt,level=0.90)[,1]
LI_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,1]
LI_MtrabajoEnt <- confint(MtrabajoEnt,level=0.90)[,1]
LI_MnegocioTot <- confint(MnegocioTot,level=0.90)[,1]
LI_MnegocioEnt <- confint(MnegocioEnt,level=0.90)[,1]
LI_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,1]
LI_Motros_trabEnt <- confint(Motros_trabEnt,level=0.90)[,1]
LI_MrentasTot <- confint(MrentasTot,level=0.90)[,1]
LI_MrentasEnt <- confint(MrentasEnt,level=0.90)[,1]
LI_MutilidadTot <- confint(MutilidadTot,level=0.90)[,1]
LI_MutilidadEnt <- confint(MutilidadEnt,level=0.90)[,1]
LI_MarrendaTot <- confint(MarrendaTot,level=0.90)[,1]
LI_MarrendaEnt <- confint(MarrendaEnt,level=0.90)[,1]
LI_MtransferTot <- confint(MtransferTot,level=0.90)[,1]
LI_MtransferEnt <- confint(MtransferEnt,level=0.90)[,1]
LI_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,1]
LI_MjubilacionEnt <- confint(MjubilacionEnt,level=0.90)[,1]
LI_MbecasTot <- confint(MbecasTot,level=0.90)[,1]
LI_MbecasEnt <- confint(MbecasEnt,level=0.90)[,1]
LI_MdonativosTot <- confint(MdonativosTot,level=0.90)[,1]
LI_MdonativosEnt <- confint(MdonativosEnt,level=0.90)[,1]
LI_MremesasTot <- confint(MremesasTot,level=0.90)[,1]
LI_MremesasEnt <- confint(MremesasEnt,level=0.90)[,1]
LI_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,1]
LI_Mbene_gobEnt <- confint(Mbene_gobEnt,level=0.90)[,1]
LI_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,1]
LI_Mtransf_hogEnt <- confint(Mtransf_hogEnt,level=0.90)[,1]
LI_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,1]
LI_Mtrans_instEnt <- confint(Mtrans_instEnt,level=0.90)[,1]
LI_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,1]
LI_Mestim_alquEnt <- confint(Mestim_alquEnt,level=0.90)[,1]
LI_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,1]
LI_Motros_ingEnt <- confint(Motros_ingEnt,level=0.90)[,1]



## -----------------------------------

## Límite superior
LS_Ming_corTot <- confint(Ming_corTot,level=0.90)[,2]
LS_Ming_corEnt <- confint(Ming_corEnt,level=0.90)[,2]
LS_MingtrabTot <- confint(MingtrabTot,level=0.90)[,2]
LS_MingtrabEnt <- confint(MingtrabEnt,level=0.90)[,2]
LS_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,2]
LS_MtrabajoEnt <- confint(MtrabajoEnt,level=0.90)[,2]
LS_MnegocioTot <- confint(MnegocioTot,level=0.90)[,2]
LS_MnegocioEnt <- confint(MnegocioEnt,level=0.90)[,2]
LS_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,2]
LS_Motros_trabEnt <- confint(Motros_trabEnt,level=0.90)[,2]
LS_MrentasTot <- confint(MrentasTot,level=0.90)[,2]
LS_MrentasEnt <- confint(MrentasEnt,level=0.90)[,2]
LS_MutilidadTot <- confint(MutilidadTot,level=0.90)[,2]
LS_MutilidadEnt <- confint(MutilidadEnt,level=0.90)[,2]
LS_MarrendaTot <- confint(MarrendaTot,level=0.90)[,2]
LS_MarrendaEnt <- confint(MarrendaEnt,level=0.90)[,2]
LS_MtransferTot <- confint(MtransferTot,level=0.90)[,2]
LS_MtransferEnt <- confint(MtransferEnt,level=0.90)[,2]
LS_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,2]
LS_MjubilacionEnt <- confint(MjubilacionEnt,level=0.90)[,2]
LS_MbecasTot <- confint(MbecasTot,level=0.90)[,2]
LS_MbecasEnt <- confint(MbecasEnt,level=0.90)[,2]
LS_MdonativosTot <- confint(MdonativosTot,level=0.90)[,2]
LS_MdonativosEnt <- confint(MdonativosEnt,level=0.90)[,2]
LS_MremesasTot <- confint(MremesasTot,level=0.90)[,2]
LS_MremesasEnt <- confint(MremesasEnt,level=0.90)[,2]
LS_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,2]
LS_Mbene_gobEnt <- confint(Mbene_gobEnt,level=0.90)[,2]
LS_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,2]
LS_Mtransf_hogEnt <- confint(Mtransf_hogEnt,level=0.90)[,2]
LS_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,2]
LS_Mtrans_instEnt <- confint(Mtrans_instEnt,level=0.90)[,2]
LS_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,2]
LS_Mestim_alquEnt <- confint(Mestim_alquEnt,level=0.90)[,2]
LS_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,2]
LS_Motros_ingEnt <- confint(Motros_ingEnt,level=0.90)[,2]


## -----------------------------------
## Estimación
c_ent_ES <- data.frame(
c(ES_Ming_corTot,ES_Ming_corEnt),c(ES_MingtrabTot,ES_MingtrabEnt),
c(ES_MtrabajoTot,ES_MtrabajoEnt),c(ES_MnegocioTot,ES_MnegocioEnt),
c(ES_Motros_trabTot,ES_Motros_trabEnt),c(ES_MrentasTot,ES_MrentasEnt),
c(ES_MutilidadTot,ES_MutilidadEnt),c(ES_MarrendaTot,ES_MarrendaEnt),
c(ES_MtransferTot,ES_MtransferEnt),c(ES_MjubilacionTot,ES_MjubilacionEnt),
c(ES_MbecasTot,ES_MbecasEnt),c(ES_MdonativosTot,ES_MdonativosEnt),
c(ES_MremesasTot,ES_MremesasEnt),c(ES_Mbene_gobTot,ES_Mbene_gobEnt),
c(ES_Mtransf_hogTot,ES_Mtransf_hogEnt),c(ES_Mtrans_instTot,ES_Mtrans_instEnt),
c(ES_Mestim_alquTot,ES_Mestim_alquEnt),c(ES_Motros_ingTot,ES_Motros_ingEnt))

## Error estándar
c_ent_SE <- data.frame(
c(SE_Ming_corTot,SE_Ming_corEnt),c(SE_MingtrabTot,SE_MingtrabEnt),
c(SE_MtrabajoTot,SE_MtrabajoEnt),c(SE_MnegocioTot,SE_MnegocioEnt),
c(SE_Motros_trabTot,SE_Motros_trabEnt),c(SE_MrentasTot,SE_MrentasEnt),
c(SE_MutilidadTot,SE_MutilidadEnt),c(SE_MarrendaTot,SE_MarrendaEnt),
c(SE_MtransferTot,SE_MtransferEnt),c(SE_MjubilacionTot,SE_MjubilacionEnt),
c(SE_MbecasTot,SE_MbecasEnt),c(SE_MdonativosTot,SE_MdonativosEnt),
c(SE_MremesasTot,SE_MremesasEnt),c(SE_Mbene_gobTot,SE_Mbene_gobEnt),
c(SE_Mtransf_hogTot,SE_Mtransf_hogEnt),c(SE_Mtrans_instTot,SE_Mtrans_instEnt),
c(SE_Mestim_alquTot,SE_Mestim_alquEnt),c(SE_Motros_ingTot,SE_Motros_ingEnt))


## Coeficiente de variación
c_ent_CV <- data.frame(
c(CV_Ming_corTot,CV_Ming_corEnt),c(CV_MingtrabTot,CV_MingtrabEnt),
c(CV_MtrabajoTot,CV_MtrabajoEnt),c(CV_MnegocioTot,CV_MnegocioEnt),
c(CV_Motros_trabTot,CV_Motros_trabEnt),c(CV_MrentasTot,CV_MrentasEnt),
c(CV_MutilidadTot,CV_MutilidadEnt),c(CV_MarrendaTot,CV_MarrendaEnt),
c(CV_MtransferTot,CV_MtransferEnt),c(CV_MjubilacionTot,CV_MjubilacionEnt),
c(CV_MbecasTot,CV_MbecasEnt),c(CV_MdonativosTot,CV_MdonativosEnt),
c(CV_MremesasTot,CV_MremesasEnt),c(CV_Mbene_gobTot,CV_Mbene_gobEnt),
c(CV_Mtransf_hogTot,CV_Mtransf_hogEnt),c(CV_Mtrans_instTot,CV_Mtrans_instEnt),
c(CV_Mestim_alquTot,CV_Mestim_alquEnt),c(CV_Motros_ingTot,CV_Motros_ingEnt))


## Límite inferior
c_ent_LI <- data.frame(
c(LI_Ming_corTot,LI_Ming_corEnt),c(LI_MingtrabTot,LI_MingtrabEnt),
c(LI_MtrabajoTot,LI_MtrabajoEnt),c(LI_MnegocioTot,LI_MnegocioEnt),
c(LI_Motros_trabTot,LI_Motros_trabEnt),c(LI_MrentasTot,LI_MrentasEnt),
c(LI_MutilidadTot,LI_MutilidadEnt),c(LI_MarrendaTot,LI_MarrendaEnt),
c(LI_MtransferTot,LI_MtransferEnt),c(LI_MjubilacionTot,LI_MjubilacionEnt),
c(LI_MbecasTot,LI_MbecasEnt),c(LI_MdonativosTot,LI_MdonativosEnt),
c(LI_MremesasTot,LI_MremesasEnt),c(LI_Mbene_gobTot,LI_Mbene_gobEnt),
c(LI_Mtransf_hogTot,LI_Mtransf_hogEnt),c(LI_Mtrans_instTot,LI_Mtrans_instEnt),
c(LI_Mestim_alquTot,LI_Mestim_alquEnt),c(LI_Motros_ingTot,LI_Motros_ingEnt))


## Límite superior
c_ent_LS <- data.frame(
c(LS_Ming_corTot,LS_Ming_corEnt),c(LS_MingtrabTot,LS_MingtrabEnt),
c(LS_MtrabajoTot,LS_MtrabajoEnt),c(LS_MnegocioTot,LS_MnegocioEnt),
c(LS_Motros_trabTot,LS_Motros_trabEnt),c(LS_MrentasTot,LS_MrentasEnt),
c(LS_MutilidadTot,LS_MutilidadEnt),c(LS_MarrendaTot,LS_MarrendaEnt),
c(LS_MtransferTot,LS_MtransferEnt),c(LS_MjubilacionTot,LS_MjubilacionEnt),
c(LS_MbecasTot,LS_MbecasEnt),c(LS_MdonativosTot,LS_MdonativosEnt),
c(LS_MremesasTot,LS_MremesasEnt),c(LS_Mbene_gobTot,LS_Mbene_gobEnt),
c(LS_Mtransf_hogTot,LS_Mtransf_hogEnt),c(LS_Mtrans_instTot,LS_Mtrans_instEnt),
c(LS_Mestim_alquTot,LS_Mestim_alquEnt),c(LS_Motros_ingTot,LS_Motros_ingEnt))


## Se agregan los nombres de las entidades a las filas
row.names(c_ent_ES) <- row.names(c_ent_SE) <-
row.names(c_ent_CV) <- row.names(c_ent_LI) <-
row.names(c_ent_LI) <- row.names(c_ent_LS) <- Entidades


## -----------------------------------

## Se renombran las variables
names(c_ent_ES) <- c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB",
"RENTAS", "UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS",
"DONATIVOS", "REMESAS", "BENE GOBIERNO", "TRANS HOG", "TRANS INST",
"ESTIM ALQU", "OTROS INGRESOS")
names(c_ent_SE) = c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB",
"RENTAS", "UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS",
"DONATIVOS","REMESAS", "BENE GOBIERNO", "TRANS HOG", "TRANS INST",
"ESTIM ALQU", "OTROS INGRESOS")
names(c_ent_CV) = c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB",
"RENTAS", "UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS",
"DONATIVOS", "REMESAS", "BENE GOBIERNO", "TRANS HOG", "TRANS INST",
"ESTIM ALQU", "OTROS INGRESOS")
names(c_ent_LI) = c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB",
"RENTAS", "UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS",
"DONATIVOS", "REMESAS", "BENE GOBIERNO", "TRANS HOG", "TRANS INST",
"ESTIM ALQU", "OTROS INGRESOS")
names(c_ent_LS) = c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB",
"RENTAS", "UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS",
"DONATIVOS", "REMESAS", "BENE GOBIERNO", "TRANS HOG", "TRANS INST",
"ESTIM ALQU", "OTROS INGRESOS")
## El comando round, en el caso del coeficiente de variación, redondea a 4 decimales.
## Mostramos el resultado en pantalla
round(c_ent_ES) |> 
  gt::gt()
round(c_ent_SE) |> 
  gt::gt()
(round(c_ent_CV,4)*100) |> 
  gt::gt()
round(c_ent_LI) |> 
  gt::gt()
round(c_ent_LS) |> 
  gt::gt()


## -----------------------------------
# Gasto corriente monetario promedio trimestral por grandes rubros según
# entidad federativa
# 2022
## Carga librerías
library(foreign)
library(survey)
## Limpia la pantalla de tablas o basura de un ejercicio anterior
rm(list = ls())
## Establece el directorio donde se encuentra la base de datos
#setwd("D:/ENIGH_2022")
## Abre la tabla concentradohogar
Conc<- read.dbf("concentradohogar.dbf",as.is = T)
## Selecciona las variables de interés
Conc <- Conc [ c("folioviv", "foliohog", "tot_integ","gasto_mon",
"alimentos", "vesti_calz", "vivienda", "limpieza",
"salud", "transporte", "educa_espa", "personales",
"transf_gas","factor","upm","est_dis",
#missing
"ubica_geo")]
## Se crea una variable para agregar la entidad federativa
Conc$entidad <-substr(Conc$ubica_geo,1,2)
## Se define la columna con el nombre de las entidades federativas
Entidades <- c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California",
"Baja California Sur", "Campeche", "Coahuila de Zaragoza", "Colima",
"Chiapas", "Chihuahua", "Ciudad de México", "Durango", "Guanajuato",
"Guerrero", "Hidalgo", "Jalisco", "Estado de México", "Michoacán de Ocampo",
"Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro",
"Quintana Roo", "San Luis Potosí", "Sinaloa","Sonora", "Tabasco",
"Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave", "Yucatán",
"Zacatecas")
## Hogares
## Se crea una bandera para numerar los hogares
Conc$Nhog <- 1

## Se carga el diseño muestral
mydesign <- svydesign(id=~upm,strata=~est_dis,data=Conc,weights=~factor)


## -----------------------------------
## Gasto corriente monetario
M_gasto_monTot <- svyratio(~gasto_mon,denominator=~Nhog,mydesign)
M_gasto_monEnt <- svyby(~gasto_mon,denominator=~Nhog,by=~entidad,mydesign,svyratio)
## Gasto en alimentos
M_alimentosTot <- svyratio(~alimentos,denominator=~Nhog,mydesign)
M_alimentosEnt <- svyby(~alimentos,denominator=~Nhog,by=~entidad,mydesign,svyratio)
## Gasto en vestido y calzado
M_vesti_calzTot <- svyratio(~vesti_calz,denominator=~Nhog,mydesign)
M_vesti_calzEnt <- svyby(~vesti_calz,denominator=~Nhog,by=~entidad,mydesign,svyratio)
## Gasto en vivienda y servicios de conservación
M_viviendaTot <- svyratio(~vivienda,denominator=~Nhog,mydesign)
M_viviendaEnt<-svyby(~vivienda,denominator=~Nhog,by=~entidad,mydesign,svyratio)
## Gasto en artículos y servicios para la limpieza
M_limpiezaTot <- svyratio(~limpieza,denominator=~Nhog,mydesign)
M_limpiezaEnt<-svyby(~limpieza,denominator=~Nhog,by=~entidad,mydesign,svyratio)
## Gasto en cuidados de la salud
M_saludTot <- svyratio(~salud,denominator=~Nhog,mydesign)
M_saludEnt <- svyby(~salud,denominator=~Nhog,by=~entidad,mydesign,svyratio)
## Gasto en transporte
M_transporteTot <- svyratio(~transporte,denominator=~Nhog,mydesign)
M_transporteEnt <- svyby(~transporte,denominator=~Nhog,by=~entidad,mydesign,svyratio)
## Gasto en servicios de educación
M_educa_espaTot <- svyratio(~educa_espa,denominator=~Nhog,mydesign)
M_educa_espaEnt<-svyby(~educa_espa,denominator=~Nhog,by=~entidad,mydesign,svyratio)
## Gasto en cuidados personales
M_personalesTot <- svyratio(~personales,denominator=~Nhog,mydesign)
M_personalesEnt <- svyby(~personales,denominator=~Nhog,by=~entidad,mydesign,svyratio)
## Transferencias de gasto
M_transf_gasTot <- svyratio(~transf_gas,denominator=~Nhog,mydesign)
M_transf_gasEnt <- svyby(~transf_gas,denominator=~Nhog,by=~entidad,mydesign,svyratio)


## -----------------------------------
ES_M_gasto_monTot <- M_gasto_monTot[[1]]
ES_M_gasto_monEnt <- M_gasto_monEnt[[2]]
ES_M_alimentosTot <- M_alimentosTot[[1]]
ES_M_alimentosEnt <- M_alimentosEnt[[2]]
ES_M_vesti_calzTot <- M_vesti_calzTot[[1]]
ES_M_vesti_calzEnt <- M_vesti_calzEnt[[2]]
ES_M_viviendaTot <- M_viviendaTot[[1]]
ES_M_viviendaEnt <- M_viviendaEnt[[2]]
ES_M_limpiezaTot <- M_limpiezaTot[[1]]
ES_M_limpiezaEnt <- M_limpiezaEnt[[2]]
ES_M_saludTot <- M_saludTot[[1]]
ES_M_saludEnt <- M_saludEnt[[2]]
ES_M_transporteTot <- M_transporteTot[[1]]
ES_M_transporteEnt <- M_transporteEnt[[2]]
ES_M_educa_espaTot <- M_educa_espaTot[[1]]
ES_M_educa_espaEnt <- M_educa_espaEnt[[2]]
ES_M_personalesTot <- M_personalesTot[[1]]
ES_M_personalesEnt <- M_personalesEnt[[2]]
ES_M_transf_gasTot <- M_transf_gasTot[[1]]
ES_M_transf_gasEnt <- M_transf_gasEnt[[2]]


## -----------------------------------
SE_M_gasto_monTot <- SE(M_gasto_monTot)
SE_M_gasto_monEnt <- SE(M_gasto_monEnt)
SE_M_alimentosTot <- SE(M_alimentosTot)
SE_M_alimentosEnt <- SE(M_alimentosEnt)
SE_M_vesti_calzTot <- SE(M_vesti_calzTot)
SE_M_vesti_calzEnt <- SE(M_vesti_calzEnt)
SE_M_viviendaTot <- SE(M_viviendaTot)
SE_M_viviendaEnt <- SE(M_viviendaEnt)
SE_M_limpiezaTot <- SE(M_limpiezaTot)
SE_M_limpiezaEnt <- SE(M_limpiezaEnt)
SE_M_saludTot <- SE(M_saludTot)
SE_M_saludEnt <- SE(M_saludEnt)
SE_M_transporteTot <- SE(M_transporteTot)
SE_M_transporteEnt <- SE(M_transporteEnt)
SE_M_educa_espaTot <- SE(M_educa_espaTot)
SE_M_educa_espaEnt <- SE(M_educa_espaEnt)
SE_M_personalesTot <- SE(M_personalesTot)
SE_M_personalesEnt <- SE(M_personalesEnt)
SE_M_transf_gasTot <- SE(M_transf_gasTot)
SE_M_transf_gasEnt <- SE(M_transf_gasEnt)


## -----------------------------------
#
CV_M_gasto_monTot <- cv( M_gasto_monTot)
CV_M_gasto_monEnt <- cv( M_gasto_monEnt)
CV_M_alimentosTot <- cv(M_alimentosTot)
CV_M_alimentosEnt <- cv(M_alimentosEnt)
CV_M_vesti_calzTot <- cv( M_vesti_calzTot)
CV_M_vesti_calzEnt <- cv( M_vesti_calzEnt)
CV_M_viviendaTot <- cv(M_viviendaTot)
CV_M_viviendaEnt <- cv(M_viviendaEnt)
CV_M_limpiezaTot <- cv(M_limpiezaTot)
CV_M_limpiezaEnt <- cv(M_limpiezaEnt)
CV_M_saludTot <- cv(M_saludTot)
CV_M_saludEnt <- cv(M_saludEnt)
CV_M_transporteTot <- cv(M_transporteTot)
CV_M_transporteEnt <- cv(M_transporteEnt)

CV_M_educa_espaTot <- cv(M_educa_espaTot)
CV_M_educa_espaEnt <- cv(M_educa_espaEnt)
CV_M_personalesTot <- cv(M_personalesTot)
CV_M_personalesEnt <- cv(M_personalesEnt)
CV_M_transf_gasTot <- cv(M_transf_gasTot)
CV_M_transf_gasEnt <- cv(M_transf_gasEnt)



## -----------------------------------

LI_M_gasto_monTot <- confint(M_gasto_monTot,level=0.90)[,1]
LI_M_gasto_monEnt <- confint(M_gasto_monEnt,level=0.90)[,1]
LI_M_alimentosTot <- confint(M_alimentosTot,level=0.90)[,1]
LI_M_alimentosEnt <- confint(M_alimentosEnt,level=0.90)[,1]
LI_M_vesti_calzTot <- confint(M_vesti_calzTot,level=0.90)[,1]
LI_M_vesti_calzEnt <- confint(M_vesti_calzEnt,level=0.90)[,1]
LI_M_viviendaTot <- confint(M_viviendaTot,level=0.90)[,1]
LI_M_viviendaEnt <- confint(M_viviendaEnt,level=0.90)[,1]
LI_M_limpiezaTot <- confint(M_limpiezaTot,level=0.90)[,1]
LI_M_limpiezaEnt <- confint(M_limpiezaEnt,level=0.90)[,1]
LI_M_saludTot <- confint(M_saludTot,level=0.90)[,1]
LI_M_saludEnt <- confint(M_saludEnt,level=0.90)[,1]
LI_M_transporteTot <- confint(M_transporteTot,level=0.90)[,1]
LI_M_transporteEnt <- confint(M_transporteEnt,level=0.90)[,1]
LI_M_educa_espaTot <- confint(M_educa_espaTot,level=0.90)[,1]
LI_M_educa_espaEnt <- confint(M_educa_espaEnt,level=0.90)[,1]
LI_M_personalesTot <- confint(M_personalesTot,level=0.90)[,1]
LI_M_personalesEnt <- confint(M_personalesEnt,level=0.90)[,1]
LI_M_transf_gasTot <- confint(M_transf_gasTot,level=0.90)[,1]
LI_M_transf_gasEnt <- confint(M_transf_gasEnt,level=0.90)[,1]


## -----------------------------------

LS_M_gasto_monTot <- confint(M_gasto_monTot,level=0.90)[,2]
LS_M_gasto_monEnt <- confint(M_gasto_monEnt,level=0.90)[,2]
LS_M_alimentosTot <- confint(M_alimentosTot,level=0.90)[,2]
LS_M_alimentosEnt <- confint(M_alimentosEnt,level=0.90)[,2]
LS_M_vesti_calzTot <- confint(M_vesti_calzTot,level=0.90)[,2]
LS_M_vesti_calzEnt <- confint(M_vesti_calzEnt,level=0.90)[,2]
LS_M_viviendaTot <- confint(M_viviendaTot,level=0.90)[,2]
LS_M_viviendaEnt <- confint(M_viviendaEnt,level=0.90)[,2]
LS_M_limpiezaTot <- confint(M_limpiezaTot,level=0.90)[,2]
LS_M_limpiezaEnt <- confint(M_limpiezaEnt,level=0.90)[,2]
LS_M_saludTot <- confint(M_saludTot,level=0.90)[,2]
LS_M_saludEnt <- confint(M_saludEnt,level=0.90)[,2]
LS_M_transporteTot <- confint(M_transporteTot,level=0.90)[,2]
LS_M_transporteEnt <- confint(M_transporteEnt,level=0.90)[,2]
LS_M_educa_espaTot <- confint(M_educa_espaTot,level=0.90)[,2]
LS_M_educa_espaEnt <- confint(M_educa_espaEnt,level=0.90)[,2]
LS_M_personalesTot <- confint(M_personalesTot,level=0.90)[,2]
LS_M_personalesEnt <- confint(M_personalesEnt,level=0.90)[,2]
LS_M_transf_gasTot <- confint(M_transf_gasTot,level=0.90)[,2]
LS_M_transf_gasEnt <- confint(M_transf_gasEnt,level=0.90)[,2]


## -----------------------------------
## Estimaciones
c_gas_ES <- data.frame(
c(ES_M_gasto_monTot,ES_M_gasto_monEnt),c(ES_M_alimentosTot,ES_M_alimentosEnt),
c(ES_M_vesti_calzTot,ES_M_vesti_calzEnt),c(ES_M_viviendaTot,ES_M_viviendaEnt),
c(ES_M_limpiezaTot,ES_M_limpiezaEnt),c(ES_M_saludTot,ES_M_saludEnt),
c(ES_M_transporteTot,ES_M_transporteEnt),c(ES_M_educa_espaTot,ES_M_educa_espaEnt),
c(ES_M_personalesTot,ES_M_personalesEnt),c(ES_M_transf_gasTot,ES_M_transf_gasEnt))

## Error estándar
c_gas_SE <- data.frame(
c(SE_M_gasto_monTot,SE_M_gasto_monEnt),c(SE_M_alimentosTot,SE_M_alimentosEnt),
c(SE_M_vesti_calzTot,SE_M_vesti_calzEnt),c(SE_M_viviendaTot,SE_M_viviendaEnt),
c(SE_M_limpiezaTot,SE_M_limpiezaEnt),c(SE_M_saludTot,SE_M_saludEnt),
c(SE_M_transporteTot,SE_M_transporteEnt),c(SE_M_educa_espaTot,SE_M_educa_espaEnt),
c(SE_M_personalesTot,SE_M_personalesEnt),c(SE_M_transf_gasTot,SE_M_transf_gasEnt))

## Coeficiente de variación
c_gas_CV <- data.frame(
c(CV_M_gasto_monTot,CV_M_gasto_monEnt),c(CV_M_alimentosTot,CV_M_alimentosEnt),
c(CV_M_vesti_calzTot,CV_M_vesti_calzEnt),c(CV_M_viviendaTot,CV_M_viviendaEnt),
c(CV_M_limpiezaTot,CV_M_limpiezaEnt),c(CV_M_saludTot,CV_M_saludEnt),
c(CV_M_transporteTot,CV_M_transporteEnt),c(CV_M_educa_espaTot,CV_M_educa_espaEnt),
c(CV_M_personalesTot,CV_M_personalesEnt),c(CV_M_transf_gasTot,CV_M_transf_gasEnt))

## Límite inferior
c_gas_LI <- data.frame(
c(LI_M_gasto_monTot,LI_M_gasto_monEnt),c(LI_M_alimentosTot,LI_M_alimentosEnt),
c(LI_M_vesti_calzTot,LI_M_vesti_calzEnt),c(LI_M_viviendaTot,LI_M_viviendaEnt),
c(LI_M_limpiezaTot,LI_M_limpiezaEnt),c(LI_M_saludTot,LI_M_saludEnt),
c(LI_M_transporteTot,LI_M_transporteEnt),
c(LI_M_educa_espaTot,LI_M_educa_espaEnt),c(LI_M_personalesTot,LI_M_personalesEnt),
c(LI_M_transf_gasTot,LI_M_transf_gasEnt))

## Límite superior
c_gas_LS <- data.frame(
c(LS_M_gasto_monTot,LS_M_gasto_monEnt),c(LS_M_alimentosTot,LS_M_alimentosEnt),
c(LS_M_vesti_calzTot,LS_M_vesti_calzEnt),c(LS_M_viviendaTot,LS_M_viviendaEnt),
c(LS_M_limpiezaTot,LS_M_limpiezaEnt),c(LS_M_saludTot,LS_M_saludEnt),
c(LS_M_transporteTot,LS_M_transporteEnt),c(LS_M_educa_espaTot,LS_M_educa_espaEnt),
c(LS_M_personalesTot,LS_M_personalesEnt),c(LS_M_transf_gasTot,LS_M_transf_gasEnt))


## Etiquetas de filas
names(c_gas_ES) <-c ("GASTO MON", "ALIMENTOS", "VEST y CALZ", "VIVIENDA", "LIMPIEZA",
"SALUD","TRANSPORTE", "EDUCACION", "PERSONALES", "TRANS DE GASTO")

names(c_gas_SE) <- c("GASTO MON", "ALIMENTOS", "VEST y CALZ", "VIVIENDA", "LIMPIEZA",
"SALUD","TRANSPORTE", "EDUCACION", "PERSONALES", "TRANS DE GASTO")

names(c_gas_CV) <- c("GASTO MON", "ALIMENTOS", "VEST y CALZ", "VIVIENDA","LIMPIEZA",
"SALUD", "TRANSPORTE", "EDUCACION", "PERSONALES", "TRANS DE GASTO")


## -----------------------------------
names(c_gas_LI) <- c("GASTO MON", "ALIMENTOS", "VEST y CALZ", "VIVIENDA","LIMPIEZA",
"SALUD", "TRANSPORTE", "EDUCACION", "PERSONALES", "TRANS DE GASTO")
names(c_gas_LS) <- c("GASTO MON", "ALIMENTOS", "VEST y CALZ", "VIVIENDA","LIMPIEZA",
"SALUD","TRANSPORTE", "EDUCACION", "PERSONALES", "TRANS DE GASTO")
## Se agregan los nombres de las entidades a las filas y se muestran los resultados
names <- (row.names(c_gas_ES) <- row.names(c_gas_SE) <-
row.names(c_gas_CV) <- row.names(c_gas_LI) <-
row.names(c_gas_CV) <- row.names(c_gas_LI) <-
row.names(c_gas_LS) <- Entidades)
round(c_gas_ES) |> 
  gt::gt()
round(c_gas_SE) |> 
  gt::gt()
(round(c_gas_CV,4)*100 )|> 
  gt::gt()
round(c_gas_LI) |> 
  gt::gt()
round(c_gas_LS) |> 
  gt::gt()


## -----------------------------------
## Limpia la pantalla de tablas o basura de un ejercicio anterior
rm(list = ls())
## Carga librerías
#library(xlsx) no JAVA please
library(foreign)
library(grid)
library(Matrix)
library(doBy)
library(survival)
library(reldist)
library(tidyverse)
library(survey)
options(survey.lonely.psu="adjust")
## Cuadro de sección 4 tabulados básicos ENIGH 2022
## Establece el directorio donde se encuentra la base de datos
##setwd("D:/ENIGH_2022")
## Abre la tabla concentradohogar
Conc<- read.dbf("concentradohogar.dbf",as.is = T)
## Selección de las variables de interés
Conc <- Conc[c("folioviv", "foliohog", "tam_loc", "factor", "upm", "est_dis",
"gasto_mon", "alimentos", "vesti_calz", "vivienda", "limpieza",
"salud", "transporte", "educa_espa", "personales", "transf_gas")]


## -----------------------------------
## Creación de variables
## Tamaño de localidad
Conc$tam <- ifelse(Conc$tam_loc<=3,1,2)
Conc$tam0 <- 0
## Gasto corriente monetario
Conc$A00 <- ifelse(Conc$gasto_mon>0,1,0)
## Alimentos, bebidas y tabaco
Conc$A01 <- ifelse(Conc$alimentos>0,1,0)
## Vestido y calzado
Conc$A05 <- ifelse(Conc$vesti_calz>0,1,0)
## Vivienda y servicios de conservación, energía eléctrica y combustibles
Conc$A08 <- ifelse(Conc$vivienda>0,1,0)
## Artículos y servicios para la limpieza, cuidados de la casa
Conc$A13 <- ifelse(Conc$limpieza>0,1,0)
## Cuidados de la salud
Conc$A17 <- ifelse(Conc$salud>0,1,0)
## Transporte; adquisición, mantenimiento, accesorios y servicios para vehiculos
Conc$A18 <- ifelse(Conc$transporte>0,1,0)
## Servicios de educación, artículos educativos, artículos de esparcimiento y otros gastos
Conc$A24 <- ifelse(Conc$educa_espa>0,1,0)
## Cuidados personales, accesorios y efectos personales y otros gastos
Conc$A28 <- ifelse(Conc$personales>0,1,0)
## Transferencias de gasto
Conc$A32 <- ifelse(Conc$transf_gas>0,1,0)
## Renombra variables
Conc <- Conc %>%
  rename_with(~ "B00", gasto_mon) %>%
  rename_with(~ "B01", alimentos) %>%
  rename_with(~ "B05", vesti_calz) %>%
  rename_with(~ "B08", vivienda) %>%
  rename_with(~ "B13", limpieza) %>%
  rename_with(~ "B17", salud) %>%
  rename_with(~ "B18", transporte) %>%
  rename_with(~ "B24", educa_espa) %>%
  rename_with(~ "B28", personales) %>%
  rename_with(~ "B32", transf_gas)

## Seleccionar variables de interés
Conc2 <- Conc [ c("folioviv","foliohog","upm","est_dis","factor","tam","tam0",
"A00","A01","A05","A08","A13","A17","A18","A24","A28","A32")]
Conc4 <- Conc [ c("folioviv","foliohog","upm","est_dis","factor","tam","tam0",
"B00","B01","B05","B08","B13","B17","B18","B24","B28","B32")]


## Transponer tabla (variables a casos)
Conc3 <- gather(Conc2,ING,Pob,-folioviv,-foliohog,-upm,-est_dis,-factor,-tam,-tam0)
Conc5 <- gather(Conc4,ING,Pob,-folioviv,-foliohog,-upm,-est_dis,-factor,-tam,-tam0)


## -----------------------------------
## Selecciona variables de las tablas creadas
Conc6 <- data.frame(Conc3,Conc5$ING,Conc5$Pob)
colnames(Conc6) <- c("folioviv","foliohog","upm","est_dis","factor","tam","tam0",
"H_TIPO","HOG","I_TIPO","ING")

## Diseño muestral
mydesign6 <- svydesign(id=~upm,strata=~est_dis,data=Conc6,weights=~factor)
I2 <- svyby(~ING, by=~I_TIPO+~tam0, mydesign6, svytotal)
H2 <- svyby(~HOG, by=~H_TIPO+~tam0, mydesign6, svytotal)
I3 <- svyby(~ING, by=~I_TIPO+~tam, mydesign6, svytotal)
H3 <- svyby(~HOG, by=~H_TIPO+~tam, mydesign6, svytotal)

## Etiquetas de filas
gastos <- c("GASTO CORRIENTE MONETARIO",
"ALIMENTOS, BEBIDAS Y TABACO", "VESTIDO Y CALZADO",
"VIVIENDA Y SERVICIOS DE CONSERVACIÓN",
"ARTÍCULOS Y SERVICIOS PARA LA LIMPIEZA, CUIDADOS DE LA CASA, ENSERES DOMÉSTICOS",
"CUIDADOS DE LA SALUD",
"TRANSPORTE; ADQUISICIÓN, MANTENIMIENTO, ACCESORIOS Y SERVICIOS",
"SERVICIOS DE EDUCACIÓN, ARTÍCULOS EDUCATIVOS, ARTÍCULOS DE ESPARCIMIENTO",
"CUIDADOS PERSONALES, ACCESORIOS Y EFECTOS", "TRANSFERENCIAS DE GASTO")
## Renombrar variables
colnames(I2)[2] <-
colnames(I3)[2] <-
colnames(H2)[2] <-
colnames(H3)[2] <- "tam"

## Pegar salida de diseño muestral
IN1 <- rbind(I2,I3)
HO1 <- rbind(H2,H3)

## Precisiones estadísticas
ES_H1 <- coef(HO1)
EE_H1 <- SE(HO1)
CV_H1 <- cv(HO1)*100
LI_H1 <- confint(HO1,level = 0.90)[,1]
LS_H1 <- confint(HO1,level = 0.90)[,2]
H <- cbind(HO1[,1:2],ES_H1,EE_H1,CV_H1,LI_H1,LS_H1)
ES_H2 <- H[c("H_TIPO", "tam", "ES_H1")]
EE_H2 <- H[c("H_TIPO", "tam", "EE_H1")]
CV_H2 <- H[c("H_TIPO", "tam", "CV_H1")]
LI_H2 <- H[c("H_TIPO", "tam", "LI_H1")]
LS_H2 <- H[c("H_TIPO", "tam", "LS_H1")]
ES_H3 <- spread(ES_H2, tam, ES_H1)
EE_H3 <- spread(EE_H2, tam, EE_H1)
CV_H3 <- spread(CV_H2, tam, CV_H1)
LI_H3 <- spread(LI_H2, tam, LI_H1)
LS_H3 <- spread(LS_H2, tam, LS_H1)


ES_I1 <- coef(IN1)/1000
EE_I1 <- SE(IN1)/1000
CV_I1 <- cv(IN1)*100
LI_I1 <- confint(IN1,level = 0.90)[,1]/1000
LS_I1 <- confint(IN1,level = 0.90)[,2]/1000
I <- cbind(IN1[,1:2],ES_I1,EE_I1,CV_I1,LI_I1,LS_I1)
ES_I2 <- I[c("I_TIPO", "tam","ES_I1")]
EE_I2 <- I[c("I_TIPO", "tam","EE_I1")]
CV_I2 <- I[c("I_TIPO", "tam","CV_I1")]
LI_I2 <- I[c("I_TIPO", "tam","LI_I1")]
LS_I2 <- I[c("I_TIPO", "tam","LS_I1")]
ES_I3 <- spread(ES_I2, tam, ES_I1)
EE_I3 <- spread(EE_I2, tam, EE_I1)
CV_I3 <- spread(CV_I2, tam, CV_I1)
LI_I3 <- spread(LI_I2, tam, LI_I1)
LS_I3 <- spread(LS_I2, tam, LS_I1)


## -----------------------------------
## Creación de tablas
TOTAL <- data.frame(ES_H3[,2],ES_I3[,2],ES_H3[,3],ES_I3[,3],ES_H3[,4],ES_I3[,4])
SE_TOTAL <- data.frame(EE_H3[,2],EE_I3[,2],EE_H3[,3],EE_I3[,3],EE_H3[,4],EE_I3[,4])
CV_TOTAL <- data.frame(CV_H3[,2],CV_I3[,2],CV_H3[,3],CV_I3[,3],CV_H3[,4],CV_I3[,4])
LI_TOTAL <- data.frame(LI_H3[,2],LI_I3[,2],LI_H3[,3],LI_I3[,3],LI_H3[,4],LI_I3[,4])
LS_TOTAL <- data.frame(LS_H3[,2],LS_I3[,2],LS_H3[,3],LS_I3[,3],LS_H3[,4],LS_I3[,4])

## Etiquetas de columnas
NOM <- c("TOTAL HOGARES",
"TOTAL GASTO",
"MÁS DE 2 500 HOGARES",
"MÁS DE 2 500 GASTO",
"MENOS DE 2 500 HOGARES",
"MENOS DE 2 500 GASTO")

## Se muestran los títulos de columnas
colnames(TOTAL) <- colnames(SE_TOTAL) <-
colnames(CV_TOTAL) <- colnames(LI_TOTAL) <-
colnames(LS_TOTAL) <- NOM

## Se muestran los títulos de filas
rownames(TOTAL) <- rownames(SE_TOTAL) <-
rownames(CV_TOTAL) <- rownames(LI_TOTAL) <-
rownames(LS_TOTAL) <- gastos

## Se muestran los resultado en pantalla
round(TOTAL,8) |> 
  gt::gt()
round(SE_TOTAL,8) |> 
  gt::gt()
round(CV_TOTAL,8) |> 
  gt::gt()
round(LI_TOTAL,8) |> 
  gt::gt()
round(LS_TOTAL,8) |> 
  gt::gt()


## -----------------------------------

# Cuadro 4.1
# Personas perceptoras de ingresos y su ingreso promedio trimestral
# monetario por tipo de discapacidad
# 2022
## Carga librerías
library(foreign)
library(survey)
## Limpia la pantalla de tablas o basura de un ejercicio anterior
rm(list = ls())
## Establece el directorio donde se encuentra la base de datos
##setwd("D:/ENIGH_2022")
## Abre la tabla concentradohogar
Conc <- read.dbf("concentradohogar.dbf",as.is = T )
## Selecciona las variables de interés
Conc <- Conc [ c("folioviv","foliohog","ubica_geo","tam_loc","factor","upm","est_dis")]
## Crea un ID especial a nivel número de renglón para usarse
## como referencia para pegar con otras tablas.
Conc$ID <- paste(Conc$folioviv,Conc$foliohog,sep=".")
## Abre la tabla ingresos
Ingr <- read.dbf("ingresos.dbf",as.is = TRUE)
## Selecciona las variables de interés de la tabla ingresos
Ingr <- Ingr[ c("folioviv", "foliohog", "numren", "clave", "ing_tri")]
## Agrega los ingresos por persona de la tabla ingresos
ingr1 <- aggregate(ing_tri ~ folioviv+foliohog+numren, Ingr, sum)

## Crea un ID especial a nivel número de renglón para usarse
## como referencia para pegar con otras tablas.
ingr1$ID <- paste(ingr1$folioviv,ingr1$foliohog,ingr1$numren,sep=".")

## Abre la tabla poblacion
Pers <- read.dbf("poblacion.dbf",as.is = T)

## Selecciona las variables de interés
Pers <- Pers[c("folioviv","foliohog","numren","parentesco","edad","sexo",
"disc_camin","disc_ver","disc_brazo","disc_apren",
"disc_oir","disc_vest","disc_habla","disc_acti")]

## Crea un ID especial a nivel número de renglón para usarse
## como referencia para pegar con otras tablas.
Pers$ID <- paste(Pers$folioviv,Pers$foliohog,Pers$numren,sep=".")
## Creación de variables auxiliares
## Selecciona a los integrantes del hogar
Pers$int <- ifelse((Pers$parentesco >= 400 & Pers$parentesco < 500 |
Pers$parentesco >= 700 & Pers$parentesco < 800),0,1)


## -----------------------------------

## Caminar
Pers$d_1 <- ifelse(Pers$disc_camin%in%c(1,2),1,0)
## Ver
Pers$d_2 <- ifelse(Pers$disc_ver%in%c(1,2),1,0)
## Mover
Pers$d_3 <- ifelse(Pers$disc_brazo%in%c(1,2),1,0)
## Aprender
Pers$d_4 <- ifelse(Pers$disc_apren%in%c(1,2),1,0)
## Escuchar
Pers$d_5 <- ifelse(Pers$disc_oir%in%c(1,2),1,0)
## Bañarse, vestirse
Pers$d_6 <- ifelse(Pers$disc_vest%in%c(1,2),1,0)
## Hablar
Pers$d_7 <- ifelse(Pers$disc_habla%in%c(1,2),1,0)
## Realizar actividades diarias
Pers$d_8 <- ifelse(Pers$disc_acti%in%c(1,2),1,0)
## No especificada
Pers$d_9 <- ifelse(Pers$disc_camin%in% "&",1,0)
## Con discapacidad
Pers$d_10 <- ifelse(Pers$d_1 == 1 | Pers$d_2 == 1 | Pers$d_3 == 1 | Pers$d_4 == 1 |
Pers$d_5 == 1 | Pers$d_6 == 1 | Pers$d_7 == 1 | Pers$d_8 == 1 |
Pers$d_9 == 1,1,0)

## Sin discapacidad
Pers$d_11 <- ifelse(Pers$d_1 == 0 & Pers$d_2 == 0 & Pers$d_3 == 0 & Pers$d_4 == 0 &
Pers$d_5 == 0 & Pers$d_6 == 0 & Pers$d_7 == 0 & Pers$d_8 == 0 &
Pers$d_9 == 0,1,0)


## -----------------------------------
## Pega variable ing_tri(ingreso por persona) de la tabla ingr1 a la tabla Pers
ENIGHpers20 <- merge(Pers,ingr1[,c("ID","ing_tri")],by="ID", all=TRUE)
## Elimina la variable ID que se integra hasta el integrante del hogar
ENIGHpers20$ID <- NULL
## Crea un ID especial a nivel de hogar para usarse como
## referencia para pegar con otras tablas
ENIGHpers20$ID <- paste(ENIGHpers20$folioviv,ENIGHpers20$foliohog,sep=".")
## Crea las variables del diseño muestral de concentradohogar a la tabla ENIGHpers20
ENIGHperDisc <- merge(ENIGHpers20,Conc[,c("ID","upm","est_dis","factor")],by="ID",all = T)
## Crea una variable donde se marcan a los perceptores (perceptores 1, no perceptores 0)
ENIGHperDisc$percep <- ifelse(ENIGHperDisc$ing_tri > 0,1,0)
## Elimina a los que no son integrantes (empleados domésticos y huéspedes)
ENIGHperDisc <- ENIGHperDisc[ENIGHperDisc$int == 1, ]
## Creación de variables
## Recodifica a los perceptores con valor en percep NA a cero.
ENIGHperDisc$percep[is.na(ENIGHperDisc$percep)] <- 0
## Identifica los ingresos de los perceptores
ENIGHperDisc$ing_per <- ifelse(ENIGHperDisc$int == 1 &
ENIGHperDisc$percep == 1 &
ENIGHperDisc$ing_tri > 0,ENIGHperDisc$ing_tri,0)
## Para contar los perceptores de ingreso con discapacidad
for(i in 1:11)
{
eval(parse(text = paste0("ENIGHperDisc$percep_d_",i," <-
ifelse(ENIGHperDisc$d_",i," == 1 &
ENIGHperDisc$percep == 1,1,0)")))
}

## Para contar los ingresos de los perceptores con discapacidad
for(i in 1:11)
{
eval(parse(text = paste0("ENIGHperDisc$ing_d_",i," <-
ifelse(ENIGHperDisc$d_",i," == 1 &
ENIGHperDisc$percep == 1 &
ENIGHperDisc$ing_tri > 0,ENIGHperDisc$ing_tri,0)")))
}




## -----------------------------------
## Se carga el diseño muestral
mydesign <- svydesign(id=~upm,strata=~est_dis,data=ENIGHperDisc,weights=~factor)

## Total de perceptores
percep <- svytotal(~percep, mydesign)
## Total promedio de ingreso de perceptores
percep_mean <- svyratio(~ing_per,denominator=~percep,mydesign)
## Totales de perceptores por tipo de discapacidad
for(i in 1:11)
{
eval(parse(text = paste0("percep_",i," <-
svytotal(~percep_d_",i,",
mydesign)")))
}
## Ingreso promedio de perceptores por tipo de discapacidad
for(i in 1:11)
{
eval(parse(text = paste0("percep_mean_",i," <-
svyratio(~ing_d_",i,",
denominator=~percep_d_",i,",
mydesign)")))
}


## -----------------------------------
## Estimaciones
## Población
ES_percep <- percep[[1]] # Total de perceptores
ES_percep11 <- percep_11[[1]] # Integrantes sin discapacidad
ES_percep10 <- percep_10[[1]] # Integrantes con dispacapidad
ES_percep1 <- percep_1[[1]] # Caminar
ES_percep2 <- percep_2[[1]] # Ver
ES_percep3 <- percep_3[[1]] # Mover
ES_percep4 <- percep_4[[1]] # Aprender
ES_percep5 <- percep_5[[1]] # Escuchar
ES_percep6 <- percep_6[[1]] # Bañarse
ES_percep7 <- percep_7[[1]] # Hablar
ES_percep8 <- percep_8[[1]] # Realizar actividades
ES_percep9 <- percep_9[[1]] # Discapacidad no especificada
## Ingreso
ES_percep_ing <- percep_mean[[1]]
ES_percep11_ing <- percep_mean_11[[1]]
ES_percep10_ing <- percep_mean_10[[1]]
ES_percep1_ing <- percep_mean_1[[1]]
ES_percep2_ing <- percep_mean_2[[1]]
ES_percep3_ing <- percep_mean_3[[1]]
ES_percep4_ing <- percep_mean_4[[1]]
ES_percep5_ing <- percep_mean_5[[1]]
ES_percep6_ing <- percep_mean_6[[1]]
ES_percep7_ing <- percep_mean_7[[1]]
ES_percep8_ing <- percep_mean_8[[1]]
ES_percep9_ing <- percep_mean_9[[1]]


## -----------------------------------

## Población
EE_percep <- SE(percep)
EE_percep11 <- SE(percep_11)
EE_percep10 <- SE(percep_10)
EE_percep1 <- SE(percep_1)
EE_percep2 <- SE(percep_2)
EE_percep3 <- SE(percep_3)
EE_percep4 <- SE(percep_4)
EE_percep5 <- SE(percep_5)
EE_percep6 <- SE(percep_6)
EE_percep7 <- SE(percep_7)
EE_percep8 <- SE(percep_8)
EE_percep9 <- SE(percep_9)
## Ingreso
EE_percep_ing <- SE(percep_mean)
EE_percep11_ing <- SE(percep_mean_11)
EE_percep10_ing <- SE(percep_mean_10)
EE_percep1_ing <- SE(percep_mean_1)
EE_percep2_ing <- SE(percep_mean_2)
EE_percep3_ing <- SE(percep_mean_3)
EE_percep4_ing <- SE(percep_mean_4)
EE_percep5_ing <- SE(percep_mean_5)
EE_percep6_ing <- SE(percep_mean_6)
EE_percep7_ing <- SE(percep_mean_7)
EE_percep8_ing <- SE(percep_mean_8)
EE_percep9_ing <- SE(percep_mean_9)


## -----------------------------------

## Población
CV_percep <- cv(percep)
CV_percep11 <- cv(percep_11)
CV_percep10 <- cv(percep_10)
CV_percep1 <- cv(percep_1)
CV_percep2 <- cv(percep_2)
CV_percep3 <- cv(percep_3)
CV_percep4 <- cv(percep_4)
CV_percep5 <- cv(percep_5)
CV_percep6 <- cv(percep_6)
CV_percep7 <- cv(percep_7)
CV_percep8 <- cv(percep_8)
CV_percep9 <- cv(percep_9)
## Ingreso
CV_percep_ing <- cv(percep_mean)
CV_percep11_ing <- cv(percep_mean_11)
CV_percep10_ing <- cv(percep_mean_10)
CV_percep1_ing <- cv(percep_mean_1)
CV_percep2_ing <- cv(percep_mean_2)
CV_percep3_ing <- cv(percep_mean_3)
CV_percep4_ing <- cv(percep_mean_4)
CV_percep5_ing <- cv(percep_mean_5)
CV_percep6_ing <- cv(percep_mean_6)
CV_percep7_ing <- cv(percep_mean_7)
CV_percep8_ing <- cv(percep_mean_8)
CV_percep9_ing <- cv(percep_mean_9)


## -----------------------------------
## Población
LI_percep <- confint(percep,level=0.90)[1,1]
LI_percep11 <- confint(percep_11,level=0.90)[1,1]
LI_percep10 <- confint(percep_10,level=0.90)[1,1]
LI_percep1 <- confint(percep_1,level=0.90)[1,1]
LI_percep2 <- confint(percep_2,level=0.90)[1,1]
LI_percep3 <- confint(percep_3,level=0.90)[1,1]
LI_percep4 <- confint(percep_4,level=0.90)[1,1]
LI_percep5 <- confint(percep_5,level=0.90)[1,1]
LI_percep6 <- confint(percep_6,level=0.90)[1,1]
LI_percep7 <- confint(percep_7,level=0.90)[1,1]
LI_percep8 <- confint(percep_8,level=0.90)[1,1]
LI_percep9 <- confint(percep_9,level=0.90)[1,1]
## Ingreso
LI_percep_ing <- confint(percep_mean,level=0.90)[1,1]
LI_percep11_ing <- confint(percep_mean_11,level=0.90)[1,1]
LI_percep10_ing <- confint(percep_mean_10,level=0.90)[1,1]
LI_percep1_ing <- confint(percep_mean_1,level=0.90)[1,1]
LI_percep2_ing <- confint(percep_mean_2,level=0.90)[1,1]
LI_percep3_ing <- confint(percep_mean_3,level=0.90)[1,1]
LI_percep4_ing <- confint(percep_mean_4,level=0.90)[1,1]
LI_percep5_ing <- confint(percep_mean_5,level=0.90)[1,1]
LI_percep6_ing <- confint(percep_mean_6,level=0.90)[1,1]
LI_percep7_ing <- confint(percep_mean_7,level=0.90)[1,1]
LI_percep8_ing <- confint(percep_mean_8,level=0.90)[1,1]
LI_percep9_ing <- confint(percep_mean_9,level=0.90)[1,1]



## Límite superior
## Población
LS_percep <- confint(percep,level=0.90)[1,2]
LS_percep11 <- confint(percep_11,level=0.90)[1,2]
LS_percep10 <- confint(percep_10,level=0.90)[1,2]
LS_percep1 <- confint(percep_1,level=0.90)[1,2]
LS_percep2 <- confint(percep_2,level=0.90)[1,2]
LS_percep3 <- confint(percep_3,level=0.90)[1,2]
LS_percep4 <- confint(percep_4,level=0.90)[1,2]
LS_percep5 <- confint(percep_5,level=0.90)[1,2]
LS_percep6 <- confint(percep_6,level=0.90)[1,2]
LS_percep7 <- confint(percep_7,level=0.90)[1,2]
LS_percep8 <- confint(percep_8,level=0.90)[1,2]
LS_percep9 <- confint(percep_9,level=0.90)[1,2]
## Ingreso
LS_percep_ing <- confint(percep_mean,level=0.90)[1,2]
LS_percep11_ing <- confint(percep_mean_11,level=0.90)[1,2]
LS_percep10_ing <- confint(percep_mean_10,level=0.90)[1,2]
LS_percep1_ing <- confint(percep_mean_1,level=0.90)[1,2]
LS_percep2_ing <- confint(percep_mean_2,level=0.90)[1,2]
LS_percep3_ing <- confint(percep_mean_3,level=0.90)[1,2]
LS_percep4_ing <- confint(percep_mean_4,level=0.90)[1,2]
LS_percep5_ing <- confint(percep_mean_5,level=0.90)[1,2]
LS_percep6_ing <- confint(percep_mean_6,level=0.90)[1,2]
LS_percep7_ing <- confint(percep_mean_7,level=0.90)[1,2]
LS_percep8_ing <- confint(percep_mean_8,level=0.90)[1,2]
LS_percep9_ing <- confint(percep_mean_9,level=0.90)[1,2]


## -----------------------------------
## Estimaciones
ES_disc <- data.frame(c(ES_percep,ES_percep11,ES_percep10,
ES_percep1,ES_percep2,ES_percep3,
ES_percep4,ES_percep5,ES_percep6,
ES_percep7,ES_percep8,ES_percep9),
c(ES_percep_ing,ES_percep11_ing,
ES_percep10_ing,ES_percep1_ing,
ES_percep2_ing,ES_percep3_ing,
ES_percep4_ing,ES_percep5_ing,
ES_percep6_ing,ES_percep7_ing,
ES_percep8_ing,ES_percep9_ing))

row.names(ES_disc) <- c("TOTAL",
" NO TIENE DISCAPACIDAD",
" CON DISCAPACIDAD",
" CAMINAR, MOVERSE, SUBIR O BAJAR",
" VER, AUN USANDO LENTES",
" MOVER O USAR BRAZOS O MANOS",
" APRENDER, RECORDAR O CONCENTRARSE",
" ESCUCHAR, AUNQUE USE APARATO AUDITIVO",
" BAÑARSE, VESTIRSE O COMER",
" HABLAR O COMUNICARSE"," REALIZAR SUS ACTIVIDADES DIARIAS",
" DISCAPACIDAD NO ESPECIFICADA")

names(ES_disc) <- c("PERSONAS PERCEPTORAS DE INGRESO","INGRESO PROMEDIO (PESOS)")

## Error estándar
EE_disc <- data.frame(c(EE_percep,EE_percep11,EE_percep10,EE_percep1,
EE_percep2,EE_percep3,EE_percep4,EE_percep5,
EE_percep6,EE_percep7,EE_percep8,EE_percep9),
c(EE_percep_ing,EE_percep11_ing,EE_percep10_ing,
EE_percep1_ing,EE_percep2_ing,EE_percep3_ing,
EE_percep4_ing,EE_percep5_ing,EE_percep6_ing,
EE_percep7_ing,EE_percep8_ing,EE_percep9_ing))

row.names(EE_disc) <- c("TOTAL",
" NO TIENE DISCAPACIDAD",
" CON DISCAPACIDAD",
" CAMINAR, MOVERSE, SUBIR O BAJAR",
" VER, AUN USANDO LENTES",
" MOVER O USAR BRAZOS O MANOS",
" APRENDER, RECORDAR O CONCENTRARSE",
" ESCUCHAR, AUNQUE USE APARATO AUDITIVO",
" BAÑARSE, VESTIRSE O COMER",
" HABLAR O COMUNICARSE",
" REALIZAR SUS ACTIVIDADES DIARIAS",
" DISCAPACIDAD NO ESPECIFICADA")
names(EE_disc) <-c ("PERSONAS PERCEPTORAS DE INGRESO","INGRESO PROMEDIO (PESOS)")

## Coeficiente de variación
CV_disc <- data.frame(c(CV_percep,CV_percep11,CV_percep10,CV_percep1,
CV_percep2,CV_percep3,CV_percep4,CV_percep5,
CV_percep6,CV_percep7,CV_percep8,CV_percep9),
c(CV_percep_ing,CV_percep11_ing,CV_percep10_ing,
CV_percep1_ing,CV_percep2_ing,CV_percep3_ing,
CV_percep4_ing,CV_percep5_ing,CV_percep6_ing,
CV_percep7_ing,CV_percep8_ing,CV_percep9_ing))
row.names(CV_disc) <- c("TOTAL",
" NO TIENE DISCAPACIDAD",
" CON DISCAPACIDAD",
" CAMINAR, MOVERSE, SUBIR O BAJAR",
" VER, AUN USANDO LENTES",
" MOVER O USAR BRAZOS O MANOS",
" APRENDER, RECORDAR O CONCENTRARSE",
" ESCUCHAR, AUNQUE USE APARATO AUDITIVO",
" BAÑARSE, VESTIRSE O COMER",
" HABLAR O COMUNICARSE",
" REALIZAR SUS ACTIVIDADES DIARIAS",
" DISCAPACIDAD NO ESPECIFICADA")

names(CV_disc)<-c("PERSONAS PERCEPTORAS DE INGRESO",
"INGRESO PROMEDIO (PESOS)")

## Límite inferior
LI_disc <- data.frame(c(LI_percep,LI_percep11,LI_percep10,
LI_percep1,LI_percep2,LI_percep3,
LI_percep4,LI_percep5,LI_percep6,
LI_percep7,LI_percep8,LI_percep9),
c(LI_percep_ing,LI_percep11_ing,
LI_percep10_ing,LI_percep1_ing,
LI_percep2_ing,LI_percep3_ing,
LI_percep4_ing,LI_percep5_ing,
LI_percep6_ing,LI_percep7_ing,
LI_percep8_ing,LI_percep9_ing))

row.names(LI_disc) <- c("TOTAL",
" NO TIENE DISCAPACIDAD",
" CON DISCAPACIDAD",
" CAMINAR, MOVERSE, SUBIR O BAJAR",
" VER, AUN USANDO LENTES",
" MOVER O USAR BRAZOS O MANOS",
" APRENDER, RECORDAR O CONCENTRARSE",
" ESCUCHAR, AUNQUE USE APARATO AUDITIVO",
" BAÑARSE, VESTIRSE O COMER",
" HABLAR O COMUNICARSE",
" REALIZAR SUS ACTIVIDADES DIARIAS",
" DISCAPACIDAD NO ESPECIFICADA")

names(LI_disc) <- c("PERSONAS PERCEPTORAS DE INGRESO","INGRESO PROMEDIO (PESOS)")

## Límite superior
LS_disc <- data.frame(c(LS_percep,LS_percep11,LS_percep10,LS_percep1,LS_percep2,
LS_percep3,LS_percep4,LS_percep5,LS_percep6,LS_percep7,
LS_percep8,LS_percep9),
c(LS_percep_ing,LS_percep11_ing,LS_percep10_ing,LS_percep1_ing,
LS_percep2_ing,LS_percep3_ing,LS_percep4_ing,LS_percep5_ing,
LS_percep6_ing,LS_percep7_ing,LS_percep8_ing,LS_percep9_ing))

row.names(LS_disc) <- c("TOTAL",
" NO TIENE DISCAPACIDAD",
" CON DISCAPACIDAD",
" CAMINAR, MOVERSE, SUBIR O BAJAR",
" VER, AUN USANDO LENTES",
" MOVER O USAR BRAZOS O MANOS",
" APRENDER, RECORDAR O CONCENTRARSE",
" ESCUCHAR, AUNQUE USE APARATO AUDITIVO",
" BAÑARSE, VESTIRSE O COMER",
" HABLAR O COMUNICARSE",
" REALIZAR SUS ACTIVIDADES DIARIAS",
" DISCAPACIDAD NO ESPECIFICADA")

names(LS_disc) <- c("PERSONAS PERCEPTORAS DE INGRESO",
"INGRESO PROMEDIO (PESOS)")

## Resultados en pantalla
ES_disc |> 
  gt::gt()
round(EE_disc) |> 
  gt::gt()
(round(CV_disc,4)*100 ) |> 
  gt::gt()
LI_disc |> 
  gt::gt()
LS_disc |> 
  gt::gt()

