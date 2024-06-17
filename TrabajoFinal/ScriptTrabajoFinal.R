#################Scipt trabajo final Muestreo

# Entendiendo el problema -------------------------------------------------



#Trabajo escrito sobre la implementación de un diseño muestral en dos etapas
#usando datos reales
#El trabajo escrito tiene como objetivo principal aplicar un diseño en dos etapas a un
#conjunto de datos reales. El conjunto de datos reales debe tener un mínimo de 5000
#observaciones y comprender mínimo 5 variables, tres de las cuales son variables continuas 
#y dos variables categóricas. Los datos reales pueden ser alguna de las encuestas
#del DANE u otra encuesta de alguna oficina de estadística de otro país y para efectos
#del trabajo en la clase, éstas constituirán la población.
#El trabajo escrito debe incluir:
#1. Introducción.
#2. Resumen descriptivo de los datos.
#3. Justificación teórica e implementación del diseño muestral empleado en cada etapa de muestreo.
#4. Cálculo del tamaño de muestra apropiado indicando la fórmula a utilizar y realizar
#las correspondientes justificaciones teóricas correspondientes.
#5. Estimación de los parámetros asociados con las variables de tipo continuo como también de las variables categóricas.
#6. Conclusiones y/o comentarios finales.
#7. Bibliografía.

#Variables a utilizar
#################Numéricas
#CANTIDAD.DE.CARGOS.PROVISTOS.MÁXIMO.NIVEL.DECISORIO
#CANTIDAD.DE.HOMBRES.EN.CARGOS.DE.MÁXIMO.NIVEL.DECISORIO
#CANTIDAD.DE.MUJERES.EN.CARGOS.DE.MÁXIMO.NIVEL.DECISORIO

#################Categóricas
#Nivel-CENTRAL
#Tipo de vinculación-ADSCRITAS




#Muestreo en dos etapas, dos posibles intentos

#1. Dividir en regiones y que esos sean los estratos, luego las UPMs se seleccionan de
#   esas regiones, en este caso las UPMs son los departamentos, luego dentro de cada
#   departamento se muestrea y se obtienen las USMs, en este caso cada fila de la base
#   https://colombia.travel/es/blog/revista-colombia/pais-de-regiones

#2. Ya tenemos los estratos que son los departamentos(salieron 34), de ahí se muestrean
#   municipios, es decir, los municipios son las UPMs, luego dentro de cada municipio
#   se muestrean las USMs, que son las filas de la base


#3. Hacer un MAS-MAS, se muestrean y se seleccionan los departamentos, luego, dentro de 
#   cada departamento se hace otro MAS de entidades
#Me gusta más la primera


#Se requieren ciertas estimaciones para algunos parámetros, yo propongo usar el año 2017 como
#piloto y los demás años como población



# Tratamiento de Datos ----------------------------------------------------


missings<-function(x){sum(is.na(x))}

library(sqldf)
library(dplyr)
library(FactoClass)
Ley_de_Cuotas <- read.csv("C:/Users/juand/Desktop/Estadística/Semestres/Semestre 8/MuestreoEstadistico/TrabajoFinal/Ley_de_Cuotas.csv")

ConteoDepart<-sqldf("Select DEPARTAMENTO, count(*) as CantSale
                     From Ley_de_Cuotas
                     Group by DEPARTAMENTO")
DepRegion<-data.frame(DEPARTAMENTO=c("BOGOTÁ, D.C.","ANTIOQUIA","BOYACÁ","CALDAS","CUNDINAMARCA","HUILA","NORTE DE SANTANDER","QUINDÍO","RISARALDA","SANTANDER","TOLIMA","AMAZONAS","CAQUETÁ","GUAINÍA","GUAVIARE","PUTUMAYO","VAUPÉS","VALLE DEL CAUCA","CHOCÓ","CAUCA","NARIÑO","ATLÁNTICO","BOLÍVAR","CESAR","CÓRDOBA","LA GUAJIRA","MAGDALENA","SUCRE","ARAUCA","CASANARE","META","VICHADA","ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA"),
                      REGION=c(rep("ANDINA",11),rep("AMAZONIA",6),rep("PACIFICA",4),rep("CARIBE",7),rep("ORINOQUIA",4),"INSULAR"))
CantRegion<-sqldf("Select REGION, count(*) as Deptos From DepRegion Group by REGION")

#Hay tres observaciones que no reportaron el departamento ni el municipio
sqldf("Select * From Ley_de_Cuotas Where DEPARTAMENTO=''")
Ley_de_Cuotas$DEPARTAMENTO<-ifelse(Ley_de_Cuotas$DEPARTAMENTO=="","META",Ley_de_Cuotas$DEPARTAMENTO)
#Listo arreglado, 33 departamentos ahora sí
#Ahora le pego la región a los departamentos y selecciono las variables que quiero utilizar

Poblacion<-sqldf("Select A.AÑO as Año, A.NIVEL as Nivel, A.'TIPO.DE.VINCULACIÓN' as Vinculacion, A.'CANTIDAD.DE.CARGOS.PROVISTOS.MÁXIMO.NIVEL.DECISORIO' as CantCargosMax, A.'CANTIDAD.DE.HOMBRES.EN.CARGOS.DE.MÁXIMO.NIVEL.DECISORIO' as CantMaxHombres, A.'CANTIDAD.DE.MUJERES.EN.CARGOS.DE.MÁXIMO.NIVEL.DECISORIO' as CantMaxMujer, A.DEPARTAMENTO as Departamento, B.REGION
                  From Ley_de_Cuotas as A
                  Left Join DepRegion as B on A.DEPARTAMENTO=B.DEPARTAMENTO")

apply(Poblacion,2,missings)#Casi no hay NAs, hay en dos variables numéricas

ConteoDepart<-sqldf("Select DEPARTAMENTO, count(*) as CantSale
                     From Poblacion
                     Group by DEPARTAMENTO")
ConteoRegion<-sqldf("Select REGION, count(*) as CantSale
                     From Poblacion
                     Group by REGION")
Pobla<-sqldf("Select * 
              From Poblacion
              Where Año>2017")
Pobla$Nivel<-ifelse(Pobla$Nivel=="CENTRAL",1,0)
Pobla$Vinculacion<-ifelse(Pobla$Vinculacion=="ADSCRITAS",1,0)
p<-574/1297#Hagamos que la propor
# Encontrando los tamaños -------------------------------------------------
#Utilizaremos el año 2017 para calcular las proporciones/ estimaciones que pide el
#cálculo del tamaño de la muestra, habrán 33 tamaños de muestra porque hay 33 departamentos

Anio2017<-sqldf("Select * 
                 From Poblacion
                 Where Año=2017")
table(Anio2017$Nivel)
p<-574/1297#Hagamos que la proporción de interés sea nivel Central
deff<-1.5#Toca definir bien el deff
z_alpha<-qnorm(0.975)#alpha=0.05
ErrorRelav<-0.1
Tnr<-0.05
################Tamaños de muestra de personas n_k en cada región, alpha=0.05
#Región Amazonía
N_Amazonia<-sum(Pobla$REGION=="AMAZONIA");N_Amazonia
n_Amazonia<-ceiling((z_alpha^2*N_Amazonia*p*(1-p))/((ErrorRelav^2*p^2*(N_Amazonia-1))+(z_alpha^2*p*(1-p)))*(deff)/(1-Tnr));n_Amazonia

#Región Andina
N_Andina<-sum(Pobla$REGION=="ANDINA");N_Andina
n_Andina<-ceiling((z_alpha^2*N_Andina*p*(1-p))/((ErrorRelav^2*p^2*(N_Andina-1))+(z_alpha^2*p*(1-p)))*(deff)/(1-Tnr));n_Andina

#Region Caribe
N_Caribe<-sum(Pobla$REGION=="CARIBE");N_Caribe
n_Caribe<-ceiling((z_alpha^2*N_Caribe*p*(1-p))/((ErrorRelav^2*p^2*(N_Caribe-1))+(z_alpha^2*p*(1-p)))*(deff)/(1-Tnr));n_Caribe

#Region Insular
N_Insular<-sum(Pobla$REGION=="INSULAR");N_Insular
n_Insular<-ceiling((z_alpha^2*N_Insular*p*(1-p))/((ErrorRelav^2*p^2*(N_Insular-1))+(z_alpha^2*p*(1-p)))*(deff)/(1-Tnr));n_Insular
n_Insular<-22
#Region Orinoquia
N_Orinoquia<-sum(Pobla$REGION=="ORINOQUIA");N_Orinoquia
n_Orinoquia<-ceiling((z_alpha^2*N_Orinoquia*p*(1-p))/((ErrorRelav^2*p^2*(N_Orinoquia-1))+(z_alpha^2*p*(1-p)))*(deff)/(1-Tnr));n_Orinoquia

#Region Pacifica
N_Pacifica<-sum(Pobla$REGION=="PACIFICA");N_Pacifica
n_Pacifica<-ceiling((z_alpha^2*N_Pacifica*p*(1-p))/((ErrorRelav^2*p^2*(N_Pacifica-1))+(z_alpha^2*p*(1-p)))*(deff)/(1-Tnr));n_Pacifica

#Cuántos departamentos se muestrearán en cada regióm

#Región Amazonia
Amazonia<-sqldf("Select * From Pobla Where REGION='AMAZONIA'")
table(Amazonia$Nivel)
r_Amazonia<-177/318#Conociendo la proporcion de nivel central de cada region
n_Amazonia/(r_Amazonia*CantRegion$Deptos[1])
b_Amazonia<-sum(sqldf("Select CantCargosMax From Anio2017 Where REGION='AMAZONIA'"))/6;b_Amazonia#Promedio de cantidad de cargos max

nDep_Amazo<-ceiling(n_Amazonia/(r_Amazonia*b_Amazonia));nDep_Amazo #Se muestrean 3 deptos de región Amazónica

#Región Andina
Andina<-sqldf("Select * From Pobla Where REGION='ANDINA'")
table(Andina$Nivel)
r_Andina<-2627/5952#Conociendo la proporcion de nivel central de cada region
n_Andina/(r_Andina*CantRegion$Deptos[2])
b_Andina<-sum(sqldf("Select CantCargosMax From Anio2017 Where REGION='ANDINA'"))/11;b_Andina#Promedio de cantidad de cargos max

nDep_Andina<-ceiling(n_Andina/(r_Andina*b_Andina));nDep_Andina #Se muestrean 3 deptos de región Amazónica

#Región Caribe
Caribe<-sqldf("Select * From Pobla Where REGION='CARIBE'")
table(Caribe$Nivel)
r_Caribe<-658/1359#Conociendo la proporcion de nivel central de cada region
n_Caribe/(r_Caribe*CantRegion$Deptos[3])
b_Caribe<-sum(sqldf("Select CantCargosMax From Anio2017 Where REGION='CARIBE'"))/7;b_Caribe#Promedio de cantidad de cargos max

nDep_Caribe<-ceiling(n_Caribe/(r_Caribe*b_Caribe));nDep_Caribe #Se muestrean 3 deptos de región Amazónica

#Región Orinoquia
Orinoquia<-sqldf("Select * From Pobla Where REGION='ORINOQUIA'")
table(Orinoquia$Nivel)
r_Orinoquia<-235/481#Conociendo la proporcion de nivel central de cada region
n_Orinoquia/(r_Orinoquia*CantRegion$Deptos[5])
b_Orinoquia<-sum(sqldf("Select CantCargosMax From Anio2017 Where REGION='ORINOQUIA'"))/4;b_Orinoquia#Promedio de cantidad de cargos max

nDep_Orinoquia<-ceiling(n_Orinoquia/(r_Orinoquia*b_Orinoquia));nDep_Orinoquia #Se muestrean 3 deptos de región Amazónica

#Región Pacífica
Pacifica<-sqldf("Select * From Pobla Where REGION='PACIFICA'")
table(Pacifica$Nivel)
r_Pacifica<-622/1328#Conociendo la proporcion de nivel central de cada region
n_Pacifica/(r_Pacifica*CantRegion$Deptos[6])
b_Pacifica<-sum(sqldf("Select CantCargosMax From Anio2017 Where REGION='PACIFICA'"))/4;b_Pacifica#Promedio de cantidad de cargos max

nDep_Pacifica<-ceiling(n_Pacifica/(r_Pacifica*b_Pacifica));nDep_Pacifica #Se muestrean 3 deptos de región Amazónica

#En conclusión, toca coger todos los departamentos en todas las regiones excepto la Andina

#Cuantas personas se muestrean en cada departamento conociendo ya cuantas deben haber en cada región
table(Pobla$REGION)
#Region Amazónica
n_Amazonas<-ceiling(n_Amazonia*(sum(Pobla$Departamento=="AMAZONAS")/N_Amazonia));n_Amazonas
n_Caqueta<-ceiling(n_Amazonia*(sum(Pobla$Departamento=="CAQUETÁ")/N_Amazonia));n_Caqueta
n_Guainia<-ceiling(n_Amazonia*(sum(Pobla$Departamento=="GUAINÍA")/N_Amazonia));n_Guainia
n_Guaviare<-ceiling(n_Amazonia*(sum(Pobla$Departamento=="GUAVIARE")/N_Amazonia));n_Guaviare
n_Putumayo<-ceiling(n_Amazonia*(sum(Pobla$Departamento=="PUTUMAYO")/N_Amazonia));n_Putumayo
n_Vaupes<-ceiling(n_Amazonia*(sum(Pobla$Departamento=="VAUPÉS")/N_Amazonia));n_Vaupes  

#Region Andina
#Aquí si debo muestrear 5 departamentos de la región Andina
nDep_Andina
DeptosAndina<-sqldf("Select Departamento, count(*) as CantDeptos From Andina group by Departamento")

set.seed(1975)#Este año nació mi mamá xd
Hola<-DeptosAndina%>%
  sample_n(size=5,replace=FALSE)%>%
  as.data.frame()
#Salió Bogotá, Huila, Quindío, Antioquia y Norte de Santander

n_Bogota<-ceiling(n_Andina*(Hola$CantDeptos[1]/sum(Hola$CantDeptos)));n_Bogota
n_Huila<-ceiling(n_Andina*(Hola$CantDeptos[2]/sum(Hola$CantDeptos)));n_Huila
n_Quindio<-ceiling(n_Andina*(Hola$CantDeptos[3]/sum(Hola$CantDeptos)));n_Quindio
n_Antioquia<-ceiling(n_Andina*(Hola$CantDeptos[4]/sum(Hola$CantDeptos)));n_Antioquia
n_NortSant<-ceiling(n_Andina*(Hola$CantDeptos[5]/sum(Hola$CantDeptos)));n_NortSant

#Region Caribe

n_Atlantico<-ceiling(n_Caribe*(sum(Pobla$Departamento=="ATLÁNTICO")/N_Caribe));n_Atlantico
n_Bolivar<-ceiling(n_Caribe*(sum(Pobla$Departamento=="BOLÍVAR")/N_Caribe));n_Bolivar
n_Cesar<-ceiling(n_Caribe*(sum(Pobla$Departamento=="CESAR")/N_Caribe));n_Cesar
n_Cordoba<-ceiling(n_Caribe*(sum(Pobla$Departamento=="CÓRDOBA")/N_Caribe));n_Cordoba
n_Guajira<-ceiling(n_Caribe*(sum(Pobla$Departamento=="LA GUAJIRA")/N_Caribe));n_Guajira
n_Magdalena<-ceiling(n_Caribe*(sum(Pobla$Departamento=="MAGDALENA")/N_Caribe));n_Magdalena 
n_Sucre<-ceiling(n_Caribe*(sum(Pobla$Departamento=="SUCRE")/N_Caribe));n_Sucre

#Region Insular
n_Insular

#Region Orinoquia
n_Orinoquia
n_Arauca<-ceiling(n_Orinoquia*(sum(Pobla$Departamento=="ARAUCA")/N_Orinoquia));n_Arauca
n_Casanare<-ceiling(n_Orinoquia*(sum(Pobla$Departamento=="CASANARE")/N_Orinoquia));n_Casanare
n_Meta<-ceiling(n_Orinoquia*(sum(Pobla$Departamento=="META")/N_Orinoquia));n_Meta
n_Vichada<-ceiling(n_Orinoquia*(sum(Pobla$Departamento=="VICHADA")/N_Orinoquia));n_Vichada

#Region Pacifica
n_Pacifica
n_ValleCau<-ceiling(n_Pacifica*(sum(Pobla$Departamento=="VALLE DEL CAUCA")/N_Pacifica));n_ValleCau
n_Choco<-ceiling(n_Pacifica*(sum(Pobla$Departamento=="CHOCÓ")/N_Pacifica));n_Choco
n_Cauca<-ceiling(n_Pacifica*(sum(Pobla$Departamento=="CAUCA")/N_Pacifica));n_Cauca
n_Narino<-ceiling(n_Pacifica*(sum(Pobla$Departamento=="NARIÑO")/N_Pacifica));n_Narino


# Resumen -----------------------------------------------------------------

#Cantidad de entidades que se muestrearán por región
Res1<-data.frame(Region=CantRegion$REGION,Tamaños=c(n_Amazonia,n_Andina,n_Caribe,n_Insular,n_Orinoquia,n_Pacifica));Res1
xtable(Res1)
#Cantidad de Departamentos que se muestrearán por cada región
Res2<-data.frame(Region=CantRegion$REGION,CantDeptos=c(nDep_Amazo,nDep_Andina,nDep_Caribe,1,nDep_Orinoquia,nDep_Pacifica));Res2
xtable(Res2)
#Cantidad de entidades que se muestrearán en cada departamento
Res3<-data.frame(Departamento=c("Amazonas","Caquetá","Guainía","Guaviare","Putumayo","Vaupés","Bogotá","Huila","Quindío","Antioquia","Norte de Santander","Atlántico","Bolívar","Cesar","Córdoba","La Guajira","Magdalena","Sucre","San Andrés","Arauca","Casanare","Meta","Vichada","Valle del Cauca","Chocó","Cauca","Nariño"),
                 CantidadEntidades=c(n_Amazonas,n_Caqueta,n_Guainia,n_Guaviare,n_Putumayo,n_Vaupes,n_Bogota,n_Huila,n_Quindio,n_Antioquia,n_NortSant,n_Atlantico,n_Bolivar,n_Cesar,n_Cordoba,n_Guajira,n_Magdalena,n_Sucre,n_Insular,n_Arauca,n_Casanare,n_Meta,n_Vichada,n_ValleCau,n_Choco,n_Cauca,n_Narino));Res3
xtable(Res3)
DeptosSal<-toupper(c("Amazonas","Caquetá","Guainía","Guaviare","Putumayo","Vaupés","Bogotá, D.C.","Huila","Quindío","Antioquia","Norte de Santander","Atlántico","Bolívar","Cesar","Córdoba","La Guajira","Magdalena","Sucre","ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA","Arauca","Casanare","Meta","Vichada","Valle del Cauca","Chocó","Cauca","Nariño"))
# Muestreo por cada uno de los departamentos ------------------------------


Muestreo<-NULL
set.seed(1729)
for(i in 1:length(DeptosSal)){
  xd<-Pobla%>%filter(Departamento==DeptosSal[i])%>%as.data.frame()
  Muestreo<-rbind(Muestreo,
                  xd%>%sample_n(size=Res3$CantidadEntidades[i])%>%as.data.frame()
                  )
}

write.csv(Muestreo,"C:/Users/juand/Desktop/Estadística/Semestres/Semestre 8/MuestreoEstadistico/TrabajoFinal/MuestreoFinal")

ConteoDepart2<-sqldf("Select DEPARTAMENTO, count(*) as CantSale
                     From Pobla
                     Group by DEPARTAMENTO")

# Estimaciones ------------------------------------------------------------
#Total de Cargos Max es 
sum(Pobla$CantCargosMax,na.rm = T)
#Todos los departamentos a excepción de los pertenecientes a la región Andina tienen probabilidad
#de inclusión 1, en cambio, los de la región Andina tienen 5/11
#Las probabilidades de inclusión de segundo orden como es un MAS deben tener valores diferentes
#según el departamento

#########Estos son los Verdaderos totales
SumasMax <- tapply(Pobla$CantCargosMax, Pobla$Departamento, function(x) sum(x, na.rm = TRUE));SumasMax
SumasHombre <- tapply(Pobla$CantMaxHombres, Pobla$Departamento, function(x) sum(x, na.rm = TRUE));SumasHombre
SumasMujer <- tapply(Pobla$CantMaxMujer, Pobla$Departamento, function(x) sum(x, na.rm = TRUE));SumasMujer
SumasCentral<-tapply(Pobla$Nivel, Pobla$Departamento, function(x) sum(x, na.rm = TRUE));SumasCentral
SumasAdscr<-tapply(Pobla$Vinculacion, Pobla$Departamento, function(x) sum(x, na.rm = TRUE));SumasAdscr

#########Estos son las sumas de la muestra
SumasMaxM <- tapply(Muestreo$CantCargosMax, Muestreo$Departamento, function(x) sum(x, na.rm = TRUE));SumasMaxM
SumasHombreM <- tapply(Muestreo$CantMaxHombres, Muestreo$Departamento, function(x) sum(x, na.rm = TRUE));SumasHombreM
SumasMujerM <- tapply(Muestreo$CantMaxMujer, Muestreo$Departamento, function(x) sum(x, na.rm = TRUE));SumasMujerM
SumasCentralM<-tapply(Muestreo$Nivel, Muestreo$Departamento, function(x) sum(x, na.rm = TRUE));SumasCentralM
SumasAdscrM<-tapply(Muestreo$Vinculacion, Muestreo$Departamento, function(x) sum(x, na.rm = TRUE));SumasAdscrM

#Encontrando los pi estimadores de cada departamento
rownames(table(Muestreo$Departamento))

orden1<-ConteoDepart2$CantSale[c(1:7,10:14,16:26,29,31:33)]
orden2<-c(n_Amazonas,n_Antioquia,n_Arauca,n_Insular,n_Atlantico,n_Bogota,n_Bolivar,n_Caqueta,n_Casanare,n_Cauca,n_Cesar,n_Choco,n_Cordoba,n_Guainia,n_Guaviare,n_Huila,n_Guajira,n_Magdalena,n_Meta,n_Narino,n_NortSant,n_Putumayo,n_Quindio,n_Sucre,n_ValleCau,n_Vaupes,n_Vichada)
PiDepar<-NULL
for(i in 1:27){
  PiDepar<-rbind(PiDepar,c(rownames(table(Muestreo$Departamento))[i],(orden1[i]/orden2[i])*c(SumasMaxM[i],SumasHombreM[i],SumasMujerM[i],SumasCentralM[i],SumasAdscrM[i])))
}
colnames(PiDepar)<-c("Departamento","CantCargosMaxTot","CantCargosMaxH","CantCargosMaxM","CantNivel","CantAdscri")


#Listos, ahora que ya tengo las estimaciones del pi estimador por cada departamento, voy a hallar
#los de cada región, después ahí si hallo el total, recorsdemos que en todas las regiones excepto
#en la andina se suma y ya

##############Región Amazonia
PiAmazonia<-colSums(matrix(as.numeric(PiDepar[c(1,8,14,15,22,26),2:6]), ncol = 5));PiAmazonia
##############Región Andina
PiAndina<-(11/5)*colSums(matrix(as.numeric(PiDepar[c(2,6,16,21,23),2:6]), ncol = 5));PiAndina
##############Región Caribe
PiCaribe<-colSums(matrix(as.numeric(PiDepar[c(5,7,11,13,17,18,24),2:6]), ncol = 5));PiCaribe
##############Región Insular
PiInsular<-as.numeric(PiDepar[4,2:6]);PiInsular
##############Región Orinoquia
PiOrinoquia<-colSums(matrix(as.numeric(PiDepar[c(3,9,19,27),2:6]), ncol = 5));PiOrinoquia
##############Región Pacífica
PiPacifica<-colSums(matrix(as.numeric(PiDepar[c(25,10,12,20),2:6]), ncol = 5));PiPacifica

PiTotal<-PiAmazonia+PiAndina+PiCaribe+PiInsular+PiOrinoquia+PiPacifica;PiTotal

#########Estos son los Verdaderos totales por Region
SumasMaxR <- tapply(Pobla$CantCargosMax, Pobla$REGION, function(x) sum(x, na.rm = TRUE));SumasMaxR
SumasHombreR <- tapply(Pobla$CantMaxHombres, Pobla$REGION, function(x) sum(x, na.rm = TRUE));SumasHombreR
SumasMujerR <- tapply(Pobla$CantMaxMujer, Pobla$REGION, function(x) sum(x, na.rm = TRUE));SumasMujerR
SumasCentralR <- tapply(Pobla$Nivel, Pobla$REGION, function(x) sum(x, na.rm = TRUE));SumasCentralR
SumasAdscrR <- tapply(Pobla$Vinculacion, Pobla$REGION, function(x) sum(x, na.rm = TRUE));SumasAdscrR

##########################Ahora calculo la varianza del total

VarTerm1<-(11^2/5)*(1-5/11)*apply(PiDepar[c(6,16,23,2,21),2:6],2,var);VarTerm1


VarCarMax<-tapply(Muestreo[,4],Muestreo$Departamento,function(x) var(x, na.rm = TRUE));VarCarMax
VarCarH<-tapply(Muestreo[,5],Muestreo$Departamento,function(x) var(x, na.rm = TRUE));VarCarH
VarCarM<-tapply(Muestreo[,6],Muestreo$Departamento,function(x) var(x, na.rm = TRUE));VarCarM
VarNivel<-tapply(Muestreo[,2],Muestreo$Departamento,function(x) length(x)*mean(x, na.rm = TRUE)*(1-mean(x,na.rm=T))/(length(x)-1));VarNivel
VarVincu<-tapply(Muestreo[,3],Muestreo$Departamento,function(x) length(x)*mean(x, na.rm = TRUE)*(1-mean(x,na.rm=T))/(length(x)-1));VarVincu

Yacasi<-(orden1^2/orden2)*(1-orden2/orden1)*cbind(VarCarMax,VarCarH,VarCarM,VarNivel,VarVincu)

VarTerm2<-apply(Yacasi[c(1,8,14,15,22,26),],2,sum)+(11/5)*apply(Yacasi[c(2,6,16,21,23),],2,sum)+apply(Yacasi[c(5,7,11,13,17,18,24),],2,sum)+Yacasi[4,]+apply(Yacasi[c(3,9,19,27),],2,sum)+apply(Yacasi[c(25,10,12,20),],2,sum)

VarTotal<-VarTerm1+VarTerm2;VarTotal

#Coeficientes de variacion
sqrt(VarTotal)/PiTotal


#Resúmenes de los pi-estimadores y sus verdaderos valores por región
PiAmazonia
PiAndina
PiCaribe
PiInsular
PiOrinoquia
PiPacifica

SumasMaxR <- tapply(Pobla$CantCargosMax, Pobla$REGION, function(x) sum(x, na.rm = TRUE));SumasMaxR
SumasHombreR <- tapply(Pobla$CantMaxHombres, Pobla$REGION, function(x) sum(x, na.rm = TRUE));SumasHombreR
SumasMujerR <- tapply(Pobla$CantMaxMujer, Pobla$REGION, function(x) sum(x, na.rm = TRUE));SumasMujerR
SumasCentralR <- tapply(Pobla$Nivel, Pobla$REGION, function(x) sum(x, na.rm = TRUE));SumasCentralR
SumasAdscrR <- tapply(Pobla$Vinculacion, Pobla$REGION, function(x) sum(x, na.rm = TRUE));SumasAdscrR

Res4<-data.frame(EstimMax=c(PiAmazonia[1],PiAndina[1],PiCaribe[1],PiInsular[1],PiOrinoquia[1],PiPacifica[1]),EstimH=c(PiAmazonia[2],PiAndina[2],PiCaribe[2],PiInsular[2],PiOrinoquia[2],PiPacifica[2]),EstimM=c(PiAmazonia[3],PiAndina[3],PiCaribe[3],PiInsular[3],PiOrinoquia[3],PiPacifica[3]),EstimCent=c(PiAmazonia[4],PiAndina[4],PiCaribe[4],PiInsular[4],PiOrinoquia[4],PiPacifica[4]),EstimAds=c(PiAmazonia[5],PiAndina[5],PiCaribe[5],PiInsular[5],PiOrinoquia[5],PiPacifica[5]));Res4
Res5<-data.frame(SumasMaxR,SumasHombreR,SumasMujerR,SumasCentralR,SumasAdscrR)

#Resumen del pi estimador del total y los verdaderos
PiTotal
apply(Pobla[,c(4:6,2,3)],2,function(x) sum(x, na.rm = TRUE))

#Varianzas de esos piestimadores totales
VarTotal

#Coeficientes de variacion de esos piestimadores totales
sqrt(VarTotal)/PiTotal

xtable(rbind(apply(Pobla[,c(4:6,2,3)],2,function(x) sum(x, na.rm = TRUE)),PiTotal,VarTotal,sqrt(VarTotal)/PiTotal))
