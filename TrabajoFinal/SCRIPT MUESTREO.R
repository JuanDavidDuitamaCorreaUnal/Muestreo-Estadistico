rm(list=ls())
library(dplyr)
library(ggplot2)
setwd("C:/Users/jufem/Downloads/" )
data=read.csv("Ley_de_Cuotas.csv")
data <- read.csv("C:/Users/juand/Desktop/Estadística/Semestres/Semestre 8/MuestreoEstadistico/TrabajoFinal/Ley_de_Cuotas.csv")
#data=na.omit(data)
#View(data)
data <- data[, -c(2,3,14,21,23)]
colnames(data) <- c("ID","ORD","SUBO","CLAS", "SEC", "NIV", "TVIN" ,"NATJ" ,"LOG" ,
                    "LAT", "ANNO","HMAX", "MMAX", "HOTR", "MOTR", "TMAX","TOTR","DEP","MUN")
data$DEP[data$DEP == "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA"] <- "SAN ANDRES"
data=tibble(data);data <-data[order(data$ANNO),]
#Seleccione el primero de los NITS
#data=data %>%group_by(ID) %>%slice(1);data <- data[order(data$ANNO),]

data <- data %>%
  mutate(REG= case_when(DEP=="AMAZONAS"| DEP=="GUAINÍA"|DEP=="GUAVIARE"|DEP=="CAQUETÁ"|DEP=="PUTUMAYO"|DEP=="VAUPÉS"~'AMAZONIA',
                        DEP== "BOLÍVAR"|DEP=="ATLÁNTICO" |DEP== "BOLÍVAR"| DEP=="MAGDALENA"| DEP=="CESAR"|DEP=="LA GUAJIRA"|DEP=="CÓRDOBA"|DEP=="SUCRE"|DEP=="SAN ANDRES"~'CARIBE',
                        DEP=="CAUCA"|DEP=="NARIÑO"|DEP=="VALLE DEL CAUCA"|DEP=="CHOCÓ"~'PACIFICO',
                        DEP=="ARAUCA"|DEP=="CASANARE"|DEP=="META"|DEP=="VICHADA"|DEP=="GUAVIARE"~'ORINOQUIA',
                        DEP %in% c("ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA")~"INSULAR",
                        TRUE~ 'ANDINA'))


#Descriptiva


library(grDevices)
library(paletteer)
library(RColorBrewer)

  x11()
  
  
paleta_colores <- paletteer_c("grDevices::Inferno", 47)[-c(1:10)]
  ggplot(data, aes(x = factor(DEP), y = MMAX, fill = factor(DEP))) +
    geom_boxplot(outlier.shape = 16, outlier.color = "red", outlier.size = 1, color = "black", linetype = "solid") +
    scale_fill_manual(values = paleta_colores) +
    labs(x = "Departamentos", y = "MMAX",, title = "MMAX vs DEP") +
    theme(legend.position = "none",  # Eliminar la leyenda
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotar etiquetas en el eje x
          axis.title.y = element_text(size = 14),  # Ajustar el tamaño del título del eje y
          axis.text = element_text(size = 12),panel.grid = element_blank(),  # Eliminar la grilla
          panel.background = element_rect(fill = "white"),plot.title = element_text(hjust = 0.5, face = "bold"))  # Ajustar el tamaño de las etiquetas 



  ggplot(data, aes(x = factor(DEP), y = TMAX, fill = factor(DEP))) +
    geom_boxplot(outlier.shape = 16, outlier.color = "red", outlier.size = 1, color = "black", linetype = "solid") +
    scale_fill_manual(values = paleta_colores) +
    labs(x = "Departamentos", y = "TMAX", title = "TMAX vs DEP") +
    theme(legend.position = "none",  # Eliminar la leyenda
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotar etiquetas en el eje x
          axis.title.y = element_text(size = 14),  # Ajustar el tamaño del título del eje y
          axis.text = element_text(size = 12),panel.grid = element_blank(),  # Eliminar la grilla
          panel.background = element_rect(fill = "white"),plot.title = element_text(hjust = 0.5, face = "bold"))  # Ajustar el tamaño de las etiquetas 
  
  
  
  paleta_colores_2 <- c("#BD2B73FF" ,"#E14E5DFF","#F47C2BFF", "#F7A227FF", "#FAE175FF")
    
    
    
  ggplot(data, aes(x = factor(REG), y = MMAX, fill = factor(REG))) +
    geom_boxplot(outlier.shape = 16, outlier.color = "red", outlier.size = 1, color = "black", linetype = "solid") +
    scale_fill_manual(values = paleta_colores_2) +
    labs(x = "REG", y = "MMAX", title = "MMAX vs REG") +
    theme_minimal() +  # Cambiar el tema a minimalista
    theme(legend.position = "none",  # Eliminar la leyenda
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotar etiquetas en el eje x
          axis.title.y = element_text(size = 14),  # Ajustar el tamaño del título del eje y
          axis.text = element_text(size = 12),  # Ajustar el tamaño de las etiquetas
          panel.grid = element_blank(),  # Eliminar la grilla
          panel.background = element_rect(fill = "white"),plot.title = element_text(hjust = 0.5, face = "bold"))  # Cambiar el fondo a blanco
  
  
  ggplot(data, aes(x = factor(REG), y = TMAX, fill = factor(REG))) +
    geom_boxplot(outlier.shape = 16, outlier.color = "red", outlier.size = 1, color = "black", linetype = "solid") +
    scale_fill_manual(values = paleta_colores_2) +
    labs(x = "REG", y = "TMAX", title = "TMAX vs REG") +
    theme_minimal() +  # Cambiar el tema a minimalista
    theme(legend.position = "none",  # Eliminar la leyenda
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotar etiquetas en el eje x
          axis.title.y = element_text(size = 14),  # Ajustar el tamaño del título del eje y
          axis.text = element_text(size = 12),  # Ajustar el tamaño de las etiquetas
          panel.grid = element_blank(),  # Eliminar la grilla
          panel.background = element_rect(fill = "white"),plot.title = element_text(hjust = 0.5, face = "bold"))  # Cambiar el fondo a blanco
  
  
  
  
  



#PPT



  library(survey)
  library(dplyr)
  library(pps)
  library(sampling)


###################
dataI=data%>%group_by(DEP)%>%summarise(x=n(),y=sum(MMAX),x_2=sum(TMAX))
#Primera etapa:
#Probabilidades de selccion----
#with(dataI,cor(y,x_2))#TMAX es una variable altamente correlacionada con y

dataI$pk<-with(dataI, x/sum(x))
#Seleccion de numero de extracciones:
#length(unique(data$DEP)) #hay 1016  municipios

rI=27 #asumiendo 110 extracciones
set.seed(123)
s<-pps::ppswr(dataI$x,rI)
muestra_DEP=dataI[s,]
muestra_DEP=muestra_DEP[order(-muestra_DEP$x),]

departamentos_select<-data.frame(muestra_DEP[,"DEP"])



n.mas=function(tipo,N,s,e,p,alpha){
  if(tipo=="t"){n=round((qnorm(1-alpha/2)^2*N^2*s^2)/(e^2+qnorm(1-alpha/2)^2*N*s^2),0)}
  if(tipo=="t"){return(n)}
  if(tipo=="m"){n=round((qnorm(1-alpha/2)^2*s^2)/(e^2+(qnorm(1-alpha/2)^2*s^2/N)),0)}
  if(tipo=="m"){return(n)}
  if(tipo=="p"){n=round((qnorm(1-alpha/2)^2*(N/(N-1))*p*(1-p))/(e^2+(qnorm(1-alpha/2)^2*(N/(N-1))*p*(1-p)*(1/N))),0)
  if(tipo=="p"){return(n)}
  }
}

Muestras<-NULL
NI<-NULL
tipo="p"
N=nrow(data)
alpha=0.05
p=0.5
e=0.05
#n.mas(tipo,N,s,e,p,alpha)
nI<-NULL
sumas_MMAX<-NULL
sumas_TMAX<-NULL
sumas_HMAX<-NULL
for(i in 1:rI){
entidades<-data%>%filter(DEP %in% departamentos_select[i,1] )  
NI[i]<-nrow(entidades)
nI[i]<-n.mas(tipo,NI[i],s,e,p,alpha )
iks<-srswor1(nI[i],nrow(entidades))
set.seed(768)
sample<-getdata(entidades,iks)
#Muestras<-rbind(Muestras,sample)
sumas_MMAX<-rbind(sumas_MMAX,sum(sample$MOTR))
sumas_TMAX<-rbind(sumas_TMAX,sum(sample$TOTR))
sumas_HMAX<-rbind(sumas_HMAX,sum(sample$HOTR))
}
  


p_k_select <- dataI %>% 
  inner_join(departamentos_select, by = c("DEP" = "DEP")) %>%
  select(pk)


#MMAX
testim_MMAX<-(1/rI)*(sum( (1/p_k_select)*(NI/nI)*sumas_MMAX   ) );testim_MMAX
#TMAX
testim_TMAX<-(1/rI)*(sum( (1/p_k_select)*(NI/nI)*sumas_TMAX   ) );testim_TMAX
#HMAX
testim_HMAX<-(1/rI)*(sum( (1/p_k_select)*(NI/nI)*sumas_HMAX   ) );testim_HMAX


#VARINZAS ESTIMADAS

varestim_MMAX=(1/(rI*(rI-1)))*(  sum(  ((1/p_k_select)*(NI/nI)*sumas_MMAX -testim_MMAX  )^2 ));varestim_MMAX

varestim_TMAX=(1/(rI*(rI-1)))*(  sum(( (1/p_k_select)*(NI/nI)*sumas_TMAX -testim_TMAX  )^2 ));varestim_TMAX
varestim_HMAX=(1/(rI*(rI-1)))*(  sum(( (1/p_k_select)*(NI/nI)*sumas_HMAX -testim_HMAX  )^2 ));varestim_HMAX

100*sqrt(varestim_MMAX)/testim_MMAX
100*sqrt(varestim_TMAX)/testim_TMAX
100*sqrt(varestim_HMAX)/testim_HMAX









 