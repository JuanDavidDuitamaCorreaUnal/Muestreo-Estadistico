#################Implementacion articulo muestreo
#####################Importación de datos#####################
setwd("C:/Users/juand/Desktop/Estadística/Semestres/Semestre 8/MuestreoEstadistico/ArticulosPropuestos/DatosFaltantes")
Cancer<-read.csv("breast-cancer-wisconsin.data.txt")
N <- nrow(Cancer)
cor(Cancer[,3],Cancer[,4])#Voy a hecerlos con la 3 y la 4 y la 8
Y <- Cancer[,3]#Uniformity of cell size
B <- Cancer[,4]#Uniformity of cell shape
X <- Cancer[,8]#Bland chromatin
set.seed(1729)
S = sample (x=c(0 ,1) , size = N , prob = c (0.5 ,0.5) , replace = TRUE )

complete_cases <- data.frame (Y, X, B, S)[S == 1,]#Casos en donde toda la información está completa 
observed_data <- data.frame (Y, X, B, S)
observed_data[S==0 , 'B'] <- NA #Convirtiendo la variable B en NA si S es 0

#####################Paso1: Imputar B|X#####################
#Recordar que se obtuvieron 50 conjuntos de datos imputados que luego se ponderarán
#utilizando f(Y|X)
imputes <- mice::mice ( observed_data , m=50 , method ="norm", maxit = 1)
pred <- imputes$predictorMatrix
pred[ pred != 0] <- 0
pred ["B","X"] <- 1
imputes <- mice::mice ( observed_data , m=50 , predictorMatrix =pred , method ="norm")

#####################Paso 2: Apilar los datasets imputados#####################
stack <- mice::complete ( imputes , action ="long", include = FALSE )#Apilando los datos

#####################Paso3: Obtener los pesos#####################
library ( dplyr )
fit_cc <- glm (Y ~ X + B, family ='gaussian', data = complete_cases )#Aquí se está haciendo un modelo lineal con todos los datos que sí reportaron observaciones en todas sus covariables
stack$wt <- dnorm( stack $Y, mean = predict ( fit_cc , newdata = stack ), sd = sqrt( summary(fit_cc)$dispersion ))#Se crea una nueva columna con los pesos de forma que sean asignados gracias a f(Y|X)
stack <- as.data.frame ( stack %>% group_by (.id) %>% mutate (wt = wt / sum (wt))) #Ahora sí están finalmente ponderados

#####################Step 4: Estimación#####################
fit <- glm (Y ~X + B, data =stack , family = gaussian () , weights = stack$wt) #Ahora se hace el modelo nuevamente pero con todos los nuevos datos de stack
Info <- StackImpute :: Louis_Information (fit , stack , M = 50, IMPUTED=unique(stack$.id[stack$S==0]))
fit$coefficients
VARIANCE <- diag ( solve ( Info ));VARIANCE

#Datos verdaderos
xd<-data.frame (Y, X, B)
fit2 <- glm (Y ~X + B, data = xd , family = gaussian ())
resumen2<-summary(fit2)
fit2$coefficients
resumen2$coefficients[, "Std. Error"]^2
