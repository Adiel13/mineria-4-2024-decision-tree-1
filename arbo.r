install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

vivienda <- read.csv('C:/Users/kevin/OneDrive/Documentos/MIIC/Mineria/Censo2018/VIVIENDA_BDP.csv', sep = ',')

arbol <- rpart(AREA ~
               PCV1+
               PCV2+
               PCV3+
               PCV4+
               PCV5,
               data = vivienda, method = "class")

rpart.plot(arbol, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de area", cex = 1)

casa <- data.frame(
  PCV1=c(2),
  PCV2=c(3),
  PCV3=c(1),
  PCV4=c(1),
  PCV5=c(1)
  )

result <- predict(arbol,casa, type="class")

result

vivienda$ciudad = ifelse(vivienda$MUNICIPIO==101,1,0)

arbol2 <- rpart(ciudad ~
                 PCV1+
                 PCV2+
                 PCV3+
                 PCV4+
                 PCV5,
               data = vivienda, method = "class")



rpart.plot(arbol2, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de ciudad", cex = 1)


casa2 <- data.frame(
  PCV1=c(2),
  PCV2=c(3),
  PCV3=c(1),
  PCV4=c(1),
  PCV5=c(1)
)

result2 <- predict(arbol2,casa2, type="class")
result2


arbol3 <- rpart(DEPARTAMENTO ~
                  PCV1+
                  PCV2+
                  PCV3+
                  PCV4+
                  PCV5,
                data = vivienda, method = "class")

rpart.plot(arbol3, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de departamento", cex = 0.5)


hogar <- read.csv('C:/Users/kevin/OneDrive/Documentos/MIIC/Mineria/Censo2018/HOGAR_BDP.csv', sep = ',')


arbol4 <- rpart(PCH9_I ~
                  PCH9_D+
                  PCH9_G+
                  PCH9_K+
                  PCH9_M,
                data = hogar, method = "class")


rpart.plot(arbol4, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de internet", cex = 0.5)


arbol5 <- rpart(PCH9_I ~
                  PCH9_D+
                  PCH9_G+
                  PCH9_K+
                  PCH9_M+
                  PCH9_H+
                  PCH9_C,
                data = hogar, method = "class")

rpart.plot(arbol5, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de internet", cex = 0.5)


arbol6<- rpart(AREA ~
                  PCH9_D+
                  PCH9_G+
                  PCH9_K+
                  PCH9_M+
                  PCH9_H+
                  PCH9_C+
                 PCH9_I,
                data = hogar, method = "class")


rpart.plot(arbol6, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de area con base a servicios", cex = 0.5)
