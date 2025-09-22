library(rJava)
library(XLConnect)

#> wb <- loadWorkbook("C:/Users/matia/Downloads/DS_Actividad6.xlsx")


wb <- loadWorkbook("DS_Actividad6.xlsx")  #Le cambié el nombre al archivo
datos <- readWorksheet(wb, sheet = "Hoja1")

pairs(datos) #Aquí es para generar una matriz de dispersión entre cada variable

model <- lm(AMI ~ GEN + PR + DIAP + AMT + QRS, datos)
summary(model)
#Aquí vemos que los valores de PR, DIAP y QRS son muy altos, por lo que los eliminamos
#Abajo

model2 <- lm(AMI ~ GEN + AMT, datos)
summary(model2)

#Por últmio vemos que el intercepto, es decir la constante, no es significativa

model3 <- lm(AMI ~ 0 + GEN + AMT, datos)
summary(model3)

model4 <- lm(AMI ~ 0 + GEN + AMT + DIAP, datos) #Hice este para ver si mejoraba o no al agregar el DIAP





#Al final quedamos con que la AMI es dependiente solo de AMT y el género, además tiene sentido
#que la constante o interceptero sea 0, pues por qué el nivel de la cantidad de la droga en la sangre
#sería mayor a 0 si no se ha inyectado la droga.

#(5) al comparar los modelos usando anova, podemos ver en el p value que la pérdida de ajuste no es significativa, por lo que podríamos quedarnos con los modelos más simples.
#Al comparar los R^2, vemos que el modelo completo tiene un valor de 0.8764 y el ajustado 0.8202
#Mientras que el modelo simplificado ( el que llamamos model3) tiene 0.9138 y 0.9024 respectivamente, por lo que
#Podemos intuir que las variables extra no ayudan

anova(model,model3)

#(6) Scatter plot:
y_pred <- fitted(model3)
y_real <- datos$AMI

plot(y_pred,y_real, xlab="valores de predicción", ylab = "Valores Observados", main = "Model3 vs Datos")
abline(a = 0, b=1, col = "blue", lwd = 2)
#Histograma
dev.new()
hist(datos$DIAP)

#Test de shapiro:

library(nortest)

shapiro.test(datos$GEN)
shapiro.test(datos$AMT)

#Intervalo de confianza

confint(model3, level = 0.95)  # 95% de confianza
#Gráfico QQ
qqnorm(datos$AMT)
qqline(datos$AMT)

#Gráfico residuos vs valores ajustados

plot(fitted(modelo), resid(modelo),
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = "Residuos vs Valores ajustados")
abline(h = 0, col = "red", lwd = 2)  # línea horizontal en 0

#Gráfico de cooks distance 
> plot(cooks.distance(model3),
       +      type = "h",
       +      main = "Cook's Distance",
       +      ylab = "Distancia de Cook")
> abline(h = 4/length(model3$fitted.values), col = "red", lty = 2)
