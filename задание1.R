library(MASS) # загружаем пакет
library(lmtest) #для тестирования
library(class)
data(Boston) # открываем данные
?Boston # справка по данным
str(Boston)
train.percent <- 0.85
n.all <- 506
#построим линейную модель
model1 <- lm(crim ~ indus + age + indus:chas + age:chas, data = Boston)
summary(model1)
model2 <- lm(crim ~ indus + age + indus:chas, data = Boston)
summary(model2)
#тест на гетероскедастичность остатков
bptest(model2)
#----в остатках присутствует гетероскедастичность
#тест на автокорреляцию
dwtest(model2, alternative = 'two.sided')
#----присутствует автокорреляция

plot(model2,1)
plot(model2,2)
plot(model2,3)
plot(model2,4)
plot(model2,5)
plot(model2,6)

#построим knn-регрессию
y.fact <- Boston[-inTrain, 1]
y.model.lm <- predict(model.6, df.test)
MSE.lm <- sum((y.model.lm - y.fact)^2) / length(y.model.lm)

