# загрузка пакетов
library('GGally')       # графики совместного разброса переменных
library('lmtest')       # тесты остатков регрессионных моделей
library('FNN')          # алгоритм kNN
library('MASS')         # загружаем пакет

# константы
my.seed <- 12345
train.percent <- 0.85

# загрузка данных
data(Boston)
str(Boston)

# преобразуем категориальные переменные в факторы
Boston$chas <- as.factor(Boston$chas)

# обучающая выборка
set.seed(my.seed)
inTrain <- sample(seq_along(Boston$crim), 
                  nrow(Boston) * train.percent)
df.train <- Boston[inTrain, c(colnames(Boston)[-1], colnames(Boston)[1])]
df.test <- Boston[-inTrain, -1]

# описательные статистики по переменным
summary(df.train)

# совместный график разброса переменных
ggp <- ggpairs(df.train)
print(ggp, progress = F)

# цвета по фактору chas
ggp1 <- ggpairs(df.train[, c('crim', 'indus', 'age')], 
               mapping = ggplot2::aes(color = chas))
print(ggp1, progress = F)

#модели 
model.1 <- lm(crim ~indus + age + indus:chas + age:chas,
              data = df.train)
summary(model.1)

model.2 <- lm(crim ~indus + age + indus:chas,
              data = df.train)
summary(model.2)

#проверка остатков
# тест Бройша-Пагана
bptest(model.2)#гетероскедастичность присутствует

# статистика Дарбина-Уотсона
dwtest(model.2)#автокорреляция отсутствует

# графики остатков
par(mar = c(4.5, 4.5, 2, 1))
par(mfrow = c(1, 3))
plot(model.2, 1)
plot(model.2, 4)
plot(model.2, 5)

par(mfrow = c(1, 1))

# фактические значения y на тестовой выборке
y.fact <- Boston[-inTrain, 1]
y.model.lm <- predict(model.2, df.test)
MSE.lm <- sum((y.model.lm - y.fact)^2) / length(y.model.lm)

# kNN требует на вход только числовые переменные
df.train.num <- as.data.frame(apply(df.train, 2, as.numeric))
df.test.num <- as.data.frame(apply(df.test, 2, as.numeric))

for (i in 2:50){
  model.knn <- knn.reg(train = df.train.num[, !(colnames(df.train.num) %in% 'crim')], 
                       y = df.train.num[, 'crim'], 
                       test = df.test.num, k = i)
  y.model.knn <- model.knn$pred
  if (i == 2){
    MSE.knn <- sum((y.model.knn - y.fact)^2) / length(y.model.knn)
  } else {
    MSE.knn <- c(MSE.knn, 
                 sum((y.model.knn - y.fact)^2) / length(y.model.knn))
  }
}

# график
par(mar = c(4.5, 4.5, 1, 1))
plot(2:50, MSE.knn, type = 'b', col = 'darkgreen',
     xlab = 'значение k', ylab = 'MSE на тестовой выборке')
lines(2:50, rep(MSE.lm, 49), lwd = 2, col = grey(0.2), lty = 2)
legend('bottomright', lty = c(1, 2), pch = c(1, NA), 
       col = c('darkgreen', grey(0.2)), 
       legend = c('k ближайших соседа', 'регрессия (все факторы)'), 
       lwd = rep(2, 2))
