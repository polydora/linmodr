# ---
# title: "Линейная регрессия"
# subtitle: "Линейные модели..."
# author: "Вадим Хайтов, Марина Варфоломеева"

## Читаем данные
brain <- read.csv("data/IQ_brain.csv", sep = ",",  header = TRUE)
head(brain)


## Находим строки с пропущенными значениями


## Строим график
brain_plot <-
brain_plot


#Вычисление коэффициента корреляции


#Вычисление матрицы корреляций



## Подгоняем модель с помощью функции lm()
brain_model <-



## Прогнозируем величину IQ для человека с размером мозга 900000




#Находим остатки




##Находим доверительные интервалы для параметров



##Рисуем графики для разных уровней значимости

pl_alpha1 <- pl_brain + geom_smooth(method="lm", level=0.8) + ggtitle(bquote(alpha==0.2))

pl_alpha2 <- pl_brain + geom_smooth(method="lm", level=0.95) + ggtitle(bquote(alpha==0.05))

pl_alpha3 <- pl_brain + geom_smooth(method="lm", level=0.999) + ggtitle(bquote(alpha==0.01))


grid.arrange(pl_alpha1, pl_alpha2, pl_alpha3, ncol=3)

## Зависимость в генеральной совокупности

pop_x <- rnorm(1000, 10, 3)
pop_y <- 10 + 10 * pop_x + rnorm(1000, 0, 20)
population <- data.frame(x = pop_x, y = pop_y)

pop_plot <- ggplot(population, aes(x = x, y = y)) +
  geom_point(alpha = 0.3, color = "red") +
  geom_abline(aes(intercept = 10, slope = 10),
              color = "blue", size = 2) +
  theme(text = element_text(size = 15))
pop_plot

## Доверительный интервал

samp_coef <- data.frame(b0 = rep(NA, 100), b1 = rep(NA, 100))

for(i in 1:100) {
  samp_num <- sample(1:1000, 20)
  samp <- population[samp_num, ]
  fit <- lm(y ~ x, data = samp)
  samp_coef$b0[i] <- coef(fit)[1]
  samp_coef$b1[i] <- coef(fit)[2]

}

ggplot(population, aes(x = x, y = y)) +
  geom_point(alpha = 0.3, color = "red") +
  geom_abline(aes(intercept = b0, slope = b1), data = samp_coef) +
  geom_abline(aes(intercept = 10, slope = 10), color = "blue", size = 2) +
  theme(text = element_text(size = 18))


## Вычисляем 95%  зону предсказания

newdata <- data.frame(MRINACount = 900000)

predict(brain_model, newdata, interval = "prediction", level = 0.95, se = TRUE)$fit


## Строим график с зоной педсказаия
brain_predicted <- predict(brain_model, interval="prediction")
brain_predicted <- data.frame(brain, brain_predicted)
head(brain_predicted)

pl_brain +

  # 1) Линия регрессии и ее дов. интервал
  # Если мы указываем fill внутри aes() и задаем фиксированное значение - появится соотв. легенда с названием.
  # alpha - задает прозрачность
  geom_smooth(method = "lm", aes(fill = "Conf.interval"), alpha = 0.4) +
  # 2) Интервал предсказаний создаем при помощи геома ribbon ("лента")
  # Данные берем из другого датафрейма - из brain_predicted
  # ymin и ymax - эстетики геома ribbon, которые задают нижний и верхний край ленты в точках с заданным x (x = MRINACount было задано в ggplot() при создании pl_brain, поэтому сейчас его указывать не обязательно)
  geom_ribbon(data = brain_predicted,  aes(ymin = lwr, ymax = upr, fill = "Conf. area for prediction"), alpha = 0.2) +

  # 3) Вручную настраиваем цвета заливки при помощи шкалы fill_manual.
  # Ее аргумент name - название соотв. легенды, values - вектор цветов
  scale_fill_manual(name = "Intervals", values = c("green", "gray")) +

  # 4) Название графика
  ggtitle("Confidence interval \n and confidence area for prediction")
