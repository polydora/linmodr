# title: "Описание, проверка значимости линейных моделей"
# author: Марина Варфоломеева, Вадим Хайтов

# ## Пример: IQ и размеры мозга ##################
# Зависит ли уровень интеллекта от размера головного мозга? (Willerman et al. 1991)
# Было исследовано 20 девушек и 20 молодых людей
# - вес
# - рост
# - размер головного мозга (количество пикселей на изображении ЯМР сканера)
# - Уровень интеллекта измеряли с помощью IQ тестов
# Пример: Willerman, L., Schultz, R., Rutledge, J. N., and Bigler, E. (1991),
# "In Vivo Brain Size and Intelligence", Intelligence, 15, p.223--228.

# ## Вспомним, на чем мы остановились ############

library(readxl)
brain <- read.csv("data/IQ_brain.csv", header = TRUE)

brain_model <- lm(PIQ ~ MRINACount, data = brain)
summary(brain_model)

library(ggplot2)
theme_set(theme_bw())
ggplot(brain, aes(x = MRINACount, y = PIQ)) +
  geom_point() +
  geom_smooth(method = "lm")


# ## Зависит ли IQ от размера головного мозга?

# ## Тестирование гипотез с помощью t-критерия  ####
summary(brain_model)


# ## Тестирование гипотез при помощи F-критерия ####
summary(brain_model)
# Как видите, код тот же самый.
# Повторяю его здесь только ради полноты конспекта.
# Естественно, нет смысла, когда вы делаете анализ для себя.


# ## Оценка качества подгонки модели ####
# Коэффициент детерминации
summary(brain_model)


### Задание 1 & 2 -->-->-->-->-->-->-->-->------

# Выполните задания 1 и 2 в одном из этих файлов:
# - 07_task_assumptions_catsM.R
# - 07_task_assumptions_GAG.R



#### Диагностика линейных моделей ################

dat <- read.table('data/orly_owl_Lin_4p_5_flat.txt')

fit <- lm(V1 ~ V2 + V3 + V4 + V5 - 1, data = dat)

coef(summary(fit))

# Значимо ли влияние предикторов?

plot(fit)


# -----------------------------------------------------

# Постройте график зависимости остатков от
# предсказанных значений при помощи этого кода

library(car)
residualPlot(fit, pch = ".")






#
# http://www4.stat.ncsu.edu/~stefanski/NSF\_Supported/Hidden\_Images/stat\_res\_plots.html
#

# ## Данные для анализа остатков
library(ggplot2)
brain_diag <- fortify(brain_model)
head(brain_diag, 2)

## График расстояния Кука
ggplot(brain_diag, aes(x = 1:nrow(brain_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

## График остатков от предсказанных значений
gg_resid <- ggplot(data = brain_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0)
gg_resid

## Графики зависимости остатков от предикторов в модели
ggplot(data = brain_diag, aes(x = MRINACount, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0)
# То же самое с использованием ранее созданного gg_resid
gg_resid + aes(x =  MRINACount)

## Графики зависимости остатков от предикторов не в модели
# В данном случае в датасете нет других переменных,
# которые могли бы быть предикторами.

# Квантильный график остатков
qqPlot(brain_model) # из пакета car

# Проверка на гетероскедастичность
# Этот график у нас уже есть
gg_resid





# Тренинг по анализу остатков --------------------

# Задание 3 ----------------------------------------
#
# Выполните три блока кода.
# Какие нарушения условий применимости
# линейных моделей здесь наблюдаются?
# Для диагностики модели из каждого блока кода
# вам понадобится построить четыре графика:
# 1. График расстояния Кука
# 2. График остатков от предсказанных значений
# 3. Графики остатков от предикторов в модели и не в модели
# 4. Квантильный график остатков

# ## Задание 3, блок 1 -----------------------------
set.seed(90829)
x1 <- seq(1, 100, 1)
y1 <- diffinv(rnorm(99))  + rnorm(100, 0.2, 4)
dat1 = data.frame(x1, y1)
ggplot(dat1, aes(x = x1, y = y1)) + geom_point() +
  geom_smooth(method="lm", alpha = 0.7)
# датафрейм dat1, зависимость y1 от x1





# ## Задание 3, блок 2 -----------------------------
set.seed(7657674)
x2 <- runif(1000, 1, 100)
b_0 <- 100;  b_1 <- -20
h <- function(x) x^0.5
eps <- rnorm(1000, 0, h(x2))
y2 <- b_0 + b_1 * x2 + eps
dat2 <- data.frame(x2, y2)
ggplot(dat2, aes(x = x2, y = y2)) + geom_point() +
  geom_smooth(method = "lm", alpha = 0.7)
# датафрейм dat2, зависимость y2 от x2



# ## Задание 3, блок 3 -----------------------------
set.seed(9283)
x3 <- rnorm(25, 50, 10);
b_0 <- 20; b_1 <- 20; eps <- rnorm(50, 0, 100)
y3 <- b_0 + b_1*x3 + eps
y3[с(100, 99, 98)] <- с(1000, 1300, 1500); x3[с(100, 99, 98)] <- c(95, 90, 80)
dat3 <- data.frame(x3, y3)
ggplot(dat3, aes(x = x3, y = y3)) + geom_point() +
  geom_smooth(method = "lm", alpha = 0.7)
# датафрейм dat3, зависимость y3 от x3




# ## Задание 4 -->-->-->-->-->-->-->-->------

# Выполните последнее задание в одном из этих файлов:
# - 07_task_assumptions_catsM.R
# - 07_task_assumptions_GAG.R


####################### Своих врагов надо знать! ###############################


#################################################################
# Зоопарк нарушений условий применимости регрессионного анализа #
#################################################################

# Загон №1 (Curva nonlinearis)

#### Генерируем данные #########
set.seed(90829)
x1 <- rnorm(100, 5, 1)
y1 <- 10 + x1  - x1^2  + rnorm(100, 0, 2)
dat1 = data.frame(x1, y1)
################################

ggplot(dat1, aes(x = x1, y =  y1)) + geom_point()+
  geom_smooth(method="lm", alpha = 0.7)


mod1 <- lm(y1 ~ x1, data = dat1)

summary(mod1) #Можно ли доверять этим резултатам?



# Данные для графиков остатков
mod1_diag <- fortify(mod1)

# 1) График расстояния Кука
ggplot(mod1_diag, aes(x = 1:nrow(mod1_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <-
  ggplot(data = mod1_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0)

gg_resid

# 3) Графики остатков от предикторов в модели (желательно, включить и график зависимости остатков от тех предикторов, которые не включены в модель)
gg_resid + aes(x = x1)

# 4) Квантильный график остатков
library(car)
qqPlot(mod1, id = FALSE)

# В чем проблема?
gg_resid + geom_smooth()





# --------------------------------------------------------

# Загон № 2 (Heteroscedastica horribilis)

#######################
set.seed(7657674)
x2 <- runif(1000, 1, 100)
b_0 <- 100;  b_1 <- 20
h <- function(x) x^0.9
eps <- rnorm(1000, 0, h(x2^1.1))
y2 <- b_0 + b_1 * x2 + eps
dat2 <- data.frame(x2, y2)
######################

ggplot(dat2, aes(x = x2, y = y2)) +
  geom_point() +
  geom_smooth(method = "lm")

mod2 <- lm(y2 ~ x2, data = dat2)

summary(mod2) #Можно ли доверять этим резултатам?

# Данные для графиков остатков
mod2_diag <- fortify(mod2)
# 1) График расстояния Кука
ggplot(mod2_diag, aes(x = 1:nrow(mod2_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = mod2_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = x2)

# 4) Квантильный график остатков
qqPlot(mod2, id = FALSE)

# В чем проблема?

# --------------------------------------------------------

# Загон №3 (Mensurae atypica)

#################
set.seed(9283)
x3 <- rnorm(25, 50, 10)
b_0 <- 20; b_1 <- 20; eps <- rnorm(50, 0, 100)
y3 <- b_0 + b_1*x3 + eps
y3[100] <- 1000; x3[100] <- 95; y3[99] <- 1300; x3[99] <- 90; y3[98] <- 1500; x3[98] <- 80
dat3 <- data.frame(x3, y3)
#################

ggplot(dat3, aes(x=x3, y=y3)) + geom_point() + geom_smooth(method="lm")


mod3 <- lm(y3 ~ x3, data = dat3)

summary(mod3) #Можно ли доверять этим резултатам?


# Данные для графиков остатков
mod3_diag <- fortify(mod3)
# 1) График расстояния Кука
ggplot(mod3_diag, aes(x = 1:nrow(mod3_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = mod3_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = x3)

# 4) Квантильный график остатков
qqPlot(mod3, id = FALSE)


# В чем проблема?

# --------------------------------------------------------

# Загон № 4 (Anima amissa)

#######################
set.seed(7657674)
x1 <- runif(30, 1, 100)
x2 <- runif(30, 1, 100)
x3 <- runif(30, 1, 100)
b_0 <- 100;  b_1 <- -20; b_2 <- 20; b_3 <- 0
eps <- rnorm(30, 0, 1)
y <- b_0 + b_1 * x1 + b_2*x2 + b_3*x3 + eps
dat4 <- data.frame(x1, x2, x3, y)
######################

ggplot(dat4, aes(x=x1, y=y)) + geom_point() + geom_smooth(method="lm")


mod4 <- lm(y ~ x1, data = dat4)

summary(mod4) #Можно ли доверять этим результатам?


# Данные для графиков остатков
mod4_diag <- fortify(mod4)
# 1) График расстояния Кука
ggplot(mod4_diag, aes(x = 1:nrow(mod4_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = mod4_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid +
  geom_smooth(method = "lm")

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = x1)

gg_resid + aes(x = x2)

gg_resid + aes(x = x3)

# 4) Квантильный график остатков
qqPlot(mod4, id = FALSE)


# В чем проблема?

# --------------------------------------------------------

# Загон № 5 (Perdidit commercium)

#######################
set.seed(7657674)
x1 <- rnorm(100, 10, 1)
x2 <- rnorm(100, 100, 1)
b_0 <- 100;  b_1 <- -1; b_2 <- 2; b_12 <- 10
dat5 <- data.frame(x1, x2)
X <- model.matrix(~ x1 * x2, data = dat5)
dat5$y <- X %*% c(b_0, b_1, b_2, b_12) + rnorm(100, 0, 1)
######################

ggplot(dat5, aes(x=x1, y=y)) + geom_point() + geom_smooth(method="lm")

ggplot(dat5, aes(x=x2, y=y)) + geom_point() + geom_smooth(method="lm")


mod5 <- lm(y ~ x1 + x2, data = dat5)

vif(mod5)

summary(mod5) #Можно ли доверять этим резултатам?

# Данные для графиков остатков
mod5_diag <- fortify(mod5)
# 1) График расстояния Кука
ggplot(mod5_diag, aes(x = 1:nrow(mod5_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = mod5_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)

gg_resid

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = x1)

gg_resid + aes(x = x2)


# 4) Квантильный график остатков
qqPlot(mod5, id = FALSE)

# В чем проблема?

# --------------------------------------------------------

# Загон № 6 (Data longitudinalis)

#######################
set.seed(657674)
ts_AR1 <- as.numeric(arima.sim(n = 30, list(ar = 0.999)))
ts_AR2 <- as.numeric(arima.sim(n = 30, list(ar = 0.999)))
dat6 <- data.frame(Year = 1:30, V1 = ts_AR1, V2 = ts_AR2)
######################

ggplot(dat6, aes(x=V2, y=V1)) + geom_point() + geom_smooth(method="lm")



mod6 <- lm(V1 ~ V2, data = dat6)

summary(mod6) #Можно ли доверять этим результатам?

# Данные для графиков остатков
mod6_diag <- fortify(mod6)
# 1) График расстояния Кука
ggplot(mod6_diag, aes(x = 1:nrow(mod6_diag), y = .cooksd)) +
  geom_bar(stat = "identity")

# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = mod6_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)

gg_resid

# 3) Графики остатков от предикторов в модели и не в модели
gg_resid + aes(x = V2)

gg_resid + aes(x = dat6$Year)

# 4) Квантильный график остатков
qqPlot(mod6, id = FALSE)

# В чем проблема?

# Autocorrelation function
acf(residuals(mod6))

# Durbin-Watson Test
lmtest::dwtest(residuals(mod6) ~ dat6$Year)


#########################################################







