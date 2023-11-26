# title       : "Смешанные линейные модели для счетных данных"
# subtitle    : "Линейные модели..."
# author: Марина Варфоломеева, Вадим Хайтов

# ## От чего зависит призывный крик совят? ###############################
#
# 27 семейств сов в западной Швейцарии наблюдали с
# июня по август 1997. В день наблюдений совятам
# либо давали дополнительную подкормку (сытые),
# либо забирали остатки пищи из гнезда (голодные).
# Оба варианта манипуляций использовали в каждом
# из гнезд в случайном порядке. С 21:30 до 5:30
# утра записывали звуки и видео.
# Данные из Roulin & Bersier 2007, пример из кн. Zuur et al., 2007
#
# -`SiblingNegotiation` - число звуков в течение 15 минут до прибытия родителя
# - `FoodTreatment` - тритмент (сытые или голодные)
# - `SexParent` - пол родителя
# - `FoodTreatment x SexParent`
# - `ArrivalTime` - время прибытия родителя
# - `ArrivalTime х SexParent`
# - `Nest` - гнездо

# Roulin, A. and Bersier, L.F., 2007. Nestling
# barn owls beg more intensely in the presence of
# their mother than in the presence of their
# father. Animal Behaviour, 74(4), pp.1099-1106.


# ## Знакомство с данными ############################################3
Owls <- read.delim("data/Roulin_Bersier_2007_Owls.csv")
str(Owls)

length(unique(Owls$Nest))

# SiblingNegotiation - число криков совят - заменим на более короткое название
Owls$NCalls <- Owls$SiblingNegotiation

# Число пропущенных значений
sum(!complete.cases(Owls))


# ## Есть ли выбросы?
library(ggplot2);

library(cowplot);

theme_set(theme_bw())

gg_dot <- ggplot(Owls, aes(y = 1:nrow(Owls))) +
  geom_point(colour = "steelblue")

plot_grid(gg_dot + aes(x = NCalls),
          gg_dot + aes(x = ArrivalTime), nrow = 1)

# ## Различаются ли гнезда?
ggplot(Owls, aes(x = Nest, y = NCalls)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ## Сколько наблюдений в каждом гнезде?
table(Owls$Nest)

Owls$BroodSize

gg_dot + aes(x= BroodSize)


# ## Отклик --- счетная переменная
ggplot(Owls, aes(x = NCalls)) +
  geom_histogram(binwidth = 1, fill = "steelblue", colour = "black")

mean(Owls$NCalls == 0) # доля нулей


# ## Какого размера выводки в гнездах?
range(Owls$BroodSize)

ggplot(Owls, aes(x = Nest, y = BroodSize)) +
  stat_summary(geom = "bar", fun.y = mean, fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# ## Может быть есть взаимодействие?
ggplot(Owls) +
  stat_summary(aes(x = FoodTreatment, y = NCalls, colour = SexParent),
               fun.data = "mean_cl_boot", position = position_dodge(width = 0.2))


# ## Когда орут птенцы?
ggplot(Owls, aes(x = ArrivalTime, y = NCalls,
                 colour = FoodTreatment, fill = FoodTreatment)) +
  geom_bar(stat = "identity") +
  facet_grid(FoodTreatment + SexParent ~ Nest) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))


# ## Коллинеарность
M0 <- lm(NCalls ~ SexParent + FoodTreatment + ArrivalTime, data = Owls)
library(car)
vif(M0)




# # Смешанная линейная модель с пуассоновским распределением остатков ########


library(lme4)

M1 <- glmer(NCalls ~ SexParent * FoodTreatment +
              SexParent * ArrivalTime +
              offset(logBroodSize) + (1 | Nest),
            family = "poisson", data = Owls)

# Смешанная модель с распределением Пуассона не
# сходится. Один из возможных вариантов выхода -
# стандартизация предикторов
Owls$ArrivalTime_std <- (Owls$ArrivalTime - mean(Owls$ArrivalTime)) /  sd(Owls$ArrivalTime)

Owls$ArrivalTime_std <- scale(Owls$ArrivalTime)

M1 <- glmer(NCalls ~ SexParent * FoodTreatment +
              SexParent * ArrivalTime_std +
              offset(logBroodSize) + (1 | Nest),
            family = "poisson", data = Owls)



# ## Задание 1 ----------------------------------
#
# Проверьте модель M1 на избыточность дисперсии
### Подсказка:
# Показатель сверхдисперсии --- это соотношение суммы квадратов Пирсоновских
# остатков и числа степеней свободы.
# Поскольку сумма квадратов Пирсоновских остатков подчиняется хи-квадрат
# распределению, можно его использовать для проверки статистической значимости
# отклонений.


R_sum <- sum(residuals(M1, type = "pearson")^2)


p <- length(fixef(M1)) + 1

N <- nrow(Owls)

df <- N - p

R_sum/df

pchisq(R_sum, df = df, lower.tail = FALSE)


overdisp_fun <- function(model) {
  rdf <- df.residual(model)  # Число степеней свободы N - p
  if (any(class(model) == 'negbin')) rdf <- rdf - 1 ## учитываем k в NegBin GLMM
  rp <- residuals(model,type='pearson') # Пирсоновские остатки
  Pearson.chisq <- sum(rp^2) # Сумма квадратов остатков, подчиняется Хи-квадрат распределению
  prat <- Pearson.chisq/rdf  # Отношение суммы квадратов остатков к числу степеней свободы
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE) # Уровень значимости
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)        # Вывод результатов
}

overdisp_fun(M1)



# ## График остатков
M1_diag <- data.frame(Owls,
                      .fitted = predict(M1, type = "response"),
                      .pears_resid = residuals(M1, type = "pearson"))




gg_resid <- ggplot(M1_diag, aes(x = .fitted, y = .pears_resid,
                       colour = FoodTreatment)) +
  geom_point() +
  facet_grid(SexParent ~ FoodTreatment)
gg_resid + geom_smooth() + geom_hline(yintercept = 0)


# ## Есть ли еще какие-то паттерны в остатках?
gg_resid %+% aes(x = ArrivalTime) + geom_smooth(method = "loess")+
  geom_hline(yintercept = 0)


# ## Проверяем, есть ли нелинейный паттерн в остатках {.smaller .columns-2}

library(mgcv)

nonlin1 <- gam(.pears_resid ~ s(ArrivalTime),
               data = M1_diag)
summary(nonlin1)
plot(nonlin1)
abline(h = 0, lty = 2)


# # Смешанная линейная модель с отрицательным биномиальным распределением остатков ######
# ## У нас была сверхдисперсия. Пробуем NB GLMM

M2 <- glmer.nb(NCalls ~ SexParent * FoodTreatment +
                 SexParent * ArrivalTime_std +
                 offset(logBroodSize) + (1 | Nest),
               data = Owls)

# # Если эта модель вдруг не сходится, есть обходной маневр. Можно попробовать заранее определить k  при помощи внутренней функции. В lme4 параметр k называется theta
th <- lme4:::est_theta(M1)
M2.1 <- update(M1, family = negative.binomial(theta=th))
bind_rows(fixef(M2), fixef(M2.1))

# ## Задание 2 -----------------------------------------------------
#
# Проверьте модель с отрицательным биномиальным распределением отклика
#
# - на избыточность дисперсии
# - наличие паттернов в остатках
# - нелинейность паттернов в остатках

overdisp_fun(M2)



M2_diag <- data.frame(Owls,
                      .fitted = predict(M2, type = "response"),
                      .pears_resid = residuals(M2, type = "pearson"))




gg_resid <- ggplot(M2_diag, aes(x = .fitted, y = .pears_resid,
                                colour = FoodTreatment)) +
  geom_point() +
  facet_grid(SexParent ~ FoodTreatment)

gg_resid + geom_smooth() + geom_hline(yintercept = 0)


# ## Есть ли еще какие-то паттерны в остатках?
gg_resid %+% aes(x = ArrivalTime) + geom_smooth(method = "loess")+
  geom_hline(yintercept = 0)


nonlin2 <- gam(.pears_resid ~ s(ArrivalTime),
               data = M2_diag)
summary(nonlin2)
plot(nonlin2)
abline(h = 0, lty = 2)





# # Подбор оптимальной модели ####################################
summary(M2)

# ## Задание 3 -----------------------------------------------

# Попробуйте упростить модель M2

drop1(M2, test = "Chi")

M3 <- update(M2, .~.-SexParent:ArrivalTime_std)

drop1(M3, test = "Chi")


M4 <- update(M3, .~.-SexParent:FoodTreatment)

drop1(M4, test = "Chi")

M5 <- update(M4, .~.-SexParent)
drop1(M5)


# ## Модель изменилась. Нужно повторить диагностику
#
# Избыточность дисперсии (Overdispersion)
R_M5 <- resid(M5, type = "pearson") # Пирсоновские остатки
N <- nrow(Owls) # Объем выборки
p <- length(fixef(M5)) + 1 + 1  # Число параметров в модели
df <- (N - p) # число степенейсвободы
overdispersion <- sum(R_M5^2) /df  # во сколько раз var(y) > E(y)
overdispersion

overdisp_fun(M5)



# ## Диагностика отр. биномиальной модели
M5_diag <- data.frame(Owls,
                      .fitted <- predict(M5, type = "response"),
                      .pears_resid <- residuals(M5, type = "pearson"))

gg_resid <- ggplot(M5_diag, aes(x = .fitted, y = .pears_resid,
                                colour = FoodTreatment)) +
  geom_point() +
  facet_grid(SexParent ~ FoodTreatment)
gg_resid


# ## Есть ли еще какие-то паттерны в остатках?
gg_resid %+% aes(x = ArrivalTime) + geom_smooth(method = "loess") + geom_hline(yintercept = 0)

# ## Проверяем, есть ли нелинейные паттерны
nonlin5 <- gam(.pears_resid ~ s(ArrivalTime), data = M5_diag)
summary(nonlin5)
plot(nonlin5)
abline(h = 0)

# > - Совершенно точно нужен GAMM
# > - Но мы продолжим в учебных целях


# # Представление результатов ############################

summary(M5)

# ## Готовим данные для графика модели
library(dplyr)

NewData <- Owls %>% group_by(FoodTreatment) %>%
  do(data.frame(ArrivalTime_std = seq(min(.$ArrivalTime_std),
                                       max(.$ArrivalTime_std),
                                       length = 100)))

NewData$ArrivalTime <- NewData$ArrivalTime_std * sd(Owls$ArrivalTime) +
  mean(Owls$ArrivalTime)

# ## Предсказания и ошибки
# Модельная матрица

X <- model.matrix(~ FoodTreatment + ArrivalTime_std, data = NewData)

# К предсказанным значениям нужно прибавить оффсет.
# Мы будем делать предсказания для среднего размера выводка.
# В масштабе функции связи

NewData$fit_eta <- X %*% fixef(M5) + log(mean(Owls$BroodSize))


NewData$SE_eta <- sqrt(diag(X %*% vcov(M5) %*% t(X)))


# В масштабе отклика

NewData$fit_mu <- exp(NewData$fit_eta)
NewData$lwr <- exp(NewData$fit_eta - 2 * NewData$SE_eta)
NewData$upr <- exp(NewData$fit_eta + 2 * NewData$SE_eta)



# ## График предсказанных значений
ggplot() +
  geom_point(data = Owls, aes(x = ArrivalTime, y = NCalls), colour = "steelblue") +
  geom_ribbon(data = NewData, aes(x = ArrivalTime,  ymax = upr,  ymin = lwr), alpha = 0.3) +
  geom_line(data = NewData,  aes(x = ArrivalTime, y = fit_mu, group = FoodTreatment)) +
  facet_wrap(~ FoodTreatment)


### GAM

set.seed(12345)
x <- seq(0, pi * 2, 0.1)
sin_x <- sin(x)

qplot(x, y = sin_x)


y <- sin_x + rnorm(n = length(x), mean = 0, sd = sd(sin_x / 2))


df <- data.frame(x, y)

df$mu <- sin_x

ggplot(df, aes(x, y))+
  geom_point() +
  geom_smooth(method = "lm")


Pl_init <-
  ggplot(df, aes(x, y))+
  geom_point() +
  geom_line(aes(y = mu), color = "blue") +
  geom_smooth()
Pl_init


