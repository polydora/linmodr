# ---
# title: "Дисперсионный анализ, часть 2"
# subtitle: "Линейные модели..."
# author: "Марина Варфоломеева, Вадим Хайтов, Анастасия Лянгузова"
# institute: "Кафедра Зоологии беспозвоночных, Биологический факультет, СПбГУ"




library(DAAG)
data("cuckoos")
# Положим данные в переменную с коротким названием, чтобы меньше печатать
eggs <- cuckoos
head(eggs, 3)

# Сократим названия переменных
colnames(eggs) <- c('len', 'br', 'sp', 'id')

levels(eggs$sp)
levels(eggs$sp) <- c("ЛесЗав", "ЛугКон", "БелТряс",
                     "Малин", "ЛесКон", "Крапив")


mod_treatment <- lm(len ~ sp, data = eggs)

library(multcomp)


# Пост хок тест Тьюки
eggs_posthoc <- glht(mod_treatment, linfct = mcp(sp = "Tukey"))

summary(eggs_posthoc)


# Линейные контрасты (проверка избранного подмножества гипотез) ------
# Факультативно. Для вашего личного любопытства.

# 1 способ. Описываем текстом (обратите внимание на знаки).
eggs_contrtext <- glht(mod_treatment,
                       linfct = mcp(sp = c("БелТряс - ЛугКон = 0",
                                           "ЛесЗав - ЛугКон = 0")))
summary(eggs_contrtext)



# 2 способ. Запись матрицы контрастов.
# Зная коэффициенты модели и ее параметризацию,
# мы можем записать уравнения для каждой из сравниваемых групп.
# Уравнения ниже записаны чуть иначе, чем мы привыкли:
# в слагаемых сначала значения предикторов,
# потом коэффициенты, т.е. x_i * b_i).
# Коэффициенты модели:
round(coef(mod_treatment), 1)
# Уравнения для длины яиц
# в гнездах белой трясогузки:
# Y_БелТряс = 1 * b_0       + 0 * b_ЛугКон + 0 * b_Малин +
#           + 1 * b_БелТряс + 0 * b_ЛесКон + 0 * b_ЛесЗав
# в гнездах лугового конька:
# Y_ЛугКон  = 1 * b_0       + 1 * b_ЛугКон + 0 * b_Малин +
#           + 0 * b_БелТряс + 0 * b_ЛесКон + 0 * b_ЛесЗав
# в гнездах лесной завирушки:
# Y_ЛесЗав  = 1 * b_0       + 0 * b_ЛугКон + 0 * b_Малин +
#           + 0 * b_БелТряс + 0 * b_ЛесКон + 1 * b_ЛесЗав
# Чтобы сравнить две какие-то группы, нужно почленно вычесть
# соответствующие уравнения. Например:
# БелТряс - ЛугКон = 0 * b_0       - 1 * b_ЛугКон + 0 * b_Малин +
#                  + 1 * b_БелТряс + 0 * b_ЛесКон + 0 * b_ЛесЗав
# Получившуюся разницу используют для создания матрицы контрастов.
# (Аналогично для ЛесЗав - ЛугКон).
contr <- rbind("БелТряс - ЛугКон" = c(0, -1, 0, 1, 0, 0),
               "ЛесЗав - ЛугКон" = c(0, -1, 0, 0, 0, 1))
contr
eggs_contrmat <- glht(mod_treatment, linfct = contr)
summary(eggs_contrmat)

## Разные параметризации линейных моделей



mod_sum <- lm(len ~ sp, data = eggs, contrasts = list(sp = contr.sum))





# ## Пример: Пингвины ########################
#
# Измерения особей пингвинов из рода Pygoscelis лежат в датасете `penguins` в пакете `palmerpenguins`. Исходные данные были опубликованы в работе Gorman et al., 2014. Помимо веса и пола животных, датасет содержит информацию об острове, на котором пингвины проживали, и измерения клюва. В анализ мы возьмём только следующие переменные:
#
#
#   Зависимая переменная:
#
#   - `body_mass_g` --- вес в граммах
#
# Факторы:
#
#   - `species` --- вид пингвина
#   - `sex` --- пол пингвина

# ## Открываем данные
library(palmerpenguins)
peng <- as.data.frame(penguins[, c(1, 6, 7)])
str(peng)
# Переименовываем столбцы
colnames(peng) <- c('sp', 'mass', 'sex')

# ## Пропущенные значения
colSums(is.na(peng))
# Удаляем пропущенные
pengs <- peng[complete.cases(peng), ]
colSums(is.na(pengs)) # всё ок

# ## Объемы выборок в группах
table(pengs$sp, pengs$sex)

# ##   Посмотрим на график
library(ggplot2)
theme_set(theme_bw())
gg_mass <- ggplot(data = pengs, aes(x = sp, y = mass, colour = sex)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal)
gg_mass


# # Многофакторный дисперсионный анализ ######################------------>>>

## Переменные-индикаторы для факторов
contr.treatment(levels(pengs$sp))
contr.treatment(levels(pengs$sex))

# # Двухфакторный дисперсионный анализ в параметризации индикаторов
# ## Подбираем линейную модель в параметризации индикаторов (contr.treatment)
mod_treat_pengs <- lm(mass ~ sex * sp, data = pengs)
mod_treat_pengs
model.matrix(mod_treat_pengs)


# # Двухфакторный дисперсионный анализ в параметризации эффектов

## Переменные-эффекты для факторов
contr.sum(levels(pengs$sp))
contr.sum(levels(pengs$sex))

# ## Подбираем линейную модель в параметризации эффектов (contr.sum)
mod_sum_pengs <- lm(mass ~ sex * sp, data = pengs,
                    contrasts = list(sp = 'contr.sum', sex = 'contr.sum'))
coef(mod_sum_pengs)

model.matrix(mod_sum_pengs)

# # Диагностика линейной модели ###########################################

# Данные для анализа остатков
mod_treat_peng_diag <- fortify(mod_treat_pengs) # функция из пакета ggplot2
head(mod_treat_peng_diag, 2)

# ## График расстояния Кука
ggplot(mod_treat_peng_diag, aes(x = 1:nrow(mod_treat_peng_diag), y = .cooksd)) +
  geom_bar(stat = 'identity')

# ## График остатков от предсказанных значений
gg_resid <- ggplot(data = mod_treat_peng_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() + geom_hline(yintercept = 0)
gg_resid

# ## График зависимости остатков от предикторов в модели (и не в модели, если есть)
ggplot(data = mod_treat_peng_diag, aes(x = sex, y = .stdresid, colour = sp)) +
  geom_boxplot() + geom_hline(yintercept = 0)

# ## Квантильный график остатков
library(car)
qqPlot(mod_treat_pengs, id = FALSE) # функция из пакета car


# # Многофакторный дисперсионный анализ в R ##################################

# ## Дисперсионный анализ со II типом сумм квадратов
# При таком способе, сначала тестируется взаимодействие, затем отдельные факторы
# в модели без взаимодействия.
mod_treat_pengs <- lm(mass ~ sex * sp, data = pengs)
library(car)
Anova(mod_treat_pengs, type = "II")

# ## Дисперсионный анализ c III типом сумм квадратов
# При этом способе вначале тестируют взаимодействие, когда все другие факторы
# есть в модели. Затем тестируют факторы, когда все другие факторы и
# взаимодействие есть в модели.
mod_sum_pengs <- lm(mass ~ sex * sp, data = pengs,
                    contrasts = list(sp = 'contr.sum', sex = 'contr.sum'))
Anova(mod_sum_pengs, type = "III")



# # Пост хок тест для взаимодействия факторов ############---->>>---->>>---->>>

# 1. Создаем переменную-взаимодействие
# 2. Подбираем модель без свободного члена
# 3. Делаем пост хок тест для этой модели


# ## Задание 1 ---------------------------------------------------------
# Дополните этот код, чтобы посчитать пост хок тест Тьюки по взаимодействию факторов

# Создаем переменную-взаимодействие
pengs$sex_sp <- interaction(pengs$sex, pengs$sp)
# Подбираем линейную модель без свободного члена
fit_inter <- lm()
# Делаем пост хок тест для этой модели
library()
dat_tukey <- glht(, linfct = mcp( = ))
summary(dat_tukey)



# График предсказаний модели ###########################################

# ## Данные для графика при помощи `predict()`

# У нас два дискретных фактора, поэтому вначале используем `expand.grid()`
MyData_pengs <- expand.grid(sex = levels(pengs$sex),
                            sp = levels(pengs$sp))
MyData_pengs <- data.frame(
  MyData_pengs,
  predict(mod_treat_pengs, newdata = MyData_pengs, interval = 'confidence')
)

MyData_pengs


# ## Задание 2 ---------------------------------------------------
# Создайте MyData вручную для модели в обычной параметризации:
# - предсказанные значения
# - стандартные ошибки
# - верхнюю и нижнюю границы доверительных интервалов

MyData_pengs <- expand.grid(sex = levels(pengs$sp),
                            sp = levels(pengs$sex))
X_pengs <- model.matrix(~ , data = )
betas_pengs <- coef()
MyData_pengs$fit <-
MyData_pengs$se <- (( %*% vcov() %*% t()))
MyData_pengs$lwr <- MyData_pengs$ - 2 * MyData_pengs$
MyData_pengs$upr <- MyData_pengs$ + 2 * MyData_pengs$
MyData_pengs


# ## Приводим график в приличный вид
gg_final <- gg_linep + labs(x = 'Вид', y = 'Масса тела, г') +
  scale_colour_manual(name = '', labels = c('Самки', 'Самцы'),
                      values = c('#FF0091', '#0077FF'))
gg_final

