# ---
# title: "Тестирование статистических гипотез"
# author: "Марина Варфоломеева, Юта Тамберг, Вадим Хайтов"

# # Центральная предельная теорема (демонстрация) #########################

library(ggplot2)
theme_set(theme_bw())
data("diamonds")

# 1. Среднее в генеральной совокупности -----------------------------------

mu_population <- mean(diamonds$price)

# График распределения цен алмазов в генеральной совокупности.
# Отмечено среднее
gg_population <- ggplot(data = diamonds, aes(x = price)) +
  geom_histogram(fill = 'lightskyblue1', colour = 'black') +
  geom_vline(aes(xintercept = mu_population), colour = 'red', size = 2)
gg_population

# 2. Выборочное среднее ---------------------------------------------------

# Функция, которая берет выборку из вектора population размером size,
# затем строит график распределения значений в выборке.
# Отмечены среднее в генеральной совокупности и среднее в выборке.
# (NA предварительно удалены из вектора population)
gg_sample_hist <- function(population, size, mu) {
  mu_population <- mean(na.omit(population))
  id <- sample(x = length(na.omit(population)), size = size)
  subsample <- population[id]
  my_mean <- mean(subsample)
  my_dat <- data.frame(subsamp = subsample)
  ggplot(data = my_dat, aes(x = subsamp)) +
    geom_histogram(binwidth = 50, fill = 'grey40', colour = 'grey40') +
    geom_vline(aes(xintercept = mu_population), colour = 'red', size = 2) +
    geom_vline(aes(xintercept = my_mean), colour = 'yellow3', size = 2) +
    coord_cartesian(xlim = c(-1, max(subsample))) +
    annotate('text', x = Inf, y = Inf, hjust = 1, vjust = 1.5,
             label = paste('n ==', size),
             parse = T, size = 3.5) +
    annotate('text', x = Inf, y = Inf, hjust = 1, vjust = 3.5,
             label = paste('bar(x) ==', format(my_mean, nsmall = 2, digits = 2)),
             parse = T, size = 3.5, colour = "yellow4") +
    annotate('text', x = Inf, y = Inf, hjust = 1, vjust = 4.5,
             label = paste('mu ==', mu_population),
             parse = T, size = 3.5, colour = "red")
}

# График распределения цен алмазов в выборке
set.seed(24)
gg_sample_hist(population = diamonds$price, size = 20)

# 3. Распределение выборочных средних -------------------------------------

# Функция, которая берет выборку объемом size из вектора x
# и возвращает ее среднее значение.
# (NA предварительно удалены из вектора x)
sample_mean <- function(x, size){
  x <- na.omit(x)
  id <- sample(x = length(x), size)
  my_mean <- mean(x[id])
  return(my_mean)
}

# Функция, которая берет несколько (n_samples) выборок
# объемом size из вектора x и строит распределение выборочных средних.
# Отмечено среднее в генеральной совокупности (красная линия)
# и среднее значение в распределении выборочных средних.
gg_sample_means <- function(population, n_samples, size){
  # Считаем средние значения для большого числа выборок
  mu_population <- mean(na.omit(population))
  my_means <- replicate(n = n_samples, expr = sample_mean(population, size))
  mean_of_means <- mean(my_means)
  sd_of_means <- sd(my_means)
  my_dat <- data.frame(means = my_means)
  ggplot(data = my_dat, aes(x = means)) +
    geom_histogram(binwidth = 4, fill = 'yellow3', alpha = 0.3, color = 'black') +
    geom_vline(xintercept = mu_population, colour = 'red', size = 3) +
    geom_vline(xintercept = mean_of_means, colour = 'gold1', size = 1.5) +
    coord_cartesian(xlim = c(-1, max(my_means))) +
    annotate('text', x = Inf, y = Inf, hjust = 1, vjust = 1.5,
             label = paste('n ==', size),
             parse = T, size = 3.5) +
    annotate('text', x = Inf, y = Inf, hjust = 1, vjust = 3.5,
             label = paste('bar(x) ==', format(mean_of_means, nsmall = 2, digits = 2)),
             parse = T, size = 3.5, colour = "yellow4") +
    annotate('text', x = Inf, y = Inf, hjust = 1, vjust = 4.5,
             label = paste('mu ==', mu_population), colour = "red",
             parse = T, size = 3.5) +
    annotate('text', x = Inf, y = Inf, hjust = 1, vjust = 8.5,
             label = paste('~~~sd ==', format(sd_of_means, nsmall = 2, digits = 2)),
             parse = T, size = 3.5, colour = "yellow4")
}

# График распределения выборочных средних
gg_sample_means(population = diamonds$price, n_samples = 500, size = 3)



# # Доверительный интервал ##########################################

load("data/population.RData")

library(dplyr)
set.seed(14934)
# Генеральная совокупность для симуляции
x <- population

mu <- round(mean(x), 0)
# Функция, которая берет выборку объемом sample_size из вектора x и возвращает ее среднее значение и доверительный интервал (по t)
sample_mean_ci <- function(x, size){
  id <- sample(x = length(x), size)
  my_mean <- mean(x[id])
  ci <- my_mean + qt(p = c(0.025, 0.975), df = size - 1)
  res <- c(my_mean, ci)
  names(res) <- c('sample_mean', 'lower', 'upper')
  return(res)
}


n_samples <- 100
sample_size <- 20

means_ci <- replicate(n = n_samples, expr = sample_mean_ci(x, size = sample_size))

dfr_means <- data.frame(t(means_ci)) %>%
  mutate(interval = 1:n_samples,
         inside = mu >= lower & mu <= upper,
         inside = factor(inside, levels = c(TRUE, FALSE), labels = c('Да', 'Нет')))

perc <- round(mean(dfr_means$inside == 'Да') * 100, 1)

gg_many_lims <- ggplot(data = dfr_means) +
  geom_segment(aes(x = interval, y = lower, xend = interval, yend = upper, colour = inside)) +
  geom_hline(yintercept = mean(x), colour = 'red', size = 1) +
  scale_y_continuous('', breaks = mu, labels = expression(mu)) +
  labs(x = 'Порядковый номер интервала в симуляции', y = 'x',
       color = 'Включает ли\nинтервал\nистинное\nсреднее \nзначение?') +
  coord_flip() +
  scale_x_reverse()

gg_many_lims + scale_color_manual(values = c("red", "blue"))






# ## Расчет и изображение доверительного интервала в R ##############

# цена бриллиантов хорошего качества огранки
good <- diamonds$price[diamonds$cut == "Good"]

.mean <- mean(good)                  # выборочное среднее
.n <- length(good)                   # объем выборки
SE <- sd(good)/ sqrt(.n)             # стандартная ошибка
t_crit <- qt(p = 0.975, df = .n - 1) # критич. зн. t для данного n и p = 0.95
err <- t_crit * SE                   # предел для границ доверительного интервала
err
# Границы доверительного интервала
.mean - err
.mean + err


# ## Строим доверительные интервалы в ggplot

ggplot(data = diamonds, aes(x = cut, y = price)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal)

### Задание 1 ------------------------------------------------------------
#
# Посчитайте среднюю цену и доверительный интервал для бриллиантов с такими свойствами:
#
# - качество огранки (`cut`) идеальное (`Ideal`)
# - прозрачность (`clarity`) наивысшая (`IF`)

# нужные бриллианты
group <- diamonds$price[ ]

.mean <- mean(group)                  # выборочное среднее
.n <- length(group)                   # объем выборки
SE <- sd(group)/ sqrt(.n)             # стандартная ошибка
t_crit <- qt() # критич. зн. t
err <- t_crit * SE                   # предел погрешности
# Границы доверительного интервала
.mean + c(-1, 1) * err

# Постройте один общий график средней цены с доверительными интервалами
# для бриллиантов разного качества огранки, и прозрачности.

ggplot(data = , aes(x = , y = ,  = clarity)) +
   (geom = '', fun.data = ) +
  facet_ (~ clarity)


## Статистика по группам при помощи пакета `dplyr`

library(dplyr)



## Одновыборочный t-тест ################################################
#
### Размер кладки черепах (вымышленный пример).
#
# Представьте, что в одной статье сказано, что средняя плодовитость черепах определенного вида --- mu яиц в кладке. У вас есть выборка черепах, где средняя плодовитость другая.
# Отличается ли реальная плодовитость в обследованной вами популяции черепах от того, что указано в статье?

mu <- 8
X <- c(10, 11, 10, 7, 8, 7, 9, 8, 11, 11, 12, 8, 6, 7, 10, 11, 9,
       10, 7, 11, 11, 12, 11, 9, 4, 12, 9, 6, 9, 6, 9, 7, 8, 10, 9)
(n <- length(X)) # всего черепах
(x <- mean(X)) # средний размер кладки в выборке
(s <- sd(X)) # стандартное отклонение в выборке


### Задание 2 ------------------------------------------------------------
# Проверьте условия применимости t-теста. Вычислите t и p.
library()
qqPlot()
# t = (наблюдаемое - ожидаемое) / (станд.ошибка наблюдаемого)
t <-
# число степеней свободы
df <-
# уровень значимости
p <-


# # Двухвыборочный t-тест ####################################################


# ## Пример: Гормоны и артериальная гипертензия
#
# Синдром Кушинга --- это нарушения уровня артериального давления, вызванные гиперсекрецией кортизола надпочечниками.
#
# В датасете `Cushings` (пакет `MASS`) записаны данные о секреции двух метаболитов при разных типах синдрома (данные из кн. Aitchison, Dunsmore, 1975).
#
# - `Tetrahydrocortisone` --- секреция тетрагидрокортизона с мочой (мг/сут.)
# - `Pregnanetriol` --- секреция прегнантриола с мочой (мг/сут.)
# - `Type` --- тип синдрома:
#   - `a` --- аденома
# - `b` --- двусторонняя гиперплазия
# - `c` --- карцинома
# - `u` --- не известно
#
# Различается ли секреция тетрагидрокортизона при аденома и двусторонней гиперплазии надпочечников?
library(MASS)
library(car)
data("Cushings")

head(Cushings)
str(Cushings)

colSums(is.na(Cushings))

table(Cushings$Type)

qqPlot(Cushings$Tetrahydrocortisone[Cushings$Type == 'a'])
qqPlot(Cushings$Tetrahydrocortisone[Cushings$Type == 'b'])

tt <- t.test(x = Cushings$Tetrahydrocortisone[Cushings$Type == 'a'],
             y = Cushings$Tetrahydrocortisone[Cushings$Type == 'b'])
tt



# Задание 3------------------------------------------------------------------
# Перепишите вызов функции t.test с использованием
# другого шаблона вызова (с использованием формулы).
tt <- t.test(formula =  ~ , data = Cushings,
             subset = Cushings$Type  c('a', 'b'))
tt



# Задание 4------------------------------------------------------------------
# Посмотрите структуру результатов (`tt`) при помощи
# функции `str()` и извлеките из них:
# - степени свободы
# - уровень значимости
# - значение t-критерия





#############################################
# Пермутационные оценки значимости различий #
#############################################

a <- Cushings$Tetrahydrocortisone[Cushings$Type == 'a']
b <- Cushings$Tetrahydrocortisone[Cushings$Type == 'b']

mean_a <- mean(a)
sd_a <- sd(a)
n_a <- length(a)
se_a <- sd_a/sqrt(n_a)

mean_b <- mean(b)
sd_b <- sd(b)
n_b <- length(b)
se_b <- sd_b/sqrt(n_b)

t_emp <- abs(mean_a - mean_b)/sqrt(se_a^2 + se_b^2)


# Пермутируем векторы

  all_data <- sample(c(a, b))
  a_perm <- all_data[1:n_a]
  b_perm <- all_data[(n_a +1):length(all_data)]

  mean_a <- mean(a_perm)
  sd_a <- sd(a_perm)
  n_a <- length(a_perm)
  se_a <- sd_a/sqrt(n_a)

  mean_b <- mean(b_perm)
  sd_b <- sd(b_perm)
  n_b <- length(b_perm)
  se_b <- sd_b/sqrt(n_b)

  t_perm <- abs(mean_a - mean_b)/sqrt(se_a^2 + se_b^2)

  # Повторим пермутации многократно

  t_perm_vector <- rep(NA, 1000)

  for(i in 1:999){
    all_data <- sample(c(a, b))
    a_perm <- all_data[1:n_a]
    b_perm <- all_data[(n_a +1):length(all_data)]

    mean_a <- mean(a_perm)
    sd_a <- sd(a_perm)
    n_a <- length(a_perm)
    se_a <- sd_a/sqrt(n_a)

    mean_b <- mean(b_perm)
    sd_b <- sd(b_perm)
    n_b <- length(b_perm)
    se_b <- sd_b/sqrt(n_b)

    t_perm <- abs(mean_a - mean_b)/sqrt(se_a^2 + se_b^2)

    t_perm_vector[i] <-  t_perm
  }


  t_perm_vector[1000] <- t_emp


  mean(t_perm_vector >= t_emp) # Доля пермутационных статистик которые больше или равны выборочной статистике

tt #Результаты параметрического теста




# ## Задание 5 ----------------------------------------------------------------

# Файл `aml.csv` содержит данные о влиянии регулярной химиотерапии
# на продолжительность ремиссии.
# Прочитаем эти данные
rem <- read.csv(file = "data/aml.csv", header = TRUE)
str(rem)
# - В переменной `time` представлена продолжительность ремиссии в днях.
# - `group` указывает, к какой экспериментальной группе принадлежал пациент.
# В группе 1 проводилась регулярная химиотерапия, в группе 2 - нет.
#
# - Сравните эти группы с помощью t-теста.
# - Постройте график со средними и доверительными интервалами
# для продолжительности ремиссии в этих группах.




# ## Парный t-test
# Разведочный анализ
data(sleep)
head(sleep)
# Объём выборки
table(sleep$group)
# Проверка на нормальность распределения
qqPlot(sleep$extra[sleep$group == 1])
qqPlot(sleep$extra[sleep$group == 2])


t.test(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2], paired = TRUE)

tt <- t.test(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2], paired = TRUE)
tt

# Если не учесть зависимость между группами, приходим к неверному выводу
t.test(formula = extra ~ group, data = sleep)
