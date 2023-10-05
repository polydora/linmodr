# Пермутационный тест для оценки статистической значимости коэффициента корреляции
library(ggplot2)
library(dplyr)

# Создаем искусственный датасет, содержащий скоррелированные векторы

set.seed(12345)
x = rnorm(100, 170, 5) # Случайные числа

y = 0.2*x - 1 # Вектор, связанный с вектором x

y = y + rnorm(100, 0, 7) # Добавляем шум

df <- data.frame(Trait_1 = x, Trait_2 = y)

ggplot(df, aes(x = Trait_1, y  =Trait_2)) +
  geom_point()


# Выборочный коэффициент корреляции

r_samp <- cor(df$Trait_1, df$Trait_2)

cor.test(df$Trait_1, df$Trait_2) # Параметрический тест (t-test)

### Пермутационный тест ###

# Распределение статистики (r) при условии, что верна нулевая гипотеза, можно сгенерировать из данных
# Если верна нулевая гипотеза, то Trait_1 и Trait_2 независят друг от друга (корреляция равна нулю). Следовательно, перемешивая в случайном порядке элементы вектор Trait_1 и/или Trait_2, мы ничего не испортим


df$Trait_1_perm <- sample(df$Trait_1)

df$Trait_2_perm <- sample(df$Trait_2)

ggplot(df, aes(x = Trait_1_perm, y  =Trait_2_perm)) +
  geom_point()

cor(df$Trait_1_perm, df$Trait_2_perm)


# Повторим пермутации многократно

df_r <- data.frame(r_perm = rep(NA, 10000))

for(i in 1:9999){

  df$Trait_1_perm <- sample(df$Trait_1)
  df$Trait_2_perm <- sample(df$Trait_2)
  df_r$r_perm[i] <- cor(df$Trait_1_perm, df$Trait_2_perm)
}

tail(df_r)

# Последним значением будет наше измеренное значение, но это ведь тоже один из возможных результатов пермутаций


df_r$r_perm[10000] <- r_samp

# Строим частотное распределение пермутационных коэффициентов корреляции
Pl_twoside <-
ggplot(df_r, aes(x = r_perm)) +
  geom_histogram() +
  geom_vline(xintercept = c(r_samp, -r_samp) , color = "blue") +
  geom_vline(xintercept = quantile(x= df_r$r_perm, probs = c(0.025, 0.975)) , color = "red", linetype = 2, linewidth = 1)

Pl_twoside

# Посчитаем долю тех пермутационных коэффициетов (они порождены случайным процессом), которые больше или равны наблюдаемого (Двусторонний тест!)

mean((df_r$r_perm >= r_samp) |  (df_r$r_perm <= -r_samp))

# Односторонний тест

mean(df_r$r_perm >= r_samp)

cor.test(df$Trait_1, df$Trait_2, alternative = "greater") # Параметрический тест (t-test)

Pl_one_side <-
ggplot(df_r, aes(x = r_perm)) +
  geom_histogram() +
  geom_vline(xintercept = c(r_samp) , color = "blue") +
  geom_vline(xintercept = quantile(x= df_r$r_perm, probs = c(0.95)) , color = "red", linetype = 2, linewidth = 1)

Pl_one_side


library(cowplot)

plot_grid(Pl_twoside, Pl_one_side, nrow = 2)
