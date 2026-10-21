# ---
# title: "Краткое введение в мир матричной алгебры"
# subtitle: "Линейные модели..."
# author: "Вадим Хайтов, Марина Варфоломеева, Арсений Гублер"

#Создаем матрицу из вектора
# Задание: постройте марицу 4х3 из чисел от 1 до 12

matrix(data =   )
matrix(, nrow = , ncol = )

matrix( nrow = , ncol = , byrow =  )


# Единичная матрица





A <- matrix(1:12, ncol = 3)
A

t(A)


A + 4

A + (A+4)

A + t(A)


B <- t(A)

A + B

# Создаем искусственный датасет для демонстрации сложения матриц
Large <- data.frame(Sp1 = round(rnorm(5, 10, 2)), Sp2 = round(rnorm(5, 10, 3)), Sp3 = round(rnorm(5, 10, 2)))

rownames(Large) <- c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5" )

Small <- data.frame(Sp1 = round(rnorm(5, 50, 5)), Sp2 = round(rnorm(5, 50, 5)), Sp3 = round(rnorm(5, 50, 5)))

rownames(Small) <- c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5" )

Large
Small


Large + Small


A * 4


d <- c(10, 11, 12)

A * d




##Простое произведеине

Rpocessed_portion <- c(1, 1, 1/2, 1/3, 1/4)
Processed_Factor <- 1/Rpocessed_portion

Small * Processed_Factor






#Матричное произведение двух вектров

N <- c(20, 40, 32, 45, 80, 50, 10)
Fert <- c( 0,  0,   1,   2,   2,   0,   0)

N %*% Fert

t(N) %*% Fert

(N) %*% t(Fert)

t(Fert) %*% t(N)


A

B

A %*% B

B %*% A


C <- B[-3, ]

A %*% C

A %*% A



#Матричные произведения




## Демографическая модель с использованием матриц Лесли

T1 <- c(20, 40, 32, 45, 80, 50, 10)
Age <- c("0", "1-10", "11-20", "21-35", "36-45", "46-55", "56-65")
Pop <- data.frame(Age, T1)

Lesl <- matrix(
c( 0,  0,   1,   2,   2,   0,   0,
  0.6, 0,   0,   0,   0,   0,   0,
   0,  0.7, 0,   0,   0,   0,   0,
   0,  0,   0.8, 0,   0,   0,   0,
   0,  0,   0,   0.7, 0,   0,   0,
   0,  0,   0,   0,   0.6, 0,   0,
   0, 0,    0,   0,   0,  0.2, 0  ),
byrow = T,
ncol = 7)

Lesl

Pop$T2 <- as.vector( Lesl %*% (Pop$T1 ))
Pop$T3 <- as.vector( Lesl %*% (Pop$T2 ))
Pop$T4 <- as.vector( Lesl %*% (Pop$T3 ))
Pop$T5 <- as.vector( Lesl %*% (Pop$T4 ))
Pop$T6 <- as.vector( Lesl %*% (Pop$T5 ))
Pop$T7 <- as.vector( Lesl %*% (Pop$T6 ))
Pop$T8 <- as.vector( Lesl %*% (Pop$T7 ))
Pop$T9 <- as.vector( Lesl %*% (Pop$T8 ))
Pop$T10 <- as.vector( Lesl %*% (Pop$T9 ))

library(ggplot2)
library(reshape2)
Pop2 <- melt(Pop)
ggplot(Pop2, aes(x=Age, y = value)) + geom_bar(stat = "identity") + facet_wrap(~variable, ncol = 2, dir = "v")



#Вычисляем матрицу корреляций с помощью матричной алгебры


brain <- read.csv("data/IQ_brain.csv", header = TRUE)

br <- brain[complete.cases(brain), -1]

cor(br)

br <- as.matrix(br)
str(br)

br_scaled <- scale(br) #Стандартизация данных

cor_matrix <-  %*%  /    (     - 1)

cor_matrix

# Проверим
cor(br)

###########################
# Задание
# Используя датасет `iris`, вычислите корреляцию между `Sepal.Length`  и `Sepal.Width`

iris
str(iris)
cor(iris$Sepal.Length, iris$Sepal.Width)

iris_subs <- iris[, c(1,2)]

iris_subs_scaled <- scale(iris_subs)

 %*%  /



###########################

A
B
C

A %*% C

C %*% A


C %*% t(C)

t(C) %*% (C)


t(C %*% A)

t(A) %*% t(C)



det(A %*% t(A))

det(A[-4, ])

B[3,3] <- B[3,3] + 1

X <- (B[, -4])

det(X)

round(solve(X) %*% X)



###########################

# Решение систем линейных уравнений с помощью матричной алгебры


Coef <- matrix(, nrow = , byrow = )

det()

Answer <- matrix(c(2,4,10), ncol = 1)

XYZ <- solve() %*%


1*XYZ[1] + 2*XYZ[2] + 3*XYZ[3]

## Решение системы двух линейных уравнений с двумя неизвестными










###########################

brain <- read.csv("data/IQ_brain.csv", header = TRUE)

brain
M <- lm(PIQ ~ MRINACount, data = brain)


coef(M)

model.matrix(M)

X <-  data.frame(Inntercept = 1, MRINACount = brain$MRINACount)

X <- as.matrix(X)

det(t(X) %*% X)

y <- brain$PIQ

betas <- solve(t(X) %*% X) %*% (t(X) %*% y)

coef(M)


library(ggplot2)
theme_set(theme_bw())
ggplot(brain, aes(x = MRINACount, y = PIQ)) +
  geom_point() +
  geom_smooth(method = "lm")



# Строим линю регрессии вручную

MyData <-
  data.frame(MRINACount = seq(min(brain$MRINACount), max(brain$MRINACount), length.out = 100))

head(MyData)

# Создаем модельную матрицу для искусственного датасета, задающего "линейку" варьирования предиктора

X2 <- model.matrix( ~ MRINACount, data = MyData)

head(X2)

# Создаем веутор предсказанных значений для искусственно созданной "линейки" предиктора

MyData$Predicted <-  X2 %*% betas


Pl <-
ggplot(data = MyData, aes(x = MRINACount, y = Predicted)) +
  geom_line(color = "blue") +
  geom_point(data = brain, aes(y = PIQ))


# Строим вручную доверительный интервал

vcov(M)


RES <- brain$PIQ - X %*% betas

S2 <- sum(RES^2)/(nrow(brain) - length(betas))


VarCov <- S2 * solve(t(X) %*% X)





MyData$se <- sqrt(diag(X2 %*% VarCov  %*% t(X2)))

MyData$CiUp  <- MyData$Predicted + 1.96 *MyData$se

MyData$CiLow  <- MyData$Predicted - 1.96 *MyData$se


Pl +
  geom_line(aes(y = CiLow), linetype = 2) +
  geom_line(aes(y = CiUp), linetype = 2)


# Нанесем то, что строит geom_smooth()
Pl +
  geom_line(aes(y = CiLow), linetype = 2) +
  geom_line(aes(y = CiUp), linetype = 2) +
  geom_smooth(data = brain, aes(y = PIQ), method = "lm")



######################################################
#          Здания для работы по группам              #
######################################################

#
# Визуализируйте линейную модель, описывающую связь между зависимой переменной и предиктором, используя только вычисления, проделанные "вручную". Должен получиться график, отражающий первичные данные, линию регрессии и 95% доверительный интервал регрессии.

#
# Группа № 1
# датасет iris для вида setosa
# переменная отклка: Sepal.Length
# предиктор: Petal.Length




# Группа № 2
# датасет diamonds для группы Good
# переменная отклка: price
# предиктор: carat


# Группа № 3
# датасет cats {MASS} для группы M
# переменная отклка: Hwt
# предиктор: Bwt


# Группа № 4
# датасет crabs {MASS} для группы sp = B & sex = M
# переменная отклка: CW
# предиктор: CL



# Группа № 5
# датасет geyser {MASS}
# переменная отклка: duration
# предиктор: waiting












