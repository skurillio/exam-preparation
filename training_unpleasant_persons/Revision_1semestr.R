#дизайн работы
#библиотеки
#загружаем таблицы
#очистка данных
#объединение
#визуализация
#моделирование
#валидация

library(tidyverse)
library(readxl)
library(sjPlot)
library(car)
library(corrplot)
library(lmtest)

#Кросстабуляция - это не метод (ХИ-КВАДРАТ!!!). Применяется для:
#1) Проверки наличия связи между ДВУМЯ КАТЕГОРИАЛЬНЫМИ переменными.
#2) Aнализа распределения данных в разрезе групп (например, пола и уровня образования).
#Если переменные количественные, то их надо будет нарезать

#polityV 
polity <- read_excel("~/Desktop/Глава_3/p5v2018.xls")

polity <- polity %>%
  mutate(year_crosstab = case_when(
    year %in% 1800:1900 ~ "19th century",
    year %in% 1900:2000 ~ "20th century",
  ),
  democracy_crosstab = case_when(
    democ %in% 0:3 ~ "Autocracy",
    democ %in% 4:6 ~ "Weak democracy",
    democ %in% 7:10 ~ "Strong democracy",
  ))

#вопрос от сатик 
{age <- c("twenty", "twenty", "nineteen")
name <- c("Kirill", "Artem", "Satik")
vopros_satik <- data.frame(age, name)

vopros_satik <- vopros_satik %>%
  mutate(age = case_when(
    age == "twenty" ~ 20,
    age == "nineteen" ~ 19
  ))}

#Моделирование + визуализация
tab_xtab(
  var.row = polity$democracy_crosstab,     # Строки
  var.col = polity$year_crosstab,    # Столбцы
  title = "Distribution of democracy by centuries", #Название
  show.row.prc = TRUE,     # Проценты по строкам
  show.col.prc = TRUE,     # Проценты по столбцам
  show.summary = TRUE      # Добавляет хи-квадрат тест
)

#Интерпритация
#1) хи-квадрат + p-value - показывает есть ли статистическая связь (просто факт), различие
#хи квадрат < 5.9 связь незначима 5.9 < хи квадрат < 10 - значимая связь
# хи квадрат >= 10 сильная связь

#2) тест крамера (0-1)
#0-0.1 практически нет связи
#0.1-0.3 слабая связь
#0.3-0.5 умеренная связь
# > 0.5 сильная связь

#ANOVA для сравнения средних значений между тремя и более группами.
#То есть категориальная и количественная переменные

#Генерация данных (диеты)
set.seed(42)
weight_loss <- data.frame(
  diet = rep(c("A", "B", "C"), each = 20),
  loss = c(rnorm(20, mean = 3, sd = 1),  # Группа A
           rnorm(20, mean = 5, sd = 1),  # Группа B
           rnorm(20, mean = 4, sd = 1))  # Группа C
)

#Визуалиция
boxplot(loss ~ diet, data = weight_loss,
        main = "Потеря веса по диетам",
        xlab = "Диета", ylab = "Потеря веса (кг)")

#Моделирование
anova_model <- aov(loss ~ diet, data = weight_loss)
summary(anova_model)

#Интерпритация 1) p-value 2) F-value (показывает различия дисперсий между группами)
# F-value около 1 - различий нет
# F-value > 1 - различия
# F-value < 1 (около нуля) - ошибка в данных

#Валидация
shapiro.test(anova_model$residuals)
hist(anova_model$residuals)
leveneTest(anova_model)

#Такая же логика как и с БП тестом p-value <0.05 - гетероскедатичность
#p-value > 0.05 - гомоскедатичгость

#Корреляция
#показывает линейную связь двух КОЛИЧЕСТВЕННЫХ переменных
#два стула: Пирсон и Спирмен

#Пирсон используется для нормально распределенных данных
#Спирмен для ненормального распределения/порядковых

set.seed(42)
height <- rnorm(30, mean = 170, sd = 10)
weight <- height * 0.6 + rnorm(30, mean = 0, sd = 5)
data <- data.frame(height, weight)

shapiro.test(height)
hist(height)
shapiro.test(weight)
hist(weight)

#для нормальных
(cor_pearson <- cor(data$height, data$weight, method = "pearson"))
#для ненормальных
(cor_spearman <- cor(data$height, data$weight, method = "spearman"))

#валидация оценка линейной связи
cor.test(data$height, data$weight, method = "pearson")

#пороги 20, дальше 50 и 80

#Визуализация
ggplot(data, aes(x = height, y = weight)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "pink")

#Тепловая карта (heatmap)
cor_matrix <- cor(data)
corrplot(cor_matrix, method = "number", type = "upper")

#Регрессия
model_linear <- lm(weight ~ height, data = data)
summary(model_linear)

#Валидация
shapiro.test(model_linear$residuals)
bptest(model_linear)

ggplot(data, aes(x = height, y = weight)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, color = "pink")

#qqplot - как близко остатки расположены к нормально распределению
plot(model_linear, which = 2)


#Логистическая регрессия - бинарную

#ГЕНЕРАЦИЯ
# Устанавливаем seed для воспроизводимости
set.seed(123)
# Создаем простые данные
n <- 100  # Количество наблюдений
age <- round(runif(n, 18, 65))  # Возраст от 18 до 65 лет
# Логит-модель: вероятность трудоустройства увеличивается с возрастом
log_odds <- -4 + 0.1 * age  # Базовые коэффициенты
prob <- plogis(log_odds)     # Преобразуем в вероятность (0-1)
# Генерируем бинарный исход (0 - не принят, 1 - принят)
employed <- rbinom(n, 1, prob)
# Собираем в простой датафрейм
job_data <- data.frame(
  age = age,
  employed = factor(employed, levels = c(0, 1), labels = c("Не принят", "Принят"))
)

model_logit <- glm(employed ~ age, data = job_data,
                   family = binomial(link = "logit"))
summary(model_logit)

#валидация
plot(residuals(model_logit, type = "deviance"), 
     main = "График остатков",
     ylab = "Девиансные остатки")
abline(h = 0, col = "red")
#хаотичность разброса остатков возле 0