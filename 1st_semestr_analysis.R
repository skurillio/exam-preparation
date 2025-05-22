
#кросс табуляция
#Применяется при анализе взаимосвязе двух КАТЕГОРИАЛЬНЫХ величин 
# если одна из переменных не является категориальной, то ее необходимо сделать такой
year_cut <- cut(p5v2018$year, 
                breaks = c(1800, 1900, 2000, 2020),
                labels = c("19th century", "20th century", "21th century"))
democ_cut <- cut(p5v2018$democ,
              breaks = c(0,3,6,10),
              labels = c("autocracy", "weak democracy", "strong democracy"))

#сложные интервалы 
library(dplyr)

p5v2018 <- p5v2018 %>% 
  mutate(year_cut_dplyr = case_when(
    year %in% c(1800:1900) ~ "19th century",
    year %in% c(1900:2000) ~ "20th century",
  ))


#можно построить двумя способами 
#Table
x <- table(democ_cut, year_cut)
print(x)

#tab_xtab
library(sjPlot)

tab_xtab(
  var.row = democ_cut,     # Строки
  var.col = year_cut,    # Столбцы
  title = "Distribution of democracy by centuries",
  show.row.prc = TRUE,     # Проценты по строкам
  show.col.prc = TRUE,     # Проценты по столбцам
  show.summary = TRUE      # Добавляет хи-квадрат тест
)


#anova
#Применяется для выявления взаимосвязи КАТЕГОРИАЛЬНОЙ и КОЛИЧЕСТВЕННОЙ переменных
#Например анализ урожайности разных сортов

set.seed(42)
weight_loss <- data.frame(
  diet = rep(c("A", "B", "C"), each = 20),
  loss = c(rnorm(20, mean = 3, sd = 1),  # Группа A
           rnorm(20, mean = 5, sd = 1),  # Группа B
           rnorm(20, mean = 4, sd = 1))  # Группа C
)


#смотрим визуально
boxplot(loss ~ diet, data = weight_loss, 
        main = "Потеря веса по диетам",
        xlab = "Диета", ylab = "Потеря веса (кг)")

model <- aov(loss ~ diet, data = weight_loss) #добавление переменных
summary(model)

#корреляция
#показывает линейную связь двух КОЛИЧЕСТВЕННЫХ переменных
#два стула: Пирсон и Спирмен

#Пирсон используется для нормально распределенных данных
#Спирмен для ненормального распределения/порядковых


set.seed(42)
height <- rnorm(30, mean = 170, sd = 10)
weight <- height * 0.6 + rnorm(30, mean = 0, sd = 5)
data <- data.frame(height, weight)

#проверка нормальности
 x <- p5v2018$democ[p5v2018$democ > 0]
hist(x)

shapiro.test(x[c(1:4999)])

#сама корреляция


cor_pearson <- cor(data$height, data$weight)
cor_pearson  # Пример вывода: 0.72

cor_spearman <- cor(data$height, data$weight, method = "spearman")
cor_spearman  # Пример вывода: 0.75


library(ggplot2)
ggplot(data, aes(x = height, y = weight)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)


#Регрессии
#Установление зависимости 
#Линейная
model <- lm(weight ~ height, data = data)
summary(model)

#Логистическая
logit_model <- glm(purchase ~ age + income, data = data)
summary(logit_model)
