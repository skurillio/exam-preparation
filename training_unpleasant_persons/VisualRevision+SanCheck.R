library(tidyverse)
library(ggpubr)
library(gapminder)
library(readxl)
library(lmtest)
library(car)

data(gapminder)

#точечный график
ggplot(gapminder, aes(x = gdpPercap / 1000, y = lifeExp, 
                      color = continent, size = pop)) +
  geom_point(alpha = 0.5) +
  scale_x_log10()+
  labs(title = "ВВП и продолжительность жизни с разбивкой по континентам",
       x = "ВВП на душу (log scale)", y = "Ожидаемая продолжительность жизни",
       color = "Континент", size = "Население")+
  theme_bw()

#График 
gapminder %>%
  group_by(year, continent) %>%
  summarise(avg_lifeExp = mean(lifeExp)) %>%
  ggplot(aes(x = year, y = avg_lifeExp, color = continent)) +
  geom_line(size = 1.5) +
  geom_point(size = 5) +
  geom_point(size = 3, color = "white") +
  labs(title = "Динамика средней продолжительности жизни",
       x = "Год", y = "Средняя продолжительность жизни",
       color = "Континент") +
  theme_minimal()


#столбчатая диаграмма
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(total_gdp = sum(gdpPercap * pop)) %>%
  ggplot(aes(x = continent, y = total_gdp, fill = continent)) +
  geom_col() +
  labs(title = "Общий ВВП по континентам (2007 год)",
       x = "Континент", y = "Общий ВВП (USD)") +
  scale_y_continuous(labels = scales::dollar) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 45))

#Гистограмма
ggplot(gapminder, aes(x = lifeExp)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black")+
  labs(title = "Распределение продолжительности жизни",
       x = "Ожидаемая продолжительность жизни", y = "Количество стран") +
  facet_wrap(~country) +
  theme_light()

#Ящик с усами
ggplot(gapminder, aes(x = continent, y = gdpPercap, fill = continent)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Распределение ВВП на душу по континентам",
       x = "Континент", y = "ВВП на душу (log scale)") +
  theme_minimal()

x <- c(1,2,3,5,6,100)

(srednee <- mean(x))
#(1+2+3+4+5+100)/6

(mediana <- median(x))
#берет число по серединке

y <- c(1,2,3,3,4,5,100)
#мода - самое частовстречаемое число

#С пробного первого экзамена
data <- read_csv("dirty.csv")

clean_digits <- function(col) {
  col <- col %>%
    str_remove_all("[^[:digit:]\\.]") %>%
    str_remove("\\.$") %>%
    str_remove("^\\.")%>%
    as.numeric()
}

data[,3:6] <- lapply(data[,3:6], clean_digits)

z_score <- function(x){
  y <- (x-mean(x)/sd(x))
  return(y)
}

plot_list <- list()

for (i in 1995:2020) {
  df_year <- data %>% filter(year == i)
  p <- df_year %>%
    ggplot() +
    geom_line(aes(z_score(gdp_per_capita), education_exp), color = "red") +
    geom_line(aes(z_score(gdp_per_capita), military_exp), color = "green") +
    geom_line(aes(z_score(gdp_per_capita), co2_emissions), color = "blue") 
  
  plot_list[[as.character(i)]] <- p
}

(final_plot <- ggarrange(plotlist = plot_list, ncol = 6, nrow = 6))



#Функция на валидацию

fae <- read_csv("fae.csv")
fsi <- read_excel("FSI-2023-DOWNLOAD.xlsx")

fae <- fae %>%
  filter(`Series Name` == "Total natural resources rents (% of GDP)") %>%
  select(`Country Name`, `2021 [YR2021]`)
colnames(fae)[2] <- "yr2021"
fae$yr2021 <- as.numeric(fae$yr2021)


(fsi <- fsi %>%
    select(-c(2:4)))

data <- left_join(fsi, fae, by = c("Country" = "Country Name"))
(data <- na.omit(data))

#вкуснятина
model_1 <- lm(`C2: Factionalized Elites` ~ yr2021, data = data)
summary(model_1)
model_2 <- lm(`C2: Factionalized Elites` ~ yr2021 + `C1: Security Apparatus`, data = data)
summary(model_2)

vifififi <- vif(model_2)


validation_check <- function(regr_model){
  #Проверка на NA
  if (any(is.na(coef(regr_model)))) {warning("Модель содержит NA - возможна проблема с данными")}
  #Проверка R-squared
  if (summary(regr_model)$r.squared < 0.1) {
    warning("Низкая объяснительная способность модели: R-squared ", round(summary(regr_model)$r.squared,2))}
  #Проверка распределения остатков
  shapiro_testirovanie <- shapiro.test(regr_model$residuals)
  if(shapiro_testirovanie$p.value < 0.05) {
    warning("Остатки модели распределены не нормально: тест Шапиро-Уилка ", shapiro_testirovanie$p.value)}
  #Тест Бройша-Пагана
  bp_testirovanie <- bptest(regr_model)
  if(bp_testirovanie$p.value < 0.05){
    warning("Остатки гетероскедатичны - валидация не пройдена: бп тест ", bp_testirovanie$p.value)}
  #VIF test
  if(length(regr_model$assign) - 1 > 1) {
    vif_testirovnie <- vif(regr_model)
    if (any(vif_testirovnie >= 5)){warning("Высокая корреляция между переменными: ", vif(regr_model))}
  }else {print("В модели только 1 независимая переменная - проверка на мультиколлинеарность не нужна")}
}

validation_check(model_1)
validation_check(model_2)

sd("Дядя Вася")

easy_sanity_check <- function(easy_regr_model) {
  ##############
  #SANITY CHECK
  #############
  if(is.list(easy_regr_model) == FALSE) {warning("В аргументе функции не модель, а что-то другое")}
  
  if(length(easy_regr_model$assign) - 1 > 1) {print(vif(easy_regr_model))}
  print(shapiro.test(easy_regr_model$residuals))
  print(bptest(easy_regr_model))
  }

easy_sanity_check("Виноградов")

