library(tidyverse)
library(readr)
library(ggpubr)
library(car)
library(lmtest)


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

data %>%
  filter(year %in% 2000:2001) %>%
  ggplot() +
  geom_line(aes(z_score(gdp_per_capita), education_exp), color = "red") +
  geom_line(aes(z_score(gdp_per_capita), military_exp), color = "green") +
  geom_line(aes(z_score(gdp_per_capita), co2_emissions), color = "blue") +
  facet_grid(~year)

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

final_plot <- ggarrange(plotlist = plot_list, ncol = 6, nrow = 6)

model <- lm(gdp_per_capita ~ education_exp + military_exp, data = data)
summary(model)

vif(model)

bptest(model)

shapiro.test(model$residuals)
hist(model$residuals)

data <- data %>%
  mutate(political_system = case_when(
    polity_score >= 0 ~ 1,
    polity_score < 0 ~ 0
  ))

model_logical <- glm(political_system ~ education_exp + military_exp, data = data,
                     family = binomial(link = "logit"))
summary(model_logical)

vif(model_logical)

bptest(model_logical)

shapiro.test(model_logical$residuals)
hist(model_logical$residuals)
