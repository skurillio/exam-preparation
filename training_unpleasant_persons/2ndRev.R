
#Теоритическое задание

#Влияет ли уровень образования на вероятность голосования на выборах? - логит регрессия
#Есть ли связь между уровнем стресса и количеством часов сна? - корреляция
#Различается ли средняя удовлетворенность жизнью между мужчинами, женщинами и небинарными людьми? - анова
#Какие комбинации факторов приводят к успеху стартапов (например: наличие инвестиций, опыт команды, рыночная ниша)? - QCA
#Связана ли частота занятий спортом с уровнем артериального давления? - корреляция

#Повышает ли наличие гибкого графика вероятность того, что сотрудник останется в компании дольше года? - логистичная регрессия
#Влияет ли уровень дохода и образование одновременно на уровень счастья? - линейная регрессия
#Зависит ли распределение типа занятости (полная/частичная/самозанятость) от пола и возраста? - кросстабуляция
#Какие комбинации факторов ведут к высокой вовлеченности работников? - QCA
#Существует ли различие в среднем количестве дней отпуска между отделами компании? - анова

#Как связаны средний балл в университете и уровень тревожности у студентов? - корреляция
#Какие условия способствуют высокой цифровой грамотности у населения? - QCA
#Зависит ли вероятность использовать онлайн-банкинг от возраста и уровня образования? - логистическая регрессия
#Различается ли уровень поддержки экологической политики между странами с разным уровнем ВВП? - анова
#Есть ли связь между количеством кофе в день и продуктивностью сотрудников? - корреляция


#Парсинг
library(polite)
library(rvest)

url <- "https://smartnewsliberia.com/category/development/"
user_agentus <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/136.0.0.0 Safari/537.36"

page <- bow(url, user_agent = user_agentus, delay = 7, force = TRUE) %>%
  scrape()

titles_collection <- c()
links_collection <- c()
text_collection <- c()

#Парсинг 1 страницы
titles <- page %>% html_elements(".td-module-meta-info") %>%
  html_text()
titles_collection <- c(titles_collection, titles)

links <- page %>% html_elements(".td-module-meta-info") %>% html_node("a") %>% html_attr("href")

links_collection <- c(links_collection, links)


for (scraper1stpage in 1:29) {
  print(paste0("Извлекаю текст с новости номер № ", scraper1stpage))
  pagenews1stpage <- bow(links[scraper1stpage], delay = 5, user_agent = user_agentus,
                  force = TRUE) %>%
    scrape()
  text <- pagenews1stpage %>%
    html_elements(".tdb-block-inner td-fix-index") %>%
    html_text() %>%
    paste(collapse = " ") # разграничитель
  text_collection <- c(text_collection, text)
}


for (i in 2:4) {
  url_new <- paste0(url, )
}


#QCA
library(readxl)
library(QCA)
library(venn)

lipset <- read_excel("~/Desktop/делаю виноградова/lipset.xlsx")

hist(lipset$GNPCAP)
crisp_gnpcap <- calibrate(lipset$GNPCAP, type = "crisp", thresholds = 590)
plot(lipset$GNPCAP, crisp_gnpcap)

hist(lipset$URBANIZA)
crisp_urbniza <- calibrate(lipset$URBANIZA, type = "crisp", thresholds = 34)
plot(lipset$URBANIZA, crisp_urbniza)

hist(lipset$LITERACY)
crisp_literacy <- calibrate(lipset$LITERACY, type = "crisp", thresholds = 95)
plot(lipset$LITERACY, crisp_literacy)

hist(lipset$INDLAB)
crisp_indlab <- calibrate(lipset$INDLAB, type = "crisp", thresholds = 29)
plot(lipset$INDLAB, crisp_indlab)

crisp_data <- data.frame(crisp_gnpcap, crisp_urbniza, crisp_literacy, crisp_indlab, 
                         lipset$Outcome)

tt <- truthTable(crisp_data, outcome = "lipset.Outcome", incl.cut = 0.7)

minimize(tt)

venn(tt)

Xplot(lipset$URBANIZA, jitter = TRUE, cex = 0.8)
urbaniza_fuzzy <- calibrate(lipset$URBANIZA, type = "fuzzy", thresholds = "e=24, c=40, i=55")
plot(lipset$URBANIZA, urbaniza_fuzzy)


#работа с текстом
library(ggplot2)
library(tidytext)
library(stringr)
library(quanteda)
library(quanteda.textplots)
library(topicmodels)
library(tm)

text_data <- readRDS("vector.rds")
#стеминг
stemming <- stemDocument(tolower(text_data), language = "english")

#соотношение уникальных слов ко всех и график этого
#создаем корпус слов (разбиваем текст на слова)
corpusnya <- corpus(text_data)
#делаем анализ корпуса
corpus_summary <- summary(corpusnya)

as.numeric(corpus_summary$Text)

ggplot(corpus_summary, aes(Text, Types/Tokens, group = 1)) +
  geom_line()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#облако слов
#сначала суммаризируем корпус
sumoftok <- tokens(corpusnya, remove_numbers = TRUE, remove_punct = TRUE)
sumoftok <- tokens_remove(sumoftok, stopwords("english"))
#делаем тематическое моделирование
temat <- dfm(sumoftok)
textplot_wordcloud(temat)

#конвертируем дфм в топикмодели
convertion <- convert(temat, to = "topicmodels")
#латентное распределение
ldashka <- LDA(convertion, 5)
terms(ldashka, 10)
