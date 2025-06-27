library(rvest)
library(polite)
library(tidyverse)

url <- "https://www.sergievgrad.ru/news/"

useragentus <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/136.0.0.0 Safari/537.36"

titles_collection <- c()
links_collection <- c()
text_collection <- c()

page <- bow(url, user_agent = useragentus, delay = 8, force = TRUE) %>%
  scrape()

titles <- page %>% html_elements(".c-news-block__title") %>%
  html_text()
titles_collection <- c(titles_collection, titles)

links <- page %>% html_elements(".c-news-block__title") %>%
  html_attr("href") %>%
  url_absolute("https://www.sergievgrad.ru")
links_collection <- c(links_collection, links)

#очистка заголовков
titles_collection <- titles_collection %>%
  str_replace_all("\\s+", " ") %>%
  str_trim()

for (scraper in 1:25){
  print(paste0("Извлекаю текст с новости номер № ", scraper))
  pagenews <- bow(links[scraper], delay = 10, user_agent = useragentus,
                  force = TRUE) %>%
    scrape()
  text <- pagenews %>%
    html_elements(".article-details__text") %>%
    html_text() %>%
    paste(collapse = " ")
  text_collection <- c(text_collection, text)
}

data_titles_links <- as.data.frame(titles_collection)
data_titles_links <- cbind(data_titles_links, links_collection)
