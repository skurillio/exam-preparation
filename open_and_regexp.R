library(readxl)
library(tidyverse)
#Загрузка файлов

#.csv
data <- read.csv("sample.csv", header = TRUE, sep = "|")
data <- read_csv("sample.csv", header = TRUE, sep = "|")

#Excel
data <- read_xlsx("sample.xlsx", sheet = 1, col_names = TRUE, col_types = NULL,
                  na = NULL)
data <- read_xls()
data <- read_excel()

#Регулярные выражения

#Извлечение емейлов
task_1 <- "Контакты: user123@gmail.com, support@company.org, неверный-email, hello@world"

task_1 %>% 
  str_extract_all("\\b[\\w.]+@[\\w.]+\\.[a-z]{2,}\\b")

#Удаление пунктуации
task_2 <- "Текст с     лишними    пробелами, и пунктуацией! Нужно очистить..."

task_2 %>%
  str_replace_all("\\s+", " ") %>%
  str_remove_all("[:punct:]") %>%
  str_trim()
  

#Разделение столбца
col_maninup <- read_excel("exchel_task4.xlsx")

col_maninup$`hemisphere|continent` %>% 
  str_extract_all("^\\w+(?=\\|)")

col_maninup$`hemisphere|continent` %>% 
  str_extract_all("(?<=\\|)\\w+(.+)?")

col_maninup <- col_maninup %>%
  mutate(hemisphere = unlist(str_extract_all(col_maninup$`hemisphere|continent`,"^\\w+(?=\\|)")),
         continent = unlist(str_extract_all(col_maninup$`hemisphere|continent`, "(?<=\\|)\\w+(.+)?"))) %>%
  select(-2)

col_maninup <- col_maninup %>%
  separate("hemisphere|continent", into = c("hemisphere", "continent"))
