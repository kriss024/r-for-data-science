rm(list = ls())

library(rvest)
lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")

rating <- lego_movie %>%
  html_nodes("strong span") %>%
  html_text() %>%
as.numeric()

print(rating)

rating2 <- lego_movie %>%
  html_nodes(".ratings_wrapper .imdbRating .ratingValue strong") %>%
  html_text() %>%
as.numeric()

print(rating2)

best <- lego_movie %>%
  html_nodes(".ratings_wrapper .imdbRating .grey") %>%
html_text()

print(best)

rm(list = ls())

scraping_wiki <- read_html("https://en.wikipedia.org/wiki/Web_scraping")

scraping.h1 <- scraping_wiki %>%
    html_nodes("h1") %>%
    html_text()

print(scraping.h1)

scraping.h2 <- scraping_wiki %>%
        html_nodes("h2") %>%
        html_text()

print(scraping.h2)

p_text <- scraping_wiki %>%
        html_nodes("p") %>%
        html_text()

print(p_text[1])

all_text <- scraping_wiki %>%
        html_nodes("div") %>%
        html_text()

techniques <- scraping_wiki %>%
        html_nodes("#Techniques") %>%
        html_text()

print(techniques)

rm(list = ls())

page = read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_by_life_expectancy")

my.table <- page %>%
    html_nodes(".wikitable") %>%
    html_table(fill = TRUE)

data <- data.frame(my.table)
