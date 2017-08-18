library(tidyverse)
library(rvest)

url <- "https://www.hsx.com/security/view/GCLOO"

movie <- read_html(url) %>%
    html_nodes(".credit a") %>%
    html_text()

link <- read_html(url) %>%
    html_nodes(".credit a") %>%
    html_attr("href")

date <- read_html(url) %>%
    html_nodes("strong") %>%
    html_text() %>%
    .[1:length(link)]

df <- tibble(date, movie, link)


library(stringr)

base <- "https://www.hsx.com"
prices <- tibble()

for (link in df$link){
    url <- str_c(base, link)
    
    price <- read_html(url) %>%
        html_nodes(".value") %>%
        html_text() %>%
        str_extract(., "(?<=\\$)[^\\s]+") %>%
        parse_number(.)
    
    prices <- bind_row(prices, tibble(link, price))
}

get_price <- function(link) {
    url <- str_c(base, link)
    df <- read_html(url) %>%
    
        html_nodes(".value") %>%
        html_text() %>%
        as_tibble() %>%
        mutate(value =  parse_number( str_extract(., "(?<=\\$)[^\\s]+") ) %>%
        rename(price = value) %>%
        bind_cols(link = tibble(link))
    return (df)
}


params <- df %>% select(link)

ptm <- proc.time()

prices <- params %>%
    pmap(get_price) %>%
    bind_rows()

prices <- prices %>% left_join(df, by = "link")

tag <- prices %>%
    