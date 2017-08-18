library(tidyverse)
library(rvest)

# part 1 - STARBONDS

url <- "https://www.hsx.com/security/view/ADRIV"

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

# part 2 - STOCKS

library(stringr)

base <- "https://www.hsx.com"
link <- "/security/view/STAR7"
url <- str_c(base, link)

price <- read_html(url) %>% 
    html_nodes(".value") %>% 
    html_text() %>% 
    str_extract(., "(?<=\\$)[^\\s]+") %>% 
    parse_number(.)

price_df <- price %>% 
    as_tibble() %>% 
    rename(price = value) %>% 
    bind_cols(link = tibble(link))

# part 3 - LOOPS

for(link in df$link) {
    print(link)
}

prices <- tibble()

for(link in df$link) {
    url <- str_c("https://www.hsx.com", link)
    price <- read_html(url) %>% 
        html_nodes(".value") %>% 
        html_text() %>% 
        str_extract(., "(?<=\\$)[^\\s]+") %>% 
        parse_number(.)
    prices <- bind_rows(prices, tibble(link, price))
}

# part 3 - FUNCTIONS

get_price <- function(link) {
    url <- str_c('https://www.hsx.com', link)
    price <- read_html(url) %>% 
        html_nodes(".value") %>% 
        html_text() %>% 
        str_extract(., "(?<=\\$)[^\\s]+") %>% 
        parse_number(.)
    return(price)
}

get_price('/security/view/STAR8')

df <- df %>% 
    mutate(price = get_price(link)) # will not work

prices <- df %>% 
    rowwise() %>% 
    mutate(price = get_price(link))

# part 4 - PURRR

get_price <- function(link) {
    url <- str_c('https://www.hsx.com', link)
    df <- read_html(url) %>% 
        html_nodes(".value") %>% 
        html_text() %>% 
        as_tibble() %>% 
        mutate(value = parse_number(str_extract(., "(?<=\\$)[^\\s]+"))) %>% 
        rename(price = value) %>% 
        bind_cols(link = tibble(link))
    return(df)
}

get_price('/security/view/STAR8')

params <- df %>% select(link)

prices <- params %>% 
    pmap(get_price) %>% 
    bind_rows()

prices <- prices %>% left_join(df, by = "link")

# quick NHL

url <- "https://en.wikipedia.org/wiki/Toronto_Maple_Leafs"

players <- read_html(url) %>% 
    html_nodes(".fn a") %>% 
    html_text()
    
    