# idea: make R shiny interactive map over mail suspension alerts, based on info collected by postcrossing
library(tidyverse)
######## step 1 get info ###########
# Webscraping https://towardsdatascience.com/web-scraping-tutorial-in-r-5e71fd107f32

# read html web page in as xml doc
#install.packages("rvest")
library(rvest)
pc.page <- read_html("https://www.postcrossing.com/postal-monitor")

# get date of update
updated <- pc.page %>% html_nodes(".last:nth-child(5)") %>% html_text(trim = TRUE)


# special covid countries which have blocked all in-going and out-going
covid.blocked <- pc.page %>% html_nodes("#mainContentArea .last a") %>% html_text(trim = TRUE)

# Now I want to collect a list of all the countries they have info about
# great tool: https://selectorgadget.com/

# get each country's block of info
country.info.list <- pc.page %>% html_nodes("details")

results <- tibble("send.country" = character(), 
                  "block.num" = numeric(),
                  "blocked.list" = list())


for (n in 1:length(country.info.list)) {
  # find sender country name and number of blocked countries
  # get the line which has the country name
  send.country.line <- country.info.list[n] %>% html_text(trim = TRUE)
  # isolate country name based on it being at the start of the string followed by "([any digit]" 
  last.pos <- str_locate(send.country.line, "\\(\\d")[1]-1
  send.country <- trimws(substr(send.country.line, 1, last.pos))
  # might as well get the number of blocked countries while we are at it
  send.country.blocked.num <- substr(send.country.line, last.pos+2, str_locate(send.country.line, "\\)")[1]-1)
  
  # find character vector of countries blocked by this country
  blocked.vec <- country.info.list[n] %>% html_nodes("li") %>% html_text(trim = TRUE)
  remove.me <- c("\n", "\u274c", "(source)", "Source: Contact at Maldives Post", "Source: Kyrgyz Post, via Messenger.")
  blocked.vec <- trimws(str_remove_all(blocked.vec, paste(remove.me, collapse = "|")))
  blocked.vec.clean <- trimws(str_remove_all(blocked.vec, "\\(\\)"))

  # save info in tibble
  results <- results %>% add_row(send.country = send.country, 
                                 block.num = as.numeric(as.character(send.country.blocked.num)), 
                                 blocked.list = list(blocked.vec.clean))
}


# how many countries are there in total?
all.countries <- sort(unique(c(results$send.country, unlist(results$blocked.list))))
length(all.countries)



