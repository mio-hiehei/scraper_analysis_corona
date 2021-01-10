library(rvest)
library(RSelenium)
library(tidyverse)
library(stringr)
library(RCurl)
library(maps)
library(htmltab)
library(lubridate)
library(httr)
library(truncnorm)
library(rstudioapi)


wd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(wd)


# url to parse vote information

url <- paste0("https://www.abgeordnetenwatch.de/bundestag/abstimmungen?page=",c(0:11))


# 1. extract links for vote pages

links <- vector()

for (i in 1:length(url)){
  
  temp <- url[i] %>%
  read_html %>%
  html_nodes(xpath = "/html/body/div[1]/main/div[2]/div/article/div/div/div/div/article/a") %>%
  html_attr("href") %>%
  paste0("https://www.abgeordnetenwatch.de",.,"/tabelle")
  
  links <- c(links,temp)
}

# 2. scrape information from vote page

# 2.1 get votes by MP

df <- data.frame(Name = character(),
                 Fraktion = character(), 
                 Wahlkreis = character(), 
                 Stimme = character(),
                 Name = character(),
                 Date = character(),
                 Description = character(),
                 Outcome = character())


#colnames(df) <- colnames(table)

for (i in 1:length(links)){
  
  
  parsed <- read_html(links[i])
  
  last_page <- parsed %>% html_nodes(xpath = "/html/body/div[1]/main/div[2]/div/article/div[1]/div/div[3]/nav/ul/li[12]/a") %>% html_attr("href") %>% paste0(links[i],.)
  
  no_pages <- str_extract(last_page, "\\d{1,3}$")
  
  pages <- paste0(str_remove(last_page, "\\d{1,3}$"),c(1:no_pages))
  
  
  table <- data.frame(Name = character(),
                      Fraktion = character(), 
                      Wahlkreis = character(), 
                      Stimme = character())
  
  for (j in 1:length(pages)){
    
    temp <- pages[j] %>% read_html %>%
      html_table() %>%
      .[[1]] %>% 
      .[,2:5]
    colnames(temp) <- colnames(table)
      
      table <- rbind(table,temp)
      
      cat(paste0(" ",i,".",j," "))
      
     # Sys.sleep(rtruncnorm(1,0.8,1, a = 0.0001))
    
  }
  
  table$Name <- parsed %>%
    html_nodes(xpath = "/html/body/div[1]/main/div[1]/div/div/div[1]/h1") %>%
    html_text()
  
  table$Date <- parsed %>%
    html_nodes(xpath = "/html/body/div[1]/main/div[1]/div/div/div[1]/span/div/time") %>%
    html_text()
  
  table$Description <- parsed %>% 
    html_nodes(xpath = "/html/body/div[1]/main/div[1]/div/div/div[1]/div/p[1]") %>%
    html_text()
  
  table$Outcome <- ifelse(is_empty(html_nodes(parsed, xpath = "/html/body/div[1]/main/div[1]/div/div/div[1]/div/p[2]")),NA,html_text(html_nodes(parsed, xpath = "/html/body/div[1]/main/div[1]/div/div/div[1]/div/p[2]")))
  
  table$Policy_areas <- parsed %>% 
  html_nodes(xpath = "/html/body/div[1]/main/div[2]/div/article/div[2]/div/div[2]/div[1]/span/a
") %>% 
    html_text() %>%
    paste(collapse = ", ")
    
    
  df <- rbind(df,table)
  
  # repeated writing to save the progress when interruption
  write.csv2(df, paste0("corona_bt_",i,".csv"))
  
  Sys.sleep(runif(1,2,1))

}

