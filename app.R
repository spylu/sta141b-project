  library(shiny)
  library(nycflights13)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(httr)
  library(rvest)
  library(tidytext)
  
  ## WEB SCRAPING AND DATA ASSEMBLY
  
  page1 <- read_html("https://myanimelist.net/topanime.php")
  
  ranks <- as_tibble(c(1:50)) %>% rename(Rank=value)
  
  name <- page1 %>% html_nodes("div.detail") %>% html_nodes("div.di-ib.clearfix") %>% html_nodes("a.hoverinfo_trigger.fl-l.fs14.fw-b") %>% html_text() %>% as_tibble() %>% rename(Name=value)
  
  url <- page1 %>% html_nodes("div.detail") %>% html_nodes("div.di-ib.clearfix") %>% html_nodes("a.hoverinfo_trigger.fl-l.fs14.fw-b") %>% html_attr("href") %>% as_tibble() %>% rename(URL=value)
  
  rating9 <- page1 %>% html_nodes("div.js-top-ranking-score-col.di-ib.al") %>% html_nodes("span.text.on.score-label.score-9") %>% html_text() %>% as_tibble() %>% rename(Rating=value)
  
  rating8 <- page1 %>% html_nodes("div.js-top-ranking-score-col.di-ib.al") %>% html_nodes("span.text.on.score-label.score-8") %>% html_text() %>% as_tibble() %>% rename(Rating=value)
  
  rating <- rbind(rating9,rating8)
  
  genre <- c("Action","Adventure","Cars","Comedy","Demons","Drama","Fantasy","Game","Historical","Horror","Josei","Kids","Magic","Martial_Arts","Mecha","Military","Music","Myster","Parody","Police","Psychological","Romance","Samurai","School","Sci-Fi","Seinen","Shoujo","Shoujo_Ai","Shounen","Shounen_Ai","Slice_of_Life","Space","Sports","Super_Power","Supernatural","Thriller","Vampire")
  
  genres <- data.frame(Genres = NA)
  n=1
  while(n <= 50){
    x = slice(url,n) %>% pull
    y = read_html(x) %>% 
      html_nodes("td.borderClass") %>% 
      html_nodes("div") %>%
      html_nodes("a") %>%
      html_attr("href") %>% 
      as_tibble() %>% 
      filter(str_detect(value,"genre")) %>%
      mutate(value=str_extract(value,"[A-Z].*")) %>% 
      summarise(genres=paste(value,collapse=", "))
    genres[n,1] <- y
    n=n+1
  }
  
  all <- bind_cols(ranks,name,genres,rating,url)
  
  ########
  
  page2 <- read_html("https://myanimelist.net/topanime.php?limit=50")
  
  ranks <- as_tibble(c(51:100)) %>% rename(Rank=value)
  
  name <- page2 %>% html_nodes("div.detail") %>% html_nodes("div.di-ib.clearfix") %>% html_nodes("a.hoverinfo_trigger.fl-l.fs14.fw-b") %>% html_text() %>% as_tibble() %>% rename(Name=value)
  
  url <- page2 %>% html_nodes("div.detail") %>% html_nodes("div.di-ib.clearfix") %>% html_nodes("a.hoverinfo_trigger.fl-l.fs14.fw-b") %>% html_attr("href") %>% as_tibble() %>% rename(URL=value)
  
  rating9 <- page2 %>% html_nodes("div.js-top-ranking-score-col.di-ib.al") %>% html_nodes("span.text.on.score-label.score-9") %>% html_text() %>% as_tibble() %>% rename(Rating=value)
  
  rating8 <- page2 %>% html_nodes("div.js-top-ranking-score-col.di-ib.al") %>% html_nodes("span.text.on.score-label.score-8") %>% html_text() %>% as_tibble() %>% rename(Rating=value)
  
  rating <- rbind(rating9,rating8)
  
  genre <- c("Action","Adventure","Comedy","Demons","Demons","Drama","Fantasy","Game","Historical","Horror","Kids","Magic","Martial_Arts","Mecha","Military","Music","Myster","Parody","Police","Psychological","Romance","Samurai","School","Sci-Fi","Seinen","Shoujo","Shoujo_Ai","Shounen","Shounen_Ai","Slice_of_Life","Space","Sports","Super_Power","Supernatural","Thriller","Vampire")
  
  genres <- data.frame(Genres = NA)
  n=1
  while(n <= 50){
    x = slice(url,n) %>% pull
    y = read_html(x) %>% 
      html_nodes("td.borderClass") %>% 
      html_nodes("div") %>%
      html_nodes("a") %>%
      html_attr("href") %>% 
      as_tibble() %>% 
      filter(str_detect(value,"genre")) %>%
      mutate(value=str_extract(value,"[A-Z].*")) %>% 
      summarise(genres=paste(value,collapse=", "))
    genres[n,1] <- y
    n=n+1
  }
  
  all2 <- bind_cols(ranks,name,genres,rating,url)
  all <- bind_rows(all,all2)

#################################

ui <- fluidPage(
  
  helpText("Use this option if you want to look up a specific anime."),
  
  selectInput(inputId = "inName",
              label = "Anime",
              choices = c("-",all %>% arrange(Name) %>% select(Name) %>% pull())),
  tableOutput("summary"),
  
  helpText("Or, look up multiple animes based on genre and rating. Rankings are calculated by magnitude of ratings."),
  
  selectInput(inputId = "inGenre",
              label = "Genre",
              choices = c(genre)),
  
  selectInput(inputId = "inRating",
              label = "Minimum Rating",
              choices = c("-"=1,8.5,8.75,9,9.25)),
  
  helpText("This number shows how many animes fit your specified qualifications."),
  
  verbatimTextOutput("count"),
  
  helpText("This table shows the ratings summary statistics for the animes that fit your specified qualifications."),
  
  verbatimTextOutput("stuff"),
  
  helpText("This table shows the details of the animes that fit your specified qualifications."),
  
  tableOutput("rating")

)

server <- function(input, output, session) {
  
  output$summary <- renderTable({
    all %>% filter(Name==input$inName|Name=="asdf")
  })
  
  output$rating <- renderTable({
    all %>% filter(Rating >= input$inRating) %>% filter(str_detect(Genres,input$inGenre))
  })

  output$stuff <- renderPrint({
    summary(pull(all %>% filter(Rating >= input$inRating) %>% filter(str_detect(Genres,input$inGenre)) %>% select(Rating) %>% mutate_if(is.character,as.numeric)))
  })
  
  output$count <- renderPrint({
    pull(count(all %>% filter(Rating >= input$inRating) %>% filter(str_detect(Genres,input$inGenre))))
  })

}

shinyApp(ui,server)