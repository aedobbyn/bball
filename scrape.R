
library(here)
library(tidyverse)
library(dobtools)
library(rvest)

kareem <- "https://www.basketball-reference.com/players/a/abdulka01.html"

lebron <- "https://www.basketball-reference.com/players/j/jamesle01.html"

durant <- "https://www.basketball-reference.com/players/d/duranke01.html"

player_urls <- c(kareem, lebron, durant)

player_names <- c("kareem", "lebron", "durant")

kareem_pts_per_game_raw <- 
  kareem %>% 
  read_html(kareem) %>% 
  # html_node("#per_game :nth-child(30)") %>% 
  html_nodes("table") %>% 
  html_table() 

kareem_pts_per_game <-
  kareem_pts_per_game[[1]] %>% 
  as_tibble()


get_tables <- function(nms = player_names,
                       urls = player_urls) {
  out <- NULL
  
  for (i in seq_along(urls)) {
    this <- 
      read_html(urls[i]) %>% 
      html_nodes("table") %>% 
      html_table() 
    
    names(this) <- nms[i]
    
    out <- out %>% append(raw)
  }
  return(out) 
}


all_stats <- get_tables()













