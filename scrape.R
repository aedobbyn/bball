
library(here)
library(tidyverse)
library(dobtools)
library(rvest)
library(hrbrthemes)
library(wesanderson)

pal <- wesanderson::wes_palette("Darjeeling1")

kareem <- "https://www.basketball-reference.com/players/a/abdulka01.html"

lebron <- "https://www.basketball-reference.com/players/j/jamesle01.html"

durant <- "https://www.basketball-reference.com/players/d/duranke01.html"

player_urls <- c(kareem, lebron, durant)

player_names <- c("kareem", "lebron", "durant")

kareem_pts_per_game_raw <- 
  kareem %>% 
  read_html(kareem) %>% 
  # html_node("#per_game :nth-child(30)") %>% 
  # html_nodes(css = "#totals .center , #totals .left , #totals .right") %>%
  # html_nodes("#totals") %>% 
  html_nodes("table") %>%
  html_nodes("#totals") %>%
  # html_nodes("table") %>% 
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
    
    out <- out %>% append(this)
  }
  return(out) 
}


all_stats <- get_tables()


tidy_stats <- function(lst) {
  out <- NULL
    
  for (i in seq_along(lst)) {
    this <- lst[[i]] %>% 
      mutate(player = names(lst[i]))
    
    out <- out %>% bind_rows(this)
  }
  out <- out %>% as_tibble()
  return(out)
}


tidied <- tidy_stats(all_stats)



tidied_clean <- 
  tidied %>% 
  filter(!(str_detect(Season, "[A-Za-z]") |
           Season == "")) %>% 
  group_by(player) %>% 
  rename(
    season_points = PTS
  ) %>% 
  mutate(
    season_num = row_number(),
    cumulative_points = cumsum(season_points)
  ) %>% 
  select(Season, player, season_num, season_points, cumulative_points) 


ggplot(tidied_clean) +
  geom_point(aes(x = season_num, y = cumulative_points, colour = player),
             stat = "identity") +
  labs(x = "Season Number", y = "Cumulative Points", colour = "Player") +
  ggtitle("Catching Kareem") +
  theme_ipsum() +
  scale_colour_manual(values = pal) 


tidied_clean %>% 
  filter(season_num == 15)





