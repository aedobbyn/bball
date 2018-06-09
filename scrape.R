library(here)
library(tidyverse)
library(dobtools)
library(rvest)
library(hrbrthemes)
library(wesanderson)

pal <- wesanderson::wes_palette("Darjeeling1")


# Gather urls and names
kareem <- "https://www.basketball-reference.com/players/a/abdulka01.html"
lebron <- "https://www.basketball-reference.com/players/j/jamesle01.html"
durant <- "https://www.basketball-reference.com/players/d/duranke01.html"

player_urls <- c(kareem, lebron, durant)
player_names <- c("Kareem", "Lebron", "Durant")



# Scrape each url and throw it in a named list, giving it the name nsm[i]
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


# Tidy that list into a dataframe, adding a column for player name
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


# Filter out seasons that aren't actually seasons
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


# Plot season averages
ggplot(tidied_clean) +
  geom_point(aes(x = season_num, y = cumulative_points, colour = player),
             stat = "identity") +
  labs(x = "Season Number", y = "Cumulative Points", colour = "Player") +
  ggtitle("Catching Kareem", subtitle = "Average points per game") +
  theme_ipsum() +
  scale_colour_manual(values = pal) 

# Save plot
ggsave("catching_kareem.png", path = here("img"))

# Check out season number where Lebron surpasses Kareem
tidied_clean %>% 
  filter(season_num == 15)

