
# Test scraping one url
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