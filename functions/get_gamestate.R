
get_gamestate <- function(id, hlf, min) {
  
  m = intervals %>%
    filter(match == id)
  
  if(hlf == 1) {
    home_red_cards_1 = m %>%
      filter(half == 1, end < min) %>%
      .$home_red_card %>%
      sum()
    
    away_red_cards_1 = m %>%
      filter(half == 1, end < min) %>%
      .$away_red_card %>%
      sum()
    
  } else if(hlf == 2) {
    home_red_cards_1 = m %>%
      filter(half == 1) %>%
      .$home_red_card %>%
      sum()
    
    away_red_cards_1 = m %>%
      filter(half == 1) %>%
      .$away_red_card %>%
      sum()
  }
  
  if(hlf == 2) {
    home_red_cards_2 = m %>%
      filter(half == 2, end < min) %>%
      .$home_red_card %>%
      sum()
    
    away_red_cards_2 = m %>%
      filter(half == 2, end < min) %>%
      .$away_red_card %>%
      sum()
    
  } else if(hlf == 1) {
    home_red_cards_2 = 0
    away_red_cards_2 = 0
  }
  
  int = m %>%
    filter(half == hlf, begin < min, end >= min)
  
  if(min == 0 & hlf == 2) {
    int = m %>%
      filter(half == 2) %>%
      head(1)
  }
  
  if(min == 0 & hlf == 1) {
    int = tibble(current_home_goals = 0, current_away_goals = 0)
  }
  
  c("home_goals" = int$current_home_goals,
    "away_goals" = int$current_away_goals,
    "home_red_cards_1" = home_red_cards_1,
    "away_red_cards_1" = away_red_cards_1,
    "home_red_cards_2" = home_red_cards_2,
    "away_red_cards_2" = away_red_cards_2)
}
