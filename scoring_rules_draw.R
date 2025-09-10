
library(dplyr)
library(xtable)

load("raw-data/results.RData")
load("raw-data/brasileirao_odds.RData")
load("data/matches_to_ignore.RData")

results = results %>%
  anti_join(matches_to_ignore) %>%
  mutate(Draw = home_goals == away_goals) %>% # just added these
  filter(Draw)                                # 2 lines

p_home = NULL
p_draw = NULL
p_away = NULL
p = NULL
for(i in 1:nrow(results)) {
  
  match = results[i,]
  
  odds = brasileirao_odds %>%
    filter(home_team == results$home_team[i], 
           away_team == results$away_team[i], 
           date == results$date[i],
           bookmaker == "bet365",
           bet_type == "hda") %>%
    mutate(p = 1/as.numeric(odd))
  odds$p = odds$p / sum(odds$p)
  
  if(nrow(odds) == 0) {
    p[i] = NA
  } else  if(match$result == 1) {
    p[i] = odds %>%
      filter(bet == "home") %>%
      .$p
    
    p_home[i] = p[i]
    
    p_draw[i] = odds %>%
      filter(bet == "draw") %>%
      .$p
    
    p_away[i] = odds %>%
      filter(bet == "away") %>%
      .$p
    
  } else if(match$result == 2) {
    
    p_home[i] = odds %>%
      filter(bet == "home") %>%
      .$p
    
    p[i] = odds %>%
      filter(bet == "draw") %>%
      .$p
    
    p_draw[i] = p[i]
    
    p_away[i] = odds %>%
      filter(bet == "away") %>%
      .$p
    
  } else {
    p[i] = odds %>%
      filter(bet == "away") %>%
      .$p
    
    p_home[i] = odds %>%
      filter(bet == "home") %>%
      .$p
    
    p_draw[i] = odds %>%
      filter(bet == "draw") %>%
      .$p
    
    p_away[i] = p[i]
    
  }
}

n = length(p)

bet365_probabilities = data.frame(match = results$match,
                                  bet365_home = p_home, 
                                  bet365_draw = p_draw, 
                                  bet365_away = p_away)

load("data/simulations.RData")

p_home_0 = NULL
p_home_1 = NULL
p_home_2 = NULL
p_home_3 = NULL
p_home_4 = NULL

p_draw_0 = NULL
p_draw_1 = NULL
p_draw_2 = NULL
p_draw_3 = NULL
p_draw_4 = NULL

p_away_0 = NULL
p_away_1 = NULL
p_away_2 = NULL
p_away_3 = NULL
p_away_4 = NULL

m = NULL

for(i in 1:length(simulations)) {
  
  p_home_0[i] = simulations[[i]]$`0`$`0`$Result[1]
  p_home_1[i] = simulations[[i]]$`1`$`0`$Result[1]
  p_home_2[i] = simulations[[i]]$`2`$`0`$Result[1]
  p_home_3[i] = simulations[[i]]$`3`$`0`$Result[1]
  p_home_4[i] = simulations[[i]]$`4`$`0`$Result[1]
  
  p_draw_0[i] = simulations[[i]]$`0`$`0`$Result[2]
  p_draw_1[i] = simulations[[i]]$`1`$`0`$Result[2]
  p_draw_2[i] = simulations[[i]]$`2`$`0`$Result[2]
  p_draw_3[i] = simulations[[i]]$`3`$`0`$Result[2]
  p_draw_4[i] = simulations[[i]]$`4`$`0`$Result[2]
  
  p_away_0[i] = simulations[[i]]$`0`$`0`$Result[3]
  p_away_1[i] = simulations[[i]]$`1`$`0`$Result[3]
  p_away_2[i] = simulations[[i]]$`2`$`0`$Result[3]
  p_away_3[i] = simulations[[i]]$`3`$`0`$Result[3]
  p_away_4[i] = simulations[[i]]$`4`$`0`$Result[3]
  
}

tmp = results %>%
  select(match, result)

input_brier = data.frame(match = as.integer(names(simulations)),
                         home_0 = p_home_0,
                         home_1 = p_home_1,
                         home_2 = p_home_2,
                         home_3 = p_home_3,
                         home_4 = p_home_4,
                         draw_0 = p_draw_0,
                         draw_1 = p_draw_1,
                         draw_2 = p_draw_2,
                         draw_3 = p_draw_3,
                         draw_4 = p_draw_4,
                         away_0 = p_away_0,
                         away_1 = p_away_1,
                         away_2 = p_away_2,
                         away_3 = p_away_3,
                         away_4 = p_away_4) %>%
  left_join(bet365_probabilities) %>%
  left_join(tmp) %>%
  mutate(home = ifelse(result == 1, 1, 0),
         draw = ifelse(result == 2, 1, 0),
         away = ifelse(result == 3, 1, 0)) %>%
  na.omit() # removed the 4 matches without odds

brier <- function(h, d, a, h0, d0, a0) {
  mean((h - h0)^2 + (d - d0)^2 + (a - a0)^2)
}

b0 = brier(input_brier$home_0,
           input_brier$draw_0,
           input_brier$away_0,
           input_brier$home,
           input_brier$draw,
           input_brier$away)

b1 = brier(input_brier$home_1,
           input_brier$draw_1,
           input_brier$away_1,
           input_brier$home,
           input_brier$draw,
           input_brier$away)

b2 = brier(input_brier$home_2,
           input_brier$draw_2,
           input_brier$away_2,
           input_brier$home,
           input_brier$draw,
           input_brier$away)

b3 = brier(input_brier$home_3,
           input_brier$draw_3,
           input_brier$away_3,
           input_brier$home,
           input_brier$draw,
           input_brier$away)

b4 = brier(input_brier$home_4,
           input_brier$draw_4,
           input_brier$away_4,
           input_brier$home,
           input_brier$draw,
           input_brier$away)

b365 = brier(input_brier$bet365_home,
             input_brier$bet365_draw,
             input_brier$bet365_away,
             input_brier$home,
             input_brier$draw,
             input_brier$away)

brier_scores = data.frame(model = c("Model 0", "Model 1", "Model 2", "Model 3", "Model 4", "bet365"),
                          brier = c(b0, b1, b2, b3, b4, b365)) %>%
  arrange(brier)

n = nrow(input_brier)

input_brier = input_brier %>%
  mutate(p_model_0 = ifelse(result == 1, home_0,
                            ifelse(result == 2, draw_0, 
                                   away_0)),
         p_model_1 = ifelse(result == 1, home_1,
                            ifelse(result == 2, draw_1, 
                                   away_1)),
         p_model_2 = ifelse(result == 1, home_2,
                            ifelse(result == 2, draw_2, 
                                   away_2)),
         p_model_3 = ifelse(result == 1, home_3,
                            ifelse(result == 2, draw_3, 
                                   away_3)),
         p_model_4 = ifelse(result == 1, home_4,
                            ifelse(result == 2, draw_4, 
                                   away_4)),
         p_bet365 = ifelse(result == 1, bet365_home,
                           ifelse(result == 2, bet365_draw, 
                                  bet365_away)))

log_0 = sum(log(input_brier$p_model_0))/n
log_1 = sum(log(input_brier$p_model_1))/n
log_2 = sum(log(input_brier$p_model_2))/n
log_3 = sum(log(input_brier$p_model_3))/n
log_4 = sum(log(input_brier$p_model_4))/n
log_bet365 = sum(log(input_brier$p_bet365))/n

tab = data.frame(model = c("Model 0", "Model 1", "Model 2", "Model 3", "Model 4", "bet365"),
                 log = c(log_0, log_1, log_2, log_3, log_4, log_bet365)) %>%
  left_join(brier_scores)

rps <- function(h, d, a, h0, d0, a0) {
  mean(((h - h0)^2 + ((h + d) - (h0 + d0))^2)/2)
}

rps0 = rps(input_brier$home_0,
           input_brier$draw_0,
           input_brier$away_0,
           input_brier$home,
           input_brier$draw,
           input_brier$away)

rps1 = rps(input_brier$home_1,
           input_brier$draw_1,
           input_brier$away_1,
           input_brier$home,
           input_brier$draw,
           input_brier$away)

rps2 = rps(input_brier$home_2,
           input_brier$draw_2,
           input_brier$away_2,
           input_brier$home,
           input_brier$draw,
           input_brier$away)

rps3 = rps(input_brier$home_3,
           input_brier$draw_3,
           input_brier$away_3,
           input_brier$home,
           input_brier$draw,
           input_brier$away)

rps4 = rps(input_brier$home_4,
           input_brier$draw_4,
           input_brier$away_4,
           input_brier$home,
           input_brier$draw,
           input_brier$away)

rps365 = rps(input_brier$bet365_home,
             input_brier$bet365_draw,
             input_brier$bet365_away,
             input_brier$home,
             input_brier$draw,
             input_brier$away)

rps_scores = data.frame(model = c("Model 0", "Model 1", "Model 2", "Model 3", "Model 4", "bet365"),
                        rps = c(rps0, rps1, rps2, rps3, rps4, rps365)) %>%
  arrange(rps)

tab = tab %>%
  left_join(rps_scores)

ign_0 = -sum(log(input_brier$p_model_0, base = 2))/n
ign_1 = -sum(log(input_brier$p_model_1, base = 2))/n
ign_2 = -sum(log(input_brier$p_model_2, base = 2))/n
ign_3 = -sum(log(input_brier$p_model_3, base = 2))/n
ign_4 = -sum(log(input_brier$p_model_4, base = 2))/n
ign_bet365 = -sum(log(input_brier$p_bet365, base = 2))/n

tab_ign = data.frame(model = c("Model 0", "Model 1", "Model 2", "Model 3", "Model 4", "bet365"),
                     ign = c(ign_0, ign_1, ign_2, ign_3, ign_4, ign_bet365))

tab = tab %>%
  left_join(tab_ign) %>%
  select(-log) %>%
  filter(model %in% c("Model 4", "bet365"))

print(xtable(tab, digits = 4), include.rownames = FALSE)