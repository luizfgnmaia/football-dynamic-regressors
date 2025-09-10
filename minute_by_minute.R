
set.seed(1)

library(dplyr)
library(tidyr)
library(ggplot2)
library(future.apply)
plan(multisession(workers = 12))

source("functions/get_gamestate.R")
source("functions/predict_dynamic_model.R")
load("data/models.RData")
load("data/probabilities.RData")
load("raw-data/results.RData")
load("raw-data/intervals.RData")

n_pred = 10^5

# probabilities %>%
#   arrange(desc(p_score_4_0)) %>%
#   View() # 1498

# Ceará 1-0 Paraná, match: 1498
# https://www.flashscore.com.br/jogo/EiGZ9Y2d/#/resumo-de-jogo/resumo-de-jogo

match_info = results %>%
  filter(match == 1498)

pred1 = list()
for(m in 0:44) {
  gamestate = get_gamestate(id = match_info$match, hlf = 1, min = m)
  pred1[[m+1]] = predict_dynamic_model(mod = models$`4`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = n_pred,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = gamestate["home_goals"],
                                       away_goals = gamestate["away_goals"],
                                       home_red_cards_1 = gamestate["home_red_cards_1"],
                                       away_red_cards_1 = gamestate["away_red_cards_1"],
                                       home_red_cards_2 = gamestate["home_red_cards_2"],
                                       away_red_cards_2 = gamestate["away_red_cards_2"],
                                       minute = m,
                                       half = 1)
}

for(m in 45:90) {
  gamestate = get_gamestate(id = match_info$match, hlf = 2, min = m-45)
  pred1[[m+1]] = predict_dynamic_model(mod = models$`4`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = n_pred,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = gamestate["home_goals"],
                                       away_goals = gamestate["away_goals"],
                                       home_red_cards_1 = gamestate["home_red_cards_1"],
                                       away_red_cards_1 = gamestate["away_red_cards_1"],
                                       home_red_cards_2 = gamestate["home_red_cards_2"],
                                       away_red_cards_2 = gamestate["away_red_cards_2"],
                                       minute = m-45,
                                       half = 2)
}

match_info = results %>%
  filter(match == 1498)

pred1 = list()
for(m in 0:44) {
  gamestate = get_gamestate(id = match_info$match, hlf = 1, min = m)
  pred1[[m+1]] = predict_dynamic_model(mod = models$`4`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = n_pred,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = gamestate["home_goals"],
                                       away_goals = gamestate["away_goals"],
                                       home_red_cards_1 = gamestate["home_red_cards_1"],
                                       away_red_cards_1 = gamestate["away_red_cards_1"],
                                       home_red_cards_2 = gamestate["home_red_cards_2"],
                                       away_red_cards_2 = gamestate["away_red_cards_2"],
                                       minute = m,
                                       half = 1)
}

for(m in 45:90) {
  gamestate = get_gamestate(id = match_info$match, hlf = 2, min = m-45)
  pred1[[m+1]] = predict_dynamic_model(mod = models$`4`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = n_pred,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = gamestate["home_goals"],
                                       away_goals = gamestate["away_goals"],
                                       home_red_cards_1 = gamestate["home_red_cards_1"],
                                       away_red_cards_1 = gamestate["away_red_cards_1"],
                                       home_red_cards_2 = gamestate["home_red_cards_2"],
                                       away_red_cards_2 = gamestate["away_red_cards_2"],
                                       minute = m-45,
                                       half = 2)
}

save(pred1, file = "data/pred1.RData")

# probabilities %>%
#   mutate(dif = (p_result_4_45 - p_result_3_45)/ p_result_3_45) %>%
#   arrange(desc(dif)) %>%
#   View() # 1129

# Ponte Preta 2-3 Vitória, match: 1129
# https://www.flashscore.com.br/jogo/KpgfDhqf/#/resumo-de-jogo/resumo-de-jogo

match_info = results %>%
  filter(match == 1129)

pred2 = list()
for(m in 0:44) {
  gamestate = get_gamestate(id = match_info$match, hlf = 1, min = m)
  pred2[[m+1]] = predict_dynamic_model(mod = models$`4`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = n_pred,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = gamestate["home_goals"],
                                       away_goals = gamestate["away_goals"],
                                       home_red_cards_1 = gamestate["home_red_cards_1"],
                                       away_red_cards_1 = gamestate["away_red_cards_1"],
                                       home_red_cards_2 = gamestate["home_red_cards_2"],
                                       away_red_cards_2 = gamestate["away_red_cards_2"],
                                       minute = m,
                                       half = 1)
}

for(m in 45:90) {
  gamestate = get_gamestate(id = match_info$match, hlf = 2, min = m-45)
  pred2[[m+1]] = predict_dynamic_model(mod = models$`4`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = n_pred,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = gamestate["home_goals"],
                                       away_goals = gamestate["away_goals"],
                                       home_red_cards_1 = gamestate["home_red_cards_1"],
                                       away_red_cards_1 = gamestate["away_red_cards_1"],
                                       home_red_cards_2 = gamestate["home_red_cards_2"],
                                       away_red_cards_2 = gamestate["away_red_cards_2"],
                                       minute = m-45,
                                       half = 2)
}

save(pred2, file = "data/pred2.RData")

#################################################################################
#################################################################################
#################################################################################

load("data/pred1.RData")
load("data/pred2.RData")

wide = tibble()
scores = c("1-0", "0-0", "2-0", "1-1", "3-0")
for(i in 1:91) {
  wide = rbind(wide, pred1[[i]]$Score[scores])
}
wide[is.na(wide)] = 0
names(wide) = scores
wide$Minute = 0:90

long = wide %>%
  pivot_longer(cols = scores,
               names_to = "Score",
               values_to = "Probability")

p = long %>%
  mutate(Score = factor(Score, levels = scores)) %>%
  ggplot(aes(x = Minute, y = Probability, col = Score)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 34, linetype = "dashed") +
  scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90)) +
  theme(legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white", color = "black"))

p

ggsave(filename = paste0("plots/Ceará x Paraná.png"),
       plot = p, width = 10, height = 5, dpi = 300)

#################################################################################

wide = tibble()
for(i in 1:91) {
  wide = rbind(wide, pred2[[i]]$Result)
}

names(wide) = c("Ponte Preta win", "Draw", "Vitória win")
wide$Minute = 0:90

long = wide %>%
  pivot_longer(cols = c("Ponte Preta win", "Draw", "Vitória win"),
               names_to = "Result",
               values_to = "Probability")

p = long %>%
  mutate(Result = factor(Result, levels = c("Ponte Preta win", "Draw", "Vitória win"))) %>%
  ggplot(aes(x = Minute, y = Probability, col = Result)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 7, linetype = "dashed") +
  geom_vline(xintercept = 16, linetype = "dashed") +
  geom_vline(xintercept = 58, linetype = "dashed", col = "red") +
  geom_vline(xintercept = 59, linetype = "dashed", col = "red") +
  geom_vline(xintercept = 82, linetype = "dashed", col = "red") +
  geom_vline(xintercept = 20, linetype = "dotted", col = "black") +
  scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90), limits = c(0, 90)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), limits = c(0, 1)) +
  scale_color_manual(values = c("black", "gray55", "red")) +
  theme(legend.position = c(0.83, 0.98),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", color = "black"))

p

ggsave(filename = paste0("plots/Ponte Preta x Vitória.png"),
       plot = p, width = 10, height = 5, dpi = 300)
