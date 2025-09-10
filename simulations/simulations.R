
library(dplyr)
library(future.apply)
plan(multisession(workers = 12))
library(progress)

source("functions/get_gamestate.R")
source("functions/predict_dynamic_model.R")
load("data/models.RData")
load("raw-data/results.RData")
load("raw-data/intervals.RData")
load("data/matches_to_ignore.RData")

# id of matches that will be simulated
matches_to_simulate = results %>%
  anti_join(matches_to_ignore) %>%
  .$match

if(file.exists("data/simulations.RData")) {
  load("data/simulations.RData")
} else {
  simulations = list()
}

pb = progress_bar$new(format = "[:bar] :percent eta: :eta", total = length(matches_to_simulate))
for(i in (length(simulations)+1):length(matches_to_simulate)) {
  # print(paste("Partida", i, "de", length(matches_to_simulate)))
  
  match_info = results %>%
    filter(match == matches_to_simulate[i])
  
  state_15 = get_gamestate(id = matches_to_simulate[i],
                           hlf = 1,
                           min = 15)
  
  state_30 = get_gamestate(id = matches_to_simulate[i],
                           hlf = 1,
                           min = 30)
  
  state_45 = get_gamestate(id = matches_to_simulate[i],
                           hlf = 2,
                           min = 0)
  
  state_60 = get_gamestate(id = matches_to_simulate[i],
                           hlf = 2,
                           min = 15)
  
  state_75 = get_gamestate(id = matches_to_simulate[i],
                           hlf = 2,
                           min = 30)
  
  # Model 0
  mod0 = list()
  mod0[["0"]] = predict_dynamic_model(mod = models$`0`[[match_info$date]],
                                      home_team = match_info$home_team,
                                      away_team = match_info$away_team,
                                      n = 10^5,
                                      home_team_market_value = match_info$home_team_market_value,
                                      away_team_market_value = match_info$away_team_market_value)
  
  mod0[["15"]] = predict_dynamic_model(mod = models$`0`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_15["home_goals"],
                                       away_goals = state_15["away_goals"],
                                       home_red_cards_1 = state_15["home_red_cards_1"],
                                       away_red_cards_1 = state_15["away_red_cards_1"],
                                       minute = 15)
  
  mod0[["30"]] = predict_dynamic_model(mod = models$`0`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_30["home_goals"],
                                       away_goals = state_30["away_goals"],
                                       home_red_cards_1 = state_30["home_red_cards_1"],
                                       away_red_cards_1 = state_30["away_red_cards_1"],
                                       minute = 30)
  
  mod0[["45"]] = predict_dynamic_model(mod = models$`0`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_45["home_goals"],
                                       away_goals = state_45["away_goals"],
                                       home_red_cards_1 = state_45["home_red_cards_1"],
                                       away_red_cards_1 = state_45["away_red_cards_1"],
                                       minute = 0,
                                       half = 2,
                                       stoppage_time_1 = match_info$st1)
  
  mod0[["60"]] = predict_dynamic_model(mod = models$`0`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_60["home_goals"],
                                       away_goals = state_60["away_goals"],
                                       home_red_cards_1 = state_60["home_red_cards_1"],
                                       away_red_cards_1 = state_60["away_red_cards_1"],
                                       home_red_cards_2 = state_60["home_red_cards_2"],
                                       away_red_cards_2 = state_60["away_red_cards_2"],
                                       minute = 15,
                                       half = 2,
                                       stoppage_time_1 = match_info$st1)
  
  mod0[["75"]] = predict_dynamic_model(mod = models$`0`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_75["home_goals"],
                                       away_goals = state_75["away_goals"],
                                       home_red_cards_1 = state_75["home_red_cards_1"],
                                       away_red_cards_1 = state_75["away_red_cards_1"],
                                       home_red_cards_2 = state_75["home_red_cards_2"],
                                       away_red_cards_2 = state_75["away_red_cards_2"],
                                       minute = 30,
                                       half = 2,
                                       stoppage_time_1 = match_info$st1)
  
  # Model 1
  mod1 = list()
  mod1[["0"]] = predict_dynamic_model(mod = models$`1`[[match_info$date]],
                                      home_team = match_info$home_team,
                                      away_team = match_info$away_team,
                                      n = 10^5,
                                      home_team_market_value = match_info$home_team_market_value,
                                      away_team_market_value = match_info$away_team_market_value)
  
  mod1[["15"]] = predict_dynamic_model(mod = models$`1`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_15["home_goals"],
                                       away_goals = state_15["away_goals"],
                                       home_red_cards_1 = state_15["home_red_cards_1"],
                                       away_red_cards_1 = state_15["away_red_cards_1"],
                                       minute = 15)
  
  mod1[["30"]] = predict_dynamic_model(mod = models$`1`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_30["home_goals"],
                                       away_goals = state_30["away_goals"],
                                       home_red_cards_1 = state_30["home_red_cards_1"],
                                       away_red_cards_1 = state_30["away_red_cards_1"],
                                       minute = 30)
  
  mod1[["45"]] = predict_dynamic_model(mod = models$`1`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_45["home_goals"],
                                       away_goals = state_45["away_goals"],
                                       home_red_cards_1 = state_45["home_red_cards_1"],
                                       away_red_cards_1 = state_45["away_red_cards_1"],
                                       minute = 0,
                                       half = 2,
                                       stoppage_time_1 = match_info$st1)
  
  mod1[["60"]] = predict_dynamic_model(mod = models$`1`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_60["home_goals"],
                                       away_goals = state_60["away_goals"],
                                       home_red_cards_1 = state_60["home_red_cards_1"],
                                       away_red_cards_1 = state_60["away_red_cards_1"],
                                       home_red_cards_2 = state_60["home_red_cards_2"],
                                       away_red_cards_2 = state_60["away_red_cards_2"],
                                       minute = 15,
                                       half = 2,
                                       stoppage_time_1 = match_info$st1)
  
  mod1[["75"]] = predict_dynamic_model(mod = models$`1`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_75["home_goals"],
                                       away_goals = state_75["away_goals"],
                                       home_red_cards_1 = state_75["home_red_cards_1"],
                                       away_red_cards_1 = state_75["away_red_cards_1"],
                                       home_red_cards_2 = state_75["home_red_cards_2"],
                                       away_red_cards_2 = state_75["away_red_cards_2"],
                                       minute = 30,
                                       half = 2,
                                       stoppage_time_1 = match_info$st1)
  
  # Model 2
  mod2 = list()
  mod2[["0"]] = predict_dynamic_model(mod = models$`2`[[match_info$date]],
                                      home_team = match_info$home_team,
                                      away_team = match_info$away_team,
                                      n = 10^5,
                                      home_team_market_value = match_info$home_team_market_value,
                                      away_team_market_value = match_info$away_team_market_value)
  
  mod2[["15"]] = predict_dynamic_model(mod = models$`2`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_15["home_goals"],
                                       away_goals = state_15["away_goals"],
                                       home_red_cards_1 = state_15["home_red_cards_1"],
                                       away_red_cards_1 = state_15["away_red_cards_1"],
                                       minute = 15)
  
  mod2[["30"]] = predict_dynamic_model(mod = models$`2`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_30["home_goals"],
                                       away_goals = state_30["away_goals"],
                                       home_red_cards_1 = state_30["home_red_cards_1"],
                                       away_red_cards_1 = state_30["away_red_cards_1"],
                                       minute = 30)
  
  mod2[["45"]] = predict_dynamic_model(mod = models$`2`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_45["home_goals"],
                                       away_goals = state_45["away_goals"],
                                       home_red_cards_1 = state_45["home_red_cards_1"],
                                       away_red_cards_1 = state_45["away_red_cards_1"],
                                       minute = 0,
                                       half = 2,
                                       stoppage_time_1 = match_info$st1)
  
  mod2[["60"]] = predict_dynamic_model(mod = models$`2`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_60["home_goals"],
                                       away_goals = state_60["away_goals"],
                                       home_red_cards_1 = state_60["home_red_cards_1"],
                                       away_red_cards_1 = state_60["away_red_cards_1"],
                                       home_red_cards_2 = state_60["home_red_cards_2"],
                                       away_red_cards_2 = state_60["away_red_cards_2"],
                                       minute = 15,
                                       half = 2,
                                       stoppage_time_1 = match_info$st1)
  
  mod2[["75"]] = predict_dynamic_model(mod = models$`2`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_75["home_goals"],
                                       away_goals = state_75["away_goals"],
                                       home_red_cards_1 = state_75["home_red_cards_1"],
                                       away_red_cards_1 = state_75["away_red_cards_1"],
                                       home_red_cards_2 = state_75["home_red_cards_2"],
                                       away_red_cards_2 = state_75["away_red_cards_2"],
                                       minute = 30,
                                       half = 2,
                                       stoppage_time_1 = match_info$st1)
  
  # Model 3
  mod3 = list()
  mod3[["0"]] = predict_dynamic_model(mod = models$`3`[[match_info$date]],
                                      home_team = match_info$home_team,
                                      away_team = match_info$away_team,
                                      n = 10^5,
                                      home_team_market_value = match_info$home_team_market_value,
                                      away_team_market_value = match_info$away_team_market_value)
  
  mod3[["15"]] = predict_dynamic_model(mod = models$`3`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_15["home_goals"],
                                       away_goals = state_15["away_goals"],
                                       home_red_cards_1 = state_15["home_red_cards_1"],
                                       away_red_cards_1 = state_15["away_red_cards_1"],
                                       minute = 15)
  
  mod3[["30"]] = predict_dynamic_model(mod = models$`3`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_30["home_goals"],
                                       away_goals = state_30["away_goals"],
                                       home_red_cards_1 = state_30["home_red_cards_1"],
                                       away_red_cards_1 = state_30["away_red_cards_1"],
                                       minute = 30)
  
  mod3[["45"]] = predict_dynamic_model(mod = models$`3`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_45["home_goals"],
                                       away_goals = state_45["away_goals"],
                                       home_red_cards_1 = state_45["home_red_cards_1"],
                                       away_red_cards_1 = state_45["away_red_cards_1"],
                                       minute = 0,
                                       half = 2,
                                       stoppage_time_1 = match_info$st1)
  
  mod3[["60"]] = predict_dynamic_model(mod = models$`3`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_60["home_goals"],
                                       away_goals = state_60["away_goals"],
                                       home_red_cards_1 = state_60["home_red_cards_1"],
                                       away_red_cards_1 = state_60["away_red_cards_1"],
                                       home_red_cards_2 = state_60["home_red_cards_2"],
                                       away_red_cards_2 = state_60["away_red_cards_2"],
                                       minute = 15,
                                       half = 2,
                                       stoppage_time_1 = match_info$st1)
  
  mod3[["75"]] = predict_dynamic_model(mod = models$`3`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_75["home_goals"],
                                       away_goals = state_75["away_goals"],
                                       home_red_cards_1 = state_75["home_red_cards_1"],
                                       away_red_cards_1 = state_75["away_red_cards_1"],
                                       home_red_cards_2 = state_75["home_red_cards_2"],
                                       away_red_cards_2 = state_75["away_red_cards_2"],
                                       minute = 30,
                                       half = 2,
                                       stoppage_time_1 = match_info$st1)
  
  # Model 4
  mod4 = list()
  mod4[["0"]] = predict_dynamic_model(mod = models$`4`[[match_info$date]],
                                      home_team = match_info$home_team,
                                      away_team = match_info$away_team,
                                      n = 10^5,
                                      home_team_market_value = match_info$home_team_market_value,
                                      away_team_market_value = match_info$away_team_market_value)
  
  mod4[["15"]] = predict_dynamic_model(mod = models$`4`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_15["home_goals"],
                                       away_goals = state_15["away_goals"],
                                       home_red_cards_1 = state_15["home_red_cards_1"],
                                       away_red_cards_1 = state_15["away_red_cards_1"],
                                       minute = 15)
  
  mod4[["30"]] = predict_dynamic_model(mod = models$`4`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_30["home_goals"],
                                       away_goals = state_30["away_goals"],
                                       home_red_cards_1 = state_30["home_red_cards_1"],
                                       away_red_cards_1 = state_30["away_red_cards_1"],
                                       minute = 30)
  
  mod4[["45"]] = predict_dynamic_model(mod = models$`4`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_45["home_goals"],
                                       away_goals = state_45["away_goals"],
                                       home_red_cards_1 = state_45["home_red_cards_1"],
                                       away_red_cards_1 = state_45["away_red_cards_1"],
                                       minute = 0,
                                       half = 2,
                                       stoppage_time_1 = match_info$st1)
  
  mod4[["60"]] = predict_dynamic_model(mod = models$`4`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_60["home_goals"],
                                       away_goals = state_60["away_goals"],
                                       home_red_cards_1 = state_60["home_red_cards_1"],
                                       away_red_cards_1 = state_60["away_red_cards_1"],
                                       home_red_cards_2 = state_60["home_red_cards_2"],
                                       away_red_cards_2 = state_60["away_red_cards_2"],
                                       minute = 15,
                                       half = 2,
                                       stoppage_time_1 = match_info$st1)
  
  mod4[["75"]] = predict_dynamic_model(mod = models$`4`[[match_info$date]],
                                       home_team = match_info$home_team,
                                       away_team = match_info$away_team,
                                       n = 10^5,
                                       home_team_market_value = match_info$home_team_market_value,
                                       away_team_market_value = match_info$away_team_market_value,
                                       home_goals = state_75["home_goals"],
                                       away_goals = state_75["away_goals"],
                                       home_red_cards_1 = state_75["home_red_cards_1"],
                                       away_red_cards_1 = state_75["away_red_cards_1"],
                                       home_red_cards_2 = state_75["home_red_cards_2"],
                                       away_red_cards_2 = state_75["away_red_cards_2"],
                                       minute = 30,
                                       half = 2,
                                       stoppage_time_1 = match_info$st1)
  
  simulations[[i]] = list("0" = mod0, "1" = mod1, "2" = mod2, "3" = mod3, "4" = mod4)
  names(simulations)[i] = matches_to_simulate[i]
  
  if(i %% 5 == 0) {
    print("Saving.")
    save(simulations, file = "data/simulations.RData")
  }
  pb$tick()
}

save(simulations, file = "data/simulations.RData")