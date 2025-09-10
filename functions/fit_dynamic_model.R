
fit_dynamic_model <- function(intervals, csi = 0, estimation_date = Sys.Date(), goal_model_only = FALSE, par_log_market_value_difference = TRUE, par_second_half = TRUE, par_goal_difference = TRUE, par_player_difference = TRUE) {
  
  options(dplyr.summarise.inform = FALSE)
  
  library(dplyr)
  library(CVXR)
  
  intervals = intervals %>%
    filter(date < estimation_date)
  
  n = length(unique(c(intervals$home_team, intervals$away_team)))
  N = max(intervals$match) 
  l = nrow(intervals)
  
  # Stoppage time
  st1 = intervals %>%
    filter(half == 1) %>%
    group_by(match, date) %>%
    summarise(st1 = max(end) - 45)
  
  st2 = intervals %>%
    filter(half == 2) %>%
    group_by(match, date) %>%
    summarise(st2 = max(end) - 45)
  
  st = full_join(st1, st2, by = c("match", "date")) %>%
    mutate(diff = as.integer(difftime(as.Date(estimation_date, "%Y-%m-%d"), as.Date(date, "%Y-%m-%d"), units = "days")),
           weight = exp(- csi * diff))
  
  first_half_45 = intervals %>%
    filter(half == 1) %>%
    group_by(match) %>%
    rowwise() %>%
    filter(45 %in% seq(from = begin + 1, to = end, by = 1))
  
  second_half_0 = intervals %>%
    filter(half == 2) %>%
    group_by(match) %>%
    filter(begin == 0)
  
  second_half_45 = intervals %>%
    filter(half == 2) %>%
    group_by(match) %>%
    rowwise() %>%
    filter(45 %in% seq(from = begin + 1, to = end, by = 1))
  
  st$current_home_players_45_1 = first_half_45$current_home_players
  st$current_away_players_45_1 = first_half_45$current_away_players
  
  st$current_home_players_0_2 = second_half_0$current_home_players
  st$current_away_players_0_2 = second_half_0$current_away_players
  
  st$current_home_goals_45_2 = second_half_45$current_home_goals
  st$current_away_goals_45_2 = second_half_45$current_away_goals
  
  st$current_home_players_45_2 = second_half_45$current_home_players
  st$current_away_players_45_2 = second_half_45$current_away_players
  
  st = st %>%
    mutate(red_cards_1 = 22 - current_home_players_45_1 - current_away_players_45_1,
           red_cards_2 = current_home_players_0_2 + current_away_players_0_2 - current_home_players_45_2 - current_away_players_45_2,
           close_match = ifelse(abs(current_home_goals_45_2 - current_away_goals_45_2) <= 1, 1, 0))
  
  st1_parameters = Variable(2) # intercept / red_cards
  st2_parameters = Variable(3) # intercept / red_cards / close_match
  
  log_pi1 = st1_parameters[1] + st1_parameters[2] * st$red_cards_1
  log_pi2 = st2_parameters[1] + st2_parameters[2] * st$red_cards_2 + st2_parameters[3] * st$close_match
  
  U1 = st$st1
  U2 = st$st2
  w_st = st$weight
  
  log_lik_st = sum_entries(w_st * (U1 * log_pi1 + U2 * log_pi2 - exp(log_pi1) - exp(log_pi2) - log(factorial(U1)) - log(factorial(U2))))
  
  # Goals
  alpha = Variable(n)
  beta = Variable(n)
  goal_parameters = Variable(5) # home_advantage / log_market_value_diff / half / goal_diff / player_diff
  theta = vstack(alpha, beta, goal_parameters)
  
  teams = tibble(id = 1:length(unique(c(intervals$home_team, intervals$away_team))), team = sort(unique(c(intervals$home_team, intervals$away_team))))
  
  cols_home = paste("home_team", teams$team, sep = "_")
  cols_away = paste("away_team", teams$team, sep = "_")
  
  dummies_home = fastDummies::dummy_cols(intervals, c("home_team")) %>%
    .[,-(1:ncol(intervals))] %>%
    as.matrix()
  new_columns = setdiff(cols_home, colnames(dummies_home)) # Needed mainly for the second rounds of each season
  for(c in new_columns) {
    dummies_home = cbind(dummies_home, rep(0, l))
    colnames(dummies_home)[ncol(dummies_home)] = c
  }
  dummies_home = dummies_home[, order(colnames(dummies_home))]
  
  dummies_away = fastDummies::dummy_cols(intervals, c("away_team")) %>%
    .[,-(1:ncol(intervals))] %>%
    as.matrix()
  new_columns = setdiff(cols_away, colnames(dummies_away))
  for(c in new_columns) {
    dummies_away = cbind(dummies_away, rep(0, l))
    colnames(dummies_away)[ncol(dummies_away)] = c
  }
  dummies_away = dummies_away[, order(colnames(dummies_away))]
  
  dummies_home_away = cbind(dummies_home, dummies_away)
  colnames(dummies_home_away) = c(paste0("atk_", 1:n), paste0("def_", 1:n))
  
  dummies_away_home = cbind(dummies_away, dummies_home)
  colnames(dummies_away_home) = c(paste0("atk_", 1:n), paste0("def_", 1:n))
  
  home_advantage = c(rep(1, l), rep(0, l))
  half = rep(intervals$half, 2) - 1 # {1,2} -> {0,1}
  goal_diff = c(intervals$current_home_goals - intervals$current_away_goals, intervals$current_away_goals - intervals$current_home_goals)
  player_diff = c(intervals$current_home_players - intervals$current_away_players, intervals$current_away_players - intervals$current_home_players)
  
  intervals = intervals %>%
    mutate(home_diff_log_market_value = log(home_team_market_value) - log(away_team_market_value),
           away_diff_log_market_value = log(away_team_market_value) - log(home_team_market_value))
  
  log_market_value_diff = c(intervals$home_diff_log_market_value, intervals$away_diff_log_market_value)
  
  M_goals = rbind(dummies_home_away, dummies_away_home) %>%
    cbind(home_advantage, log_market_value_diff, half, goal_diff, player_diff)
  
  intervals = intervals %>%
    mutate(diff = as.integer(difftime(as.Date(estimation_date, "%Y-%m-%d"), as.Date(date, "%Y-%m-%d"), units = "days")),
           weight = exp(- csi * diff))
  
  w_goals = rep(intervals$weight, 2)
  goals = c(intervals$home_goal, intervals$away_goal)
  I_goals = rep(intervals$length, 2)
  lambda = M_goals %*% theta
  
  log_lik_goals = sum_entries(w_goals * (-I_goals * exp(lambda) + lambda * goals))
  
  # Red cards
  home_red_card_parameters = Variable(2) # constant / logarithmic
  away_red_card_parameters = Variable(2) # constant / logarithmic
  
  aux_intervals = tibble(match = unique(intervals$match), U1, U2)
  
  intervals = intervals %>%
    left_join(aux_intervals, by = "match") %>%
    mutate(begin2 = ifelse(half == 2, begin + 45 + U1, begin),
           end2 = ifelse(half == 2, end + 45 + U1, end))
  
  w_red_cards = intervals$weight
  I_red_cards = intervals$I_red_cards
  t = intervals$end2
  home_red_card = intervals$home_red_card
  away_red_card = intervals$away_red_card
  
  log_lik_home_red_cards_1 = sum_entries(-w_st * (exp(home_red_card_parameters[1] + (home_red_card_parameters[2]+1) * log(90+U1+U2) - log(home_red_card_parameters[2]+1))))
  log_lik_away_red_cards_1 = sum_entries(-w_st * (exp(away_red_card_parameters[1] + (away_red_card_parameters[2]+1) * log(90+U1+U2) - log(away_red_card_parameters[2]+1))))
  
  log_lik_home_red_cards_2 = sum_entries(w_red_cards * home_red_card * (home_red_card_parameters[1] + home_red_card_parameters[2] * log(t)))
  log_lik_away_red_cards_2 = sum_entries(w_red_cards * away_red_card * (away_red_card_parameters[1] + away_red_card_parameters[2] * log(t)))
  
  log_lik_red_cards = log_lik_home_red_cards_1 + log_lik_home_red_cards_2 + log_lik_away_red_cards_1 + log_lik_away_red_cards_2
  
  # Fitting the model
  constraints = list(sum(alpha) - sum(beta) == 0)
  if(!par_log_market_value_difference) {
    constraints = append(constraints, goal_parameters[2] == 0)
  }
  if(!par_second_half) {
    constraints = append(constraints, goal_parameters[3] == 0)
  }
  if(!par_goal_difference) {
    constraints = append(constraints, goal_parameters[4] == 0)
  }
  if(!par_player_difference) {
    constraints = append(constraints, goal_parameters[5] == 0)
  }
  
  if(goal_model_only) {
    log_lik = log_lik_goals
    
    objective = Maximize(log_lik)
    
    problem = Problem(objective, constraints)
    solution = solve(problem, solver = "MOSEK")
    
    mod = list(alpha = as.vector(c(solution$getValue(alpha))),
               beta = as.vector(solution$getValue(beta)),
               goal_parameters = as.vector(solution$getValue(goal_parameters)),
               loglik = solution$value)
    names(mod$alpha) = teams$team
    names(mod$beta) = teams$team
    names(mod$goal_parameters) = c("home_advantage", "log_market_value_difference", "second_half", "goal_difference", "player_difference")
    
  } else {
    log_lik = log_lik_goals + log_lik_st + log_lik_red_cards
    
    objective = Maximize(log_lik)
    
    problem = Problem(objective, constraints)
    solution = solve(problem, solver = "MOSEK")
    
    if(solution$status == "solver_error") {
      solution = solve(problem, solver = "ECOS")
      message(paste0("MOSEK apresentou solver_error. Tentando com ECOS. ECOS teve solution$status: ", solution$status, "."))
    }
    
    mod = list(alpha = as.vector(c(solution$getValue(alpha))),
               beta = as.vector(solution$getValue(beta)),
               goal_parameters = as.vector(solution$getValue(goal_parameters)),
               home_red_card_parameters = as.vector(solution$getValue(home_red_card_parameters)),
               away_red_card_parameters = as.vector(solution$getValue(away_red_card_parameters)),
               st1_parameters = as.vector(solution$getValue(st1_parameters)),
               st2_parameters = as.vector(solution$getValue(st2_parameters)),
               loglik = solution$value)
    names(mod$alpha) = teams$team
    names(mod$beta) = teams$team
    names(mod$home_red_card_parameters) = c("constant", "logarithmic")
    names(mod$away_red_card_parameters) = c("constant", "logarithmic")
    names(mod$goal_parameters) = c("home_advantage", "log_market_value_difference", "second_half", "goal_difference", "player_difference")
    names(mod$st1_parameters) = c("intercept", "red_cards")
    names(mod$st2_parameters) = c("intercept", "red_cards", "close_match")
  }
  
  return(mod)
}
