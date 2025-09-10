
fit_st_model <- function(intervals, csi = 0, estimation_date = Sys.Date(), intercept = TRUE, goals = FALSE, red_cards = TRUE, close_match = TRUE, same_intercept = FALSE, same_goals = FALSE, same_red_cards = FALSE) {
  
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
  
  st$current_home_goals_45_1 = first_half_45$current_home_goals
  st$current_away_goals_45_1 = first_half_45$current_away_goals
  
  st$current_home_players_0_2 = second_half_0$current_home_players
  st$current_away_players_0_2 = second_half_0$current_away_players
  
  st$current_home_goals_0_2 = second_half_0$current_home_goals
  st$current_away_goals_0_2 = second_half_0$current_away_goals
  
  st$current_home_goals_45_2 = second_half_45$current_home_goals
  st$current_away_goals_45_2 = second_half_45$current_away_goals
  
  st$current_home_players_45_2 = second_half_45$current_home_players
  st$current_away_players_45_2 = second_half_45$current_away_players
  
  st = st %>%
    mutate(goals_1 = current_home_goals_45_1 + current_away_goals_45_1,
           goals_2 = current_home_goals_45_2 + current_away_goals_45_2 - current_home_goals_0_2 - current_away_goals_0_2,
           red_cards_1 = 22 - current_home_players_45_1 - current_away_players_45_1,
           red_cards_2 = current_home_players_0_2 + current_away_players_0_2 - current_home_players_45_2 - current_away_players_45_2,
           close_match = ifelse(abs(current_home_goals_45_2 - current_away_goals_45_2) <= 1, 1, 0))
  
  st1_parameters = Variable(3) # intercept / goals / red_cards
  st2_parameters = Variable(4) # intercept / goals / red_cards / close_match
  
  log_pi1 = st1_parameters[1] + st1_parameters[2]*st$goals_1 + st1_parameters[3]*st$red_cards_1
  log_pi2 = st2_parameters[1] + st2_parameters[2]*st$goals_2 + st2_parameters[3]*st$red_cards_2 + st2_parameters[4]*st$close_match
  
  U1 = st$st1
  U2 = st$st2
  w_st = st$weight
  
  log_lik_st = sum_entries(w_st * (U1 * log_pi1 + U2 * log_pi2 - exp(log_pi1) - exp(log_pi2) - log(factorial(U1)) - log(factorial(U2))))
  
  objective = Maximize(log_lik_st)
  
  constraints = list()
  if(!intercept) {
    constraints = append(constraints, st1_parameters[1] == 0)
    constraints = append(constraints, st2_parameters[1] == 0)
  } 
  if(!goals) {
    constraints = append(constraints, st1_parameters[2] == 0)
    constraints = append(constraints, st2_parameters[2] == 0)
  } 
  if(!red_cards) {
    constraints = append(constraints, st1_parameters[3] == 0)
    constraints = append(constraints, st2_parameters[3] == 0)
  } 
  if(!close_match) {
    constraints = append(constraints, st2_parameters[4] == 0)
  } 
  if(same_intercept) {
    constraints = append(constraints, st1_parameters[1] == st2_parameters[1])
  } 
  if(same_goals) {
    constraints = append(constraints, st1_parameters[2] == st2_parameters[2])
  } 
  if(same_red_cards) {
    constraints = append(constraints, st1_parameters[3] == st2_parameters[3])
  } 
  
  problem = Problem(objective, constraints)
  solution = solve(problem, solver = "MOSEK")
  
  mod = list(st1_parameters = as.vector(solution$getValue(st1_parameters)),
             st2_parameters = as.vector(solution$getValue(st2_parameters)),
             loglik = solution$value)
  names(mod$st1_parameters) = c("intercept", "goals", "red_cards")
  names(mod$st2_parameters) = c("intercept", "goals", "red_cards", "close_match")
  
  return(mod)
}