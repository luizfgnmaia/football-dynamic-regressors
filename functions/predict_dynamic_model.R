
# library(future.apply)
# plan(multisession(workers = 12))

predict_dynamic_model <- function(mod, home_team, away_team, n = 10^5, home_team_market_value = NA, away_team_market_value = NA, home_goals = 0, away_goals = 0, home_red_cards_1 = 0, away_red_cards_1 = 0, home_red_cards_2 = 0, away_red_cards_2 = 0, minute = 0, half = 1, end_minute = 45, end_half = 2, stoppage_time = TRUE, stoppage_time_1 = NA) {
  
  if(!is.numeric(n) | n <= 0) {
    stop("Invalid n.")
  }
  
  n = as.integer(ceiling(n))
  
  if(!home_team %in% names(mod$alpha)) {
    stop("Invalid home team.")
  }
  
  if(!away_team %in% names(mod$alpha)) {
    stop("Invalid away team.")
  }
  
  if(!is.na(home_team_market_value)) {
    if(!is.numeric(home_team_market_value) | home_team_market_value < 0) {
      stop("Invalid home_team_market_value.")
    }
  }
  
  if(!is.na(away_team_market_value)) {
    if(!is.numeric(away_team_market_value) | away_team_market_value < 0) {
      stop("Invalid away_team_market_value.")
    }
  }
  
  if(!is.numeric(home_goals) | home_goals < 0) {
    stop("Invalid home_goals.")
  }
  
  if(!is.numeric(away_goals) | away_goals < 0) {
    stop("Invalid away_goals.")
  }
  
  if(!is.numeric(home_red_cards_1) | home_red_cards_1 < 0) {
    stop("Invalid home_red_cards_1.")
  }
  
  if(!is.numeric(away_red_cards_1) | away_red_cards_1 < 0) {
    stop("Invalid away_red_cards_1.")
  }
  
  if(!is.numeric(home_red_cards_2) | home_red_cards_2 < 0) {
    stop("Invalid home_red_cards_1.")
  }
  
  if(!is.numeric(away_red_cards_2) | away_red_cards_2 < 0) {
    stop("Invalid away_red_cards_2.")
  }
  
  if(minute > 45 | minute < 0) {
    stop("Invalid minute.")
  }
  
  if(!half %in% c(1,2)) {
    stop("Invalid half.")
  }
  
  if((end_minute > 45 | end_minute < 0)) {
    stop("Invalid end_minute.")
  }
  
  if(!end_half %in% c(1,2)) {
    stop("Invalid end_half.")
  }
  
  if(!is.logical(stoppage_time)) {
    stop("stoppage_time must be boolean.")
  }
  
  if(end_half < half) {
    stop("end_half can't be smaller than half.")
  } else if(end_half == half) {
    if(end_minute < minute) {
      stop("end_minute needs to be bigger than minute.")
    } else if(end_minute == minute & stoppage_time == FALSE) {
      stop("end_minute needs to be bigger than minute or stoppage_time needs to be TRUE.")
    }
  }
  
  # Function that calculates the integral of the red card model rate
  red_card_int <- function(x, par_const, par_log) {
    exp(par_const + (par_log + 1)*log(x) - log(par_log + 1))
  }
  
  # Function that calculates the inverse of the integral of the red card model rate (Çinlar's Method)
  inv_red_card_int <- function(x, par_const, par_log) {
    ((x*(par_log+1))/(exp(par_const)))^(1/(par_log+1))
  }
  
  # Auxiliary function that simulates a match
  pred <- function(home_team, away_team, home_team_market_value, away_team_market_value, home_goals, away_goals, home_red_cards_1, away_red_cards_1, home_red_cards_2, away_red_cards_2, minute, half, end_minute, end_half, stoppage_time, stoppage_time_1) {
    
    # minute is the minute from 0 to 45
    # minute2 is the minute from 0 to 90
    minute2 = minute + ifelse(half == 1, 0, 45) + ifelse(is.na(stoppage_time_1), 0, stoppage_time_1) 
    
    home_red_cards = home_red_cards_1 + home_red_cards_2
    away_red_cards = away_red_cards_1 + away_red_cards_2
    
    log_market_value_difference = ifelse(is.na(home_team_market_value) | is.na(away_team_market_value),
                                         0,
                                         log(home_team_market_value) - log(away_team_market_value))
    
    if(end_minute < 45) {
      stoppage_time = FALSE
    }
    
    if(end_half == 1) {
      half = 1
    }
    
    if(half == 2) {
      end_half = 2
    }
    
    # Generating expulsions for the home team until the end of the game
    t = minute2
    s = red_card_int(t, mod$home_red_card_parameters["constant"], mod$home_red_card_parameters["logarithmic"])
    t_home_red_cards = NULL
    while(t < 150) { 
      u = runif(1)
      s = s - log(u)
      t = inv_red_card_int(s, mod$home_red_card_parameters["constant"], mod$home_red_card_parameters["logarithmic"])
      if(t < 150) {
        t_home_red_cards = c(t_home_red_cards, t)
      }
    }
    
    # Generating expulsions for the away team until the end of the game
    t = minute2
    s = red_card_int(t, mod$away_red_card_parameters["constant"], mod$away_red_card_parameters["logarithmic"])
    t_away_red_cards = NULL
    while(t < 150) { 
      u = runif(1)
      s = s - log(u)
      t = inv_red_card_int(s, mod$away_red_card_parameters["constant"], mod$away_red_card_parameters["logarithmic"])
      if(t < 150) {
        t_away_red_cards = c(t_away_red_cards, t)
      }
    }
    
    # Generating goals in regular time of the first half
    if(half == 1) {
      
      if(end_half == 1) {
        end_1st = end_minute
      } else {
        end_1st = 45
      }
      
      while(minute < end_1st) {
        lambda = exp(mod$alpha[home_team] + mod$beta[away_team] + mod$goal_parameters["home_advantage"] + mod$goal_parameters["goal_difference"]*(home_goals-away_goals) + mod$goal_parameters["player_difference"]*(away_red_cards-home_red_cards) + mod$goal_parameters["log_market_value_difference"]*log_market_value_difference)
        mu = exp(mod$alpha[away_team] + mod$beta[home_team] + mod$goal_parameters["goal_difference"]*(away_goals-home_goals) + mod$goal_parameters["player_difference"]*(home_red_cards-away_red_cards) - mod$goal_parameters["log_market_value_difference"]*log_market_value_difference)
        next_home_goal = rexp(1, rate = lambda) + minute
        next_away_goal = rexp(1, rate = mu) + minute
        next_home_red = t_home_red_cards[t_home_red_cards > minute][1]
        next_home_red = ifelse(is.null(next_home_red), Inf, next_home_red)
        next_home_red = ifelse(is.na(next_home_red), Inf, next_home_red)
        next_away_red = t_away_red_cards[t_away_red_cards > minute][1]
        next_away_red = ifelse(is.null(next_away_red), Inf, next_away_red)
        next_away_red = ifelse(is.na(next_away_red), Inf, next_away_red)
        next_event = min(next_home_goal, next_away_goal, next_home_red, next_away_red)
        
        if(next_event < end_1st) {
          minute = next_event
          if(next_home_goal == minute) {
            home_goals = home_goals + 1
          } else if(next_away_goal == minute) {
            away_goals = away_goals + 1
          } else if(next_home_red == minute) {
            home_red_cards = home_red_cards + 1
          } else {
            away_red_cards = away_red_cards + 1
          }
        } else {
          minute = end_1st
        }
      }
      
      # Generating goals in stoppage time of the first half
      if(stoppage_time == TRUE | end_half == 2) {
        
        U1 = rpois(1, lambda = exp(mod$st1_parameters["intercept"] + mod$st1_parameters["red_cards"]*(home_red_cards + away_red_cards)))
        stoppage_time_1 = U1
        
        while(minute < 45 + U1) {
          lambda = exp(mod$alpha[home_team] + mod$beta[away_team] + mod$goal_parameters["home_advantage"] + mod$goal_parameters["goal_difference"]*(home_goals-away_goals) + mod$goal_parameters["player_difference"]*(away_red_cards-home_red_cards) + mod$goal_parameters["log_market_value_difference"]*log_market_value_difference)
          mu = exp(mod$alpha[away_team] + mod$beta[home_team] + mod$goal_parameters["goal_difference"]*(away_goals-home_goals) + mod$goal_parameters["player_difference"]*(home_red_cards-away_red_cards) - mod$goal_parameters["log_market_value_difference"]*log_market_value_difference)
          next_home_goal = rexp(1, rate = lambda) + minute
          next_away_goal = rexp(1, rate = mu) + minute
          next_home_red = t_home_red_cards[t_home_red_cards > minute][1]
          next_home_red = ifelse(is.null(next_home_red), Inf, next_home_red)
          next_home_red = ifelse(is.na(next_home_red), Inf, next_home_red)
          next_away_red = t_away_red_cards[t_away_red_cards > minute][1]
          next_away_red = ifelse(is.null(next_away_red), Inf, next_away_red)
          next_away_red = ifelse(is.na(next_away_red), Inf, next_away_red)
          next_event = min(next_home_goal, next_away_goal, next_home_red, next_away_red)
          
          if(next_event < 45 + U1) {
            minute = next_event
            if(next_home_goal == minute) {
              home_goals = home_goals + 1
            } else if(next_away_goal == minute) {
              away_goals = away_goals + 1
            } else if(next_home_red == minute) {
              home_red_cards = home_red_cards + 1
            } else {
              away_red_cards = away_red_cards + 1
            }
          } else {
            minute = 45 + U1
            half = 2
          }
        }
        minute = 0
        home_red_cards_1 = home_red_cards
        away_red_cards_1 = away_red_cards
      }
    }
    
    # Generating goals in regular time of the second half
    if(end_half == 2) {
      
      end_2nd = end_minute
      
      while(minute < end_2nd) {
        
        minute2 = minute + 45 + stoppage_time_1
        lambda = exp(mod$alpha[home_team] + mod$beta[away_team] + mod$goal_parameters["home_advantage"] + mod$goal_parameters["goal_difference"]*(home_goals-away_goals) + mod$goal_parameters["player_difference"]*(away_red_cards-home_red_cards) + mod$goal_parameters["second_half"] + mod$goal_parameters["log_market_value_difference"]*log_market_value_difference)
        mu = exp(mod$alpha[away_team] + mod$beta[home_team] + mod$goal_parameters["goal_difference"]*(away_goals-home_goals) + mod$goal_parameters["player_difference"]*(home_red_cards-away_red_cards) + mod$goal_parameters["second_half"] - mod$goal_parameters["log_market_value_difference"]*log_market_value_difference)
        next_home_goal = rexp(1, rate = lambda) + minute
        next_away_goal = rexp(1, rate = mu) + minute
        next_home_red = t_home_red_cards[t_home_red_cards > minute2][1] - 45 - stoppage_time_1
        next_home_red = ifelse(is.null(next_home_red), Inf, next_home_red)
        next_home_red = ifelse(is.na(next_home_red), Inf, next_home_red)
        next_away_red = t_away_red_cards[t_away_red_cards > minute2][1] - 45 - stoppage_time_1
        next_away_red = ifelse(is.null(next_away_red), Inf, next_away_red)
        next_away_red = ifelse(is.na(next_away_red), Inf, next_away_red)
        next_event = min(next_home_goal, next_away_goal, next_home_red, next_away_red)
        
        if(next_event < end_2nd) {
          minute = next_event
          if(next_home_goal == minute) {
            home_goals = home_goals + 1
          } else if(next_away_goal == minute) {
            away_goals = away_goals + 1
          } else if(next_home_red == minute) {
            home_red_cards = home_red_cards + 1
          } else {
            away_red_cards = away_red_cards + 1
          }
        } else {
          minute = end_2nd
        }
      }
      
      # Generating goals in stoppage time of the second half
      if(stoppage_time == TRUE) {
        
        U2 = rpois(1, lambda = exp(mod$st2_parameters["intercept"] + mod$st2_parameters["red_cards"]*(home_red_cards + away_red_cards - home_red_cards_1 - away_red_cards_1) + mod$st2_parameters["close_match"]*(abs(home_goals - away_goals) <= 1)))
        
        while(minute < 45 + U2) {
          
          minute2 = minute + 45 + stoppage_time_1
          lambda = exp(mod$alpha[home_team] + mod$beta[away_team] + mod$goal_parameters["home_advantage"] + mod$goal_parameters["goal_difference"]*(home_goals-away_goals) + mod$goal_parameters["player_difference"]*(away_red_cards-home_red_cards) + mod$goal_parameters["second_half"] + mod$goal_parameters["log_market_value_difference"]*log_market_value_difference)
          mu = exp(mod$alpha[away_team] + mod$beta[home_team] + mod$goal_parameters["goal_difference"]*(away_goals-home_goals) + mod$goal_parameters["player_difference"]*(home_red_cards-away_red_cards) + mod$goal_parameters["second_half"] - mod$goal_parameters["log_market_value_difference"]*log_market_value_difference)
          next_home_goal = rexp(1, rate = lambda) + minute
          next_away_goal = rexp(1, rate = mu) + minute
          next_home_red = t_home_red_cards[t_home_red_cards > minute2][1] - 45 - stoppage_time_1
          next_home_red = ifelse(is.null(next_home_red), Inf, next_home_red)
          next_home_red = ifelse(is.na(next_home_red), Inf, next_home_red)
          next_away_red = t_away_red_cards[t_away_red_cards > minute2][1] - 45 - stoppage_time_1
          next_away_red = ifelse(is.null(next_away_red), Inf, next_away_red)
          next_away_red = ifelse(is.na(next_away_red), Inf, next_away_red)
          next_event = min(next_home_goal, next_away_goal, next_home_red, next_away_red)
          
          if(next_event < 45 + U2) {
            minute = next_event
            if(next_home_goal == minute) {
              home_goals = home_goals + 1
            } else if(next_away_goal == minute) {
              away_goals = away_goals + 1
            } else if(next_home_red == minute) {
              home_red_cards = home_red_cards + 1
            } else {
              away_red_cards = away_red_cards + 1
            }
          } else {
            minute = 45 + U2
          }
        }
      }
    }
    c(home_goals, away_goals)
  }
  
  # Running auxiliary function
  lst = future_lapply(1:n, function(x) pred(home_team, away_team, home_team_market_value, away_team_market_value, home_goals, away_goals, home_red_cards_1, away_red_cards_1, home_red_cards_2, away_red_cards_2, minute, half, end_minute, end_half, stoppage_time, stoppage_time_1),
                      future.seed = TRUE)
  scores = do.call(rbind, lst)
  colnames(scores) = c(home_team, away_team)
  
  home_win = sum(scores[,1] > scores[,2])/n
  away_win = sum(scores[,1] < scores[,2])/n
  draw = 1 - home_win - away_win
  winner = c(home_win, draw, away_win)
  names(winner) = c(home_team, "Draw", away_team)
  freq_scores = sort(table(paste0(scores[,1], "-", scores[,2])), decreasing = TRUE)/n
  
  list("Result" = winner, "Score" = freq_scores)
}








