
library(dplyr)
library(stringr)

load("raw-data/results.RData")
load("data/matches_to_ignore.RData")
load("data/simulations.RData")

# Observed probabilities of home/draw/away and goals scored
# Only considering matches that were simulated
results = results %>%
  anti_join(matches_to_ignore) %>%
  mutate(result = ifelse(home_goals > away_goals, "Home",
                         ifelse(home_goals == away_goals, "Draw", "Away")))

n = nrow(results)

obs_home = sum(results$result == "Home") / n
obs_draw = sum(results$result == "Draw") / n
obs_away = sum(results$result == "Away") / n

obs_home_goals_0 = sum(results$home_goals == 0) / n
obs_home_goals_1 = sum(results$home_goals == 1) / n
obs_home_goals_2 = sum(results$home_goals == 2) / n
obs_home_goals_3 = sum(results$home_goals == 3) / n
obs_home_goals_4 = sum(results$home_goals == 4) / n
obs_home_goals_5 = sum(results$home_goals >= 5) / n

obs_away_goals_0 = sum(results$away_goals == 0) / n
obs_away_goals_1 = sum(results$away_goals == 1) / n
obs_away_goals_2 = sum(results$away_goals == 2) / n
obs_away_goals_3 = sum(results$away_goals == 3) / n
obs_away_goals_4 = sum(results$away_goals == 4) / n
obs_away_goals_5 = sum(results$away_goals >= 5) / n


# Probabilities of home/draw/away and goals scored in the simulated matches
m0_home = NULL
m0_draw = NULL
m0_away = NULL

m1_home = NULL
m1_draw = NULL
m1_away = NULL

m2_home = NULL
m2_draw = NULL
m2_away = NULL

m3_home = NULL
m3_draw = NULL
m3_away = NULL

m4_home = NULL
m4_draw = NULL
m4_away = NULL

m0_home_goals_0 = NULL
m0_home_goals_1 = NULL
m0_home_goals_2 = NULL
m0_home_goals_3 = NULL
m0_home_goals_4 = NULL
m0_home_goals_5 = NULL

m1_home_goals_0 = NULL
m1_home_goals_1 = NULL
m1_home_goals_2 = NULL
m1_home_goals_3 = NULL
m1_home_goals_4 = NULL
m1_home_goals_5 = NULL

m2_home_goals_0 = NULL
m2_home_goals_1 = NULL
m2_home_goals_2 = NULL
m2_home_goals_3 = NULL
m2_home_goals_4 = NULL
m2_home_goals_5 = NULL

m3_home_goals_0 = NULL
m3_home_goals_1 = NULL
m3_home_goals_2 = NULL
m3_home_goals_3 = NULL
m3_home_goals_4 = NULL
m3_home_goals_5 = NULL

m4_home_goals_0 = NULL
m4_home_goals_1 = NULL
m4_home_goals_2 = NULL
m4_home_goals_3 = NULL
m4_home_goals_4 = NULL
m4_home_goals_5 = NULL

m0_away_goals_0 = NULL
m0_away_goals_1 = NULL
m0_away_goals_2 = NULL
m0_away_goals_3 = NULL
m0_away_goals_4 = NULL
m0_away_goals_5 = NULL

m1_away_goals_0 = NULL
m1_away_goals_1 = NULL
m1_away_goals_2 = NULL
m1_away_goals_3 = NULL
m1_away_goals_4 = NULL
m1_away_goals_5 = NULL

m2_away_goals_0 = NULL
m2_away_goals_1 = NULL
m2_away_goals_2 = NULL
m2_away_goals_3 = NULL
m2_away_goals_4 = NULL
m2_away_goals_5 = NULL

m3_away_goals_0 = NULL
m3_away_goals_1 = NULL
m3_away_goals_2 = NULL
m3_away_goals_3 = NULL
m3_away_goals_4 = NULL
m3_away_goals_5 = NULL

m4_away_goals_0 = NULL
m4_away_goals_1 = NULL
m4_away_goals_2 = NULL
m4_away_goals_3 = NULL
m4_away_goals_4 = NULL
m4_away_goals_5 = NULL

for(i in 1:n) {
  m0_home[i] = simulations[[i]]$`0`$`0`$Result[1]
  m0_draw[i] = simulations[[i]]$`0`$`0`$Result[2]
  m0_away[i] = simulations[[i]]$`0`$`0`$Result[3]
  
  m1_home[i] = simulations[[i]]$`1`$`0`$Result[1]
  m1_draw[i] = simulations[[i]]$`1`$`0`$Result[2]
  m1_away[i] = simulations[[i]]$`1`$`0`$Result[3]
  
  m2_home[i] = simulations[[i]]$`2`$`0`$Result[1]
  m2_draw[i] = simulations[[i]]$`2`$`0`$Result[2]
  m2_away[i] = simulations[[i]]$`2`$`0`$Result[3]
  
  m3_home[i] = simulations[[i]]$`3`$`0`$Result[1]
  m3_draw[i] = simulations[[i]]$`3`$`0`$Result[2]
  m3_away[i] = simulations[[i]]$`3`$`0`$Result[3]
  
  m4_home[i] = simulations[[i]]$`4`$`0`$Result[1]
  m4_draw[i] = simulations[[i]]$`4`$`0`$Result[2]
  m4_away[i] = simulations[[i]]$`4`$`0`$Result[3]
  
  x = simulations[[i]]$`0`$`0`$Score
  s = names(x)
  h = as.integer(str_extract(s, ".*(?=-)"))
  a = as.integer(str_extract(s, "(?<=-).*"))
  
  m0_home_goals_0[i] = sum(x[h==0])
  m0_home_goals_1[i] = sum(x[h==1])
  m0_home_goals_2[i] = sum(x[h==2])
  m0_home_goals_3[i] = sum(x[h==3])
  m0_home_goals_4[i] = sum(x[h==4])
  m0_home_goals_5[i] = sum(x[h>=5])
  
  m0_away_goals_0[i] = sum(x[a==0])
  m0_away_goals_1[i] = sum(x[a==1])
  m0_away_goals_2[i] = sum(x[a==2])
  m0_away_goals_3[i] = sum(x[a==3])
  m0_away_goals_4[i] = sum(x[a==4])
  m0_away_goals_5[i] = sum(x[a>=5])
  
  x = simulations[[i]]$`1`$`0`$Score
  s = names(x)
  h = as.integer(str_extract(s, ".*(?=-)"))
  a = as.integer(str_extract(s, "(?<=-).*"))
  
  m1_home_goals_0[i] = sum(x[h==0])
  m1_home_goals_1[i] = sum(x[h==1])
  m1_home_goals_2[i] = sum(x[h==2])
  m1_home_goals_3[i] = sum(x[h==3])
  m1_home_goals_4[i] = sum(x[h==4])
  m1_home_goals_5[i] = sum(x[h>=5])
  
  m1_away_goals_0[i] = sum(x[a==0])
  m1_away_goals_1[i] = sum(x[a==1])
  m1_away_goals_2[i] = sum(x[a==2])
  m1_away_goals_3[i] = sum(x[a==3])
  m1_away_goals_4[i] = sum(x[a==4])
  m1_away_goals_5[i] = sum(x[a>=5])
  
  x = simulations[[i]]$`2`$`0`$Score
  s = names(x)
  h = as.integer(str_extract(s, ".*(?=-)"))
  a = as.integer(str_extract(s, "(?<=-).*"))
  
  m2_home_goals_0[i] = sum(x[h==0])
  m2_home_goals_1[i] = sum(x[h==1])
  m2_home_goals_2[i] = sum(x[h==2])
  m2_home_goals_3[i] = sum(x[h==3])
  m2_home_goals_4[i] = sum(x[h==4])
  m2_home_goals_5[i] = sum(x[h>=5])
  
  m2_away_goals_0[i] = sum(x[a==0])
  m2_away_goals_1[i] = sum(x[a==1])
  m2_away_goals_2[i] = sum(x[a==2])
  m2_away_goals_3[i] = sum(x[a==3])
  m2_away_goals_4[i] = sum(x[a==4])
  m2_away_goals_5[i] = sum(x[a>=5])
  
  x = simulations[[i]]$`3`$`0`$Score
  s = names(x)
  h = as.integer(str_extract(s, ".*(?=-)"))
  a = as.integer(str_extract(s, "(?<=-).*"))
  
  m3_home_goals_0[i] = sum(x[h==0])
  m3_home_goals_1[i] = sum(x[h==1])
  m3_home_goals_2[i] = sum(x[h==2])
  m3_home_goals_3[i] = sum(x[h==3])
  m3_home_goals_4[i] = sum(x[h==4])
  m3_home_goals_5[i] = sum(x[h>=5])
  
  m3_away_goals_0[i] = sum(x[a==0])
  m3_away_goals_1[i] = sum(x[a==1])
  m3_away_goals_2[i] = sum(x[a==2])
  m3_away_goals_3[i] = sum(x[a==3])
  m3_away_goals_4[i] = sum(x[a==4])
  m3_away_goals_5[i] = sum(x[a>=5])
  
  x = simulations[[i]]$`4`$`0`$Score
  s = names(x)
  h = as.integer(str_extract(s, ".*(?=-)"))
  a = as.integer(str_extract(s, "(?<=-).*"))
  
  m4_home_goals_0[i] = sum(x[h==0])
  m4_home_goals_1[i] = sum(x[h==1])
  m4_home_goals_2[i] = sum(x[h==2])
  m4_home_goals_3[i] = sum(x[h==3])
  m4_home_goals_4[i] = sum(x[h==4])
  m4_home_goals_5[i] = sum(x[h>=5])
  
  m4_away_goals_0[i] = sum(x[a==0])
  m4_away_goals_1[i] = sum(x[a==1])
  m4_away_goals_2[i] = sum(x[a==2])
  m4_away_goals_3[i] = sum(x[a==3])
  m4_away_goals_4[i] = sum(x[a==4])
  m4_away_goals_5[i] = sum(x[a>=5])
}

m0_home = mean(m0_home)
m0_draw = mean(m0_draw)
m0_away = mean(m0_away)

m1_home = mean(m1_home)
m1_draw = mean(m1_draw)
m1_away = mean(m1_away)

m2_home = mean(m2_home)
m2_draw = mean(m2_draw)
m2_away = mean(m2_away)

m3_home = mean(m3_home)
m3_draw = mean(m3_draw)
m3_away = mean(m3_away)

m4_home = mean(m4_home)
m4_draw = mean(m4_draw)
m4_away = mean(m4_away)

m0_home_goals_0 = mean(m0_home_goals_0)
m0_home_goals_1 = mean(m0_home_goals_1)
m0_home_goals_2 = mean(m0_home_goals_2)
m0_home_goals_3 = mean(m0_home_goals_3)
m0_home_goals_4 = mean(m0_home_goals_4)
m0_home_goals_5 = mean(m0_home_goals_5)

m1_home_goals_0 = mean(m1_home_goals_0)
m1_home_goals_1 = mean(m1_home_goals_1)
m1_home_goals_2 = mean(m1_home_goals_2)
m1_home_goals_3 = mean(m1_home_goals_3)
m1_home_goals_4 = mean(m1_home_goals_4)
m1_home_goals_5 = mean(m1_home_goals_5)

m2_home_goals_0 = mean(m2_home_goals_0)
m2_home_goals_1 = mean(m2_home_goals_1)
m2_home_goals_2 = mean(m2_home_goals_2)
m2_home_goals_3 = mean(m2_home_goals_3)
m2_home_goals_4 = mean(m2_home_goals_4)
m2_home_goals_5 = mean(m2_home_goals_5)

m3_home_goals_0 = mean(m3_home_goals_0)
m3_home_goals_1 = mean(m3_home_goals_1)
m3_home_goals_2 = mean(m3_home_goals_2)
m3_home_goals_3 = mean(m3_home_goals_3)
m3_home_goals_4 = mean(m3_home_goals_4)
m3_home_goals_5 = mean(m3_home_goals_5)

m4_home_goals_0 = mean(m4_home_goals_0)
m4_home_goals_1 = mean(m4_home_goals_1)
m4_home_goals_2 = mean(m4_home_goals_2)
m4_home_goals_3 = mean(m4_home_goals_3)
m4_home_goals_4 = mean(m4_home_goals_4)
m4_home_goals_5 = mean(m4_home_goals_5)

m0_away_goals_0 = mean(m0_away_goals_0)
m0_away_goals_1 = mean(m0_away_goals_1)
m0_away_goals_2 = mean(m0_away_goals_2)
m0_away_goals_3 = mean(m0_away_goals_3)
m0_away_goals_4 = mean(m0_away_goals_4)
m0_away_goals_5 = mean(m0_away_goals_5)

m1_away_goals_0 = mean(m1_away_goals_0)
m1_away_goals_1 = mean(m1_away_goals_1)
m1_away_goals_2 = mean(m1_away_goals_2)
m1_away_goals_3 = mean(m1_away_goals_3)
m1_away_goals_4 = mean(m1_away_goals_4)
m1_away_goals_5 = mean(m1_away_goals_5)

m2_away_goals_0 = mean(m2_away_goals_0)
m2_away_goals_1 = mean(m2_away_goals_1)
m2_away_goals_2 = mean(m2_away_goals_2)
m2_away_goals_3 = mean(m2_away_goals_3)
m2_away_goals_4 = mean(m2_away_goals_4)
m2_away_goals_5 = mean(m2_away_goals_5)

m3_away_goals_0 = mean(m3_away_goals_0)
m3_away_goals_1 = mean(m3_away_goals_1)
m3_away_goals_2 = mean(m3_away_goals_2)
m3_away_goals_3 = mean(m3_away_goals_3)
m3_away_goals_4 = mean(m3_away_goals_4)
m3_away_goals_5 = mean(m3_away_goals_5)

m4_away_goals_0 = mean(m4_away_goals_0)
m4_away_goals_1 = mean(m4_away_goals_1)
m4_away_goals_2 = mean(m4_away_goals_2)
m4_away_goals_3 = mean(m4_away_goals_3)
m4_away_goals_4 = mean(m4_away_goals_4)
m4_away_goals_5 = mean(m4_away_goals_5)

# Organizando
home_draw_away = tibble(" " = c("Observed", "Model 0", "Model 1", "Model 2", "Model 3", "Model 4"),
                        Home = c(obs_home, c(m0_home, m1_home, m2_home, m3_home, m4_home) - obs_home),
                        Draw = c(obs_draw, c(m0_draw, m1_draw, m2_draw, m3_draw, m4_draw) - obs_draw),
                        Away = c(obs_away, c(m0_away, m1_away, m2_away, m3_away, m4_away) - obs_away))

home_goals = tibble(" " = c("Observed", "Model 0", "Model 1", "Model 2", "Model 3", "Model 4"),
                    "0" = c(obs_home_goals_0, c(m0_home_goals_0, m1_home_goals_0, m2_home_goals_0, m3_home_goals_0, m4_home_goals_0) - obs_home_goals_0),
                    "1" = c(obs_home_goals_1, c(m0_home_goals_1, m1_home_goals_1, m2_home_goals_1, m3_home_goals_1, m4_home_goals_1) - obs_home_goals_1),
                    "2" = c(obs_home_goals_2, c(m0_home_goals_2, m1_home_goals_2, m2_home_goals_2, m3_home_goals_2, m4_home_goals_2) - obs_home_goals_2),
                    "3" = c(obs_home_goals_3, c(m0_home_goals_3, m1_home_goals_3, m2_home_goals_3, m3_home_goals_3, m4_home_goals_3) - obs_home_goals_3),
                    "4" = c(obs_home_goals_4, c(m0_home_goals_4, m1_home_goals_4, m2_home_goals_4, m3_home_goals_4, m4_home_goals_4) - obs_home_goals_4),
                    "5" = c(obs_home_goals_5, c(m0_home_goals_5, m1_home_goals_5, m2_home_goals_5, m3_home_goals_5, m4_home_goals_5) - obs_home_goals_5))

away_goals = tibble(" " = c("Observed", "Model 0", "Model 1", "Model 2", "Model 3", "Model 4"),
                    "0" = c(obs_away_goals_0, c(m0_away_goals_0, m1_away_goals_0, m2_away_goals_0, m3_away_goals_0, m4_away_goals_0) - obs_away_goals_0),
                    "1" = c(obs_away_goals_1, c(m0_away_goals_1, m1_away_goals_1, m2_away_goals_1, m3_away_goals_1, m4_away_goals_1) - obs_away_goals_1),
                    "2" = c(obs_away_goals_2, c(m0_away_goals_2, m1_away_goals_2, m2_away_goals_2, m3_away_goals_2, m4_away_goals_2) - obs_away_goals_2),
                    "3" = c(obs_away_goals_3, c(m0_away_goals_3, m1_away_goals_3, m2_away_goals_3, m3_away_goals_3, m4_away_goals_3) - obs_away_goals_3),
                    "4" = c(obs_away_goals_4, c(m0_away_goals_4, m1_away_goals_4, m2_away_goals_4, m3_away_goals_4, m4_away_goals_4) - obs_away_goals_4),
                    "5" = c(obs_away_goals_5, c(m0_away_goals_5, m1_away_goals_5, m2_away_goals_5, m3_away_goals_5, m4_away_goals_5) - obs_away_goals_5))