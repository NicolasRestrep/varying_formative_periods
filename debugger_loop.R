library(tidyverse)
#### Set-up ####
# Number of agents
N <- 50
# Maximum turns 
t_max <- 500
# Baseline fertility rate 
b <- 0.05
# Bonus fertility for fitness
bonus_fitness <- 0.05
# Bonus survival 
bonus_survival <- 0.05
# Repository (enough rows; plus one column for age)
population <- matrix(NA, nrow= 1e6, 
                     ncol = t_max+2)

# An index for organizing 
index <- sample(x = 1:100, 
                size = N, 
                replace = F)

# First traits 
population[1:N, 2] <- sample(c(1:4), 
                             size = N, 
                             replace = T)
# Draw age as integer 
age <- runif(n = N, min = 1, max = 100) %>% as.integer()
# Add to our matrix 
population[1:N,1] <- age
# State world 
state_world <- sample(c(1:4), 1)
# World changes 
world_changes <- rep(NA, N)
# Probability of environmental change 
u <- 0.01

# df_results matrix 
df_results <- matrix(NA, nrow = t_max, ncol = 7)
#### Loop ####
for(i in 1:t_max) {
  if(length(which(!is.na(population[,1])))==0) {
    print("Everyone died!")
    break
  }
  # Last agent 
  last_agent <- which(!is.na(population[,1]))[length(which(!is.na(population[,1])))]
  
  ## 1. Learning ##
  # Probability of learning
  x <- population[which(!is.na(population[,1])),1] %>% 
    as.integer()
  p_openness <- exp(1)^((-x)/35)
  # Now calculate who learns 
  learns <- rbinom(n = length(which(!is.na(population[,1]))), 
                   size = 1, 
                   prob = p_openness)
  # What individuals learned? 
  learners <- which(!is.na(population[,1]))[which(learns == 1)] 
  non_learners <- which(!is.na(population[,1]))[which(learns == 0)]
  # Let's see which will learn individually 
  soc_learners <- sample(c(TRUE, FALSE), 
                         length(learners),
                         prob = c(0.9, 0.1), 
                         replace = TRUE)
  # Draw random demonstrators 
  demonstrated_traits <- sample(population[which(!is.na(population[,i+1])),i+1], 
                                size = sum(soc_learners),
                                replace = T)
  # Now update their beliefs
  # Update beliefs of social learners 
  population[learners[soc_learners],i+2] <- demonstrated_traits
  # Update beliefs of explorers 
  population[learners[!soc_learners],i+2] <- state_world
  # The others just get their previous beliefs
  population[non_learners,i+2] <- population[non_learners,i+1]
  
  ## 2. Surviving ##
  # See what the max age is
  # Calculate the agents probability of survival
  p_survival <- 1 - ((population[which(!is.na(population[,1])),1])/(100-1))^2
  p_survival[p_survival<0] <- 0
  # Which individuals have the correct trait
  fit <- population[which(!is.na(population[,1])),i+2] == state_world
  try(if (sum(is.na(fit)) >0) stop("Wrong sum"))
  # Add their bonus 
  p_survival[fit] <- p_survival[fit] + bonus_survival
  # Make sure no probability is higher than 1 
  p_survival[p_survival >=1] <- 1
  # Now calculate who survives 
  survived <- rbinom(n = length(which(!is.na(population[,1]))), 
                     size = 1, 
                     prob = p_survival)
  # NAs in the ages of those who don't
  population[which(!is.na(population[,1]))[survived==0],1] <- NA
  
  
  ## 3. Reproduction ##
  reproducers <- which(!is.na(population[,1])&
                         population[,1]>=15&
                         population[,1]<=40)
  # From these, who have the appropriate trait 
  fit_reproducers <- population[reproducers,i+2]==state_world
  # Now let's carry see how many children we get 
  # Fit reproducers
  rf <- rbinom(n = sum(fit_reproducers), 
               size = 1, 
               prob = b+bonus_fitness)
  
  # The rest of reproducers
  r <- rbinom(n = length(reproducers)-sum(fit_reproducers), 
              size = 1, 
              prob = b)
  # Number of new children 
  num_children <- sum(r) + sum(rf)
  if(num_children > 0) {
    population[(last_agent+1):(last_agent+num_children),1] <- 1
    # They copy reproducers 
    population[(last_agent+1):(last_agent+num_children),i+2] <- 
      sample(population[reproducers,i+2], num_children)
  }
  
  
  ## 4. Aging ##
  population[which(!is.na(population[,1])),1] <- population[which(!is.na(population[,1])),1]+1
  
  ## 5. World changes ## 
  # Does it change 
  env_change <- rbinom(n =1, size = 1, prob=u)
  
  # If changes get the current state out and sample from possible worlds
  if (env_change == 1) {
    possible_states <- c(1:4)
    possible_states <- possible_states[-state_world]
    state_world <- sample(possible_states, 1)
  } else {
    state_world <- state_world
  }
  df_results[i,1] <- mean(population[,1], na.rm = T)
  df_results[i,2] <- sum(population[which(!is.na(population[,1])),i+2]==1)
  df_results[i,3] <- sum(population[which(!is.na(population[,1])),i+2]==2)
  df_results[i,4] <- sum(population[which(!is.na(population[,1])),i+2]==3)
  df_results[i,5] <- sum(population[which(!is.na(population[,1])),i+2]==4)
  df_results[i,6] <- env_change
  df_results[i, 7] <- length(population[which(!is.na(population[,1]))])
}

#### Results ####
df <- data.frame(df_results) 
names(df) <- c("average_age", 
               "t1",
               "t2", 
               "t3", 
               "t4", 
               "environment_change",
               "population")
df$turn <- 1:nrow(df)

df <- df %>% 
  mutate(changed = turn*environment_change)


df %>% 
  ggplot(aes(x = turn)) + 
  geom_vline(aes(xintercept = changed), alpha = 0.5, linetype = 2, col = "darkgreen") +
  geom_line(aes(y = t1), col = "darkred") +
  geom_line(aes(y = t2), col = "darkblue") +
  geom_line(aes(y = t3), col = "purple") +
  geom_line(aes(y = t4), col = "orange") +
  theme_bw() + 
  labs(x = "Turn", 
       y = "Prevalence of Traits", 
       title = "Traits through simulation")
