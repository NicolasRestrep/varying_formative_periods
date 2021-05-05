# Load necessary packages
library(tidyverse)
library(extraDistr)
library(broom)


varying_windows_model <- function(N,
                                  t_max,
                                  b,
                                  bonus_fitness,
                                  bonus_survival, 
                                  num_rows, 
                                  p_soc_learn, 
                                  sigma_open, 
                                  sims, 
                                  mu_open, 
                                  dem_surv) {
  # Empty list 
  all_results <- list()
  
  for (j in 1:sims) {
    #### Set-up ####
    # Repository (enough rows; plus one column for age)
    population <- matrix(NA, nrow= num_rows, 
                         ncol = t_max+2)
    #First traits randomly allocated
    population[1:N, 2] <- sample(c(1:4), 
                                 size = N, 
                                 replace = T)
    # Draw age as integer
    age <- runif(n = N, min = 1, max = 100) %>% as.integer()
    # Add age to our matrix 
    population[1:N,1] <- age
    # Initial state of the world
    state_world <- sample(c(1:4), 1)
    # An empty vector that records whether the world changes
    world_changes <- rep(NA, t_max)
    # Repository  for keeping track of our results
    df_results <- matrix(NA, nrow = t_max, ncol = 8)
    
    
    #### Loop ####
    for(i in 1:t_max) {
      # Notify me if everyone dies!
      if(length(which(!is.na(population[,1])))==0) {
        print("Everyone died!")
        break
      }
      # Keep track of last agent
      last_agent <- which(!is.na(population[,1]))[length(which(!is.na(population[,1])))]
      
      ### 1. Learning ####
      # Probability of learning
      x <- population[which(!is.na(population[,1])),1] %>% 
        as.integer()
      y <- dgumbel(seq(0, 100, by = 1), mu = mu_open, sigma = sigma_open)
      y_scaled <- (y - min(y))/(max(y)-min(y))
      p_openness <- y_scaled[x]
      # Now calculate who learns 
      learns <- rbinom(n = length(which(!is.na(population[,1]))), 
                       size = 1, 
                       prob = p_openness)
      # What individuals learned? 
      learners <- which(!is.na(population[,1]))[which(learns == 1)] 
      non_learners <- which(!is.na(population[,1]))[which(learns == 0)]
      # From the learners, who will do so by individual exploration?
      soc_learners <- sample(c(TRUE, FALSE), 
                             length(learners),
                             prob = c(p_soc_learn, (1-p_soc_learn)), 
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
      
      #### 2. Surviving ####
      # How many agents are around?
      current_agents <- length(population[which(!is.na(population[,1])),1])
      
      # If there's more than the carrying capacity then start lowering survival probability
      if(current_agents > 100) {
        exponent <- dem_surv/(current_agents/100)
      } else {
        exponent <- dem_surv
      }
      # Calculate the agents probability of survival
      p_survival <- 1 - ((population[which(!is.na(population[,1])),1])/(100-1))^exponent
      # Make sure there are no below-zero probability
      p_survival[p_survival<0] <- 0
      # Which individuals have the correct trait
      fit <- population[which(!is.na(population[,1])),i+2] == state_world
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
      
      
      #### 3. Reproduction ####
      # Pick those in the appropriate age-band
      reproducers <- which(!is.na(population[,1])&
                             population[,1]>=15&
                             population[,1]<=40)
      # From these, who have the appropriate trait?
      fit_reproducers <- population[reproducers,i+2]==state_world
      # Fitness coefficients dependent on carrying capacity
      if (current_agents < 100) {
        fitness_coef <- b
        fitness_coef_fit <- b+bonus_fitness
      } else {
        fitness_coef <- (b/(current_agents/100))
        fitness_coef_fit <- (b+bonus_fitness)/(current_agents/100)
      }
      # Now let's see how many children we get 
      # Fit reproducers
      rf <- rbinom(n = sum(fit_reproducers), 
                   size = 1, 
                   prob = fitness_coef_fit)
      # The rest of reproducers
      r <- rbinom(n = length(reproducers)-sum(fit_reproducers), 
                  size = 1, 
                  prob = fitness_coef)
      # Number of new children 
      num_children <- sum(r) + sum(rf)
      if(num_children > 0) {
        population[(last_agent+1):(last_agent+num_children),1] <- 1
        # They copy reproducers 
        population[(last_agent+1):(last_agent+num_children),i+2] <- 
          sample(population[reproducers,i+2], num_children)
      }
      
      #### 4. Aging ####
      # Everyone ages by one 
      population[which(!is.na(population[,1])),1] <- population[which(!is.na(population[,1])),1]+1
      
      #### 5. World changes #### 
      # The world changes every fifty years
      if (i %in% c(seq(50, t_max, by = 50))) { 
        env_change <-  1 } else {
          env_change  <- 0
        }
      
      # If changes get the current state out and sample from possible worlds
      if (env_change == 1) {
        possible_states <- c(1:4)
        possible_states <- possible_states[-state_world]
        state_world <- sample(possible_states, 1)
      } else {
        state_world <- state_world
      }
      
      #### Book-keeping ####
      df_results[i,1] <- mean(population[,1], na.rm = T)
      df_results[i,2] <- sum(population[which(!is.na(population[,1])),i+2]==1)
      df_results[i,3] <- sum(population[which(!is.na(population[,1])),i+2]==2)
      df_results[i,4] <- sum(population[which(!is.na(population[,1])),i+2]==3)
      df_results[i,5] <- sum(population[which(!is.na(population[,1])),i+2]==4)
      df_results[i,6] <- env_change
      df_results[i,7] <- length(population[which(!is.na(population[,1]))])
      df_results[i,8] <- state_world
    }
    
    #### Results ####
    df <- data.frame(df_results) 
    names(df) <- c("average_age", 
                   "t1",
                   "t2", 
                   "t3", 
                   "t4", 
                   "environment_change",
                   "population", 
                   "state_world")
    df$turn <- 1:nrow(df)
    df$sim <- j 
    
    df <- df %>% 
      mutate(changed = turn*environment_change)
    
    all_results[[j]] <- df
  }
  return(all_results)
}

results <- varying_windows_model(N = 100, 
                                 t_max = 500, 
                                 b = 0.05, 
                                 bonus_fitness = 0.05, 
                                 bonus_survival = 0.05, 
                                 num_rows = 1e5, 
                                 p_soc_learn = 0.95, 
                                 sigma_open = 2, 
                                 mu_open = 20,
                                 sims = 1, 
                                 dem_surv = 4)

# Plot them! 
results[[1]] %>% 
  ggplot(aes(x = turn)) + 
  geom_vline(aes(xintercept = changed), 
             alpha = 0.5, 
             linetype = 2, 
             col = "darkgreen") +
  geom_line(aes(y = t1), col = "darkred") +
  geom_line(aes(y = t2), col = "darkblue") +
  geom_line(aes(y = t3), col = "purple") +
  geom_line(aes(y = t4), col = "orange") +
  theme_bw() + 
  labs(x = "Turn", 
       y = "Prevalence of Traits", 
       title = "Traits through simulation", 
       subtitle = "Pr. environmental change = 0.1")

df <- results[[1]]

df_long <- df %>% 
  mutate(period = rep(c(1:10), 
                      each = 50)) %>% 
  pivot_longer(cols = 2:5, 
               names_to = "trait", 
               values_to = "prevalence") %>% 
  mutate(favored = case_when(trait == "t1" &
                               state_world == 1 ~ 1, 
                             trait == "t2" &
                               state_world == 2 ~ 1,
                             trait == "t3" &
                               state_world == 3 ~ 1,
                             trait == "t4" &
                               state_world == 4 ~ 1))

dfl_filtered <- df_long %>% 
  filter(favored == 1)

dfl_filtered %>% 
  ggplot(aes(x = turn, y = prevalence)) +
  geom_point(col = "orange", 
             size = 1, 
             alpha = 0.5) + 
  geom_smooth(aes(group = period), 
              method = "lm") + 
  theme_light()

# Quick function for the analysis

fit_ols <- function(df) {
  lm(prevalence ~ turn, data = df)
}

out_tidy <- dfl_filtered %>% 
  group_by(period) %>% 
  nest() %>% 
  mutate(model = map(data, fit_ols), 
         tidied = map(model, tidy)) %>% 
  unnest(tidied)

dominant_df <- df_long %>% 
  group_by(turn, trait) %>% 
  summarize(total = sum(prevalence)) %>% 
  arrange(desc(total)) %>% 
  slice(1)

total_pop_df <- df_long %>% 
  group_by(turn) %>% 
  summarise(population = population) %>% 
  slice(1)

perc_df <- left_join(dominant_df, 
                     total_pop_df, 
                     by = "turn") %>% 
  mutate(percent_dominant = total/population)
perc_df$num_favored <- dfl_filtered$prevalence
perc_df$percent_favored <- perc_df$num_favored / perc_df$total

perc_df %>% 
  ggplot(aes(x = turn, y = percent_dominant)) +
  geom_line(col = 'orange') +
  geom_line(aes(x = turn, y = percent_favored), col = "red", alpha = 0.3)

