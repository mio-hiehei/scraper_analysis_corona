library(tidyverse)
library(lubridate)
library(texreg)
library(optimx)
library(MASS)
library(MESS)
library(rstudioapi)

wd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

setwd(wd)

df <- read_csv2("Dataframe_opp_corona.csv")

# We also want to analyse whether the effect changed over the course of the pandemic

df_opposition <- df %>% 
  filter(Opposition == 1) %>%
  filter(Fraktion %in% c("FDP","CDU/CSU","AfD","DIE GRÜNEN","DIE LINKE","SPD")) %>%
  mutate(Health = as.numeric(Health),
         Corona_leg = ifelse(grepl("[Cc]orona", Name),1,0),
         AfD = ifelse(Fraktion == "AfD",1,0))
  
  



ll_logit <- function(theta, y, X) {
  # theta consists merely of beta (dim is ncol(X))
  beta <- theta[1:ncol(X)]
  # linear predictor; make sure that X is stored as.matrix
  mu <- X %*% beta
  # response function
  p <- 1 / (1 + exp(-mu))
  # log-likelihood
  ll <- y * log(p) + (1 - y) * log(1 - p)
  # sum
  ll <- sum(ll)
  return(ll)
}


logit_df <- df_opposition %>%
<<<<<<< Updated upstream
  dplyr::select("Party_yes", "Corona","Days_next_ele", "Rel_Seats", "Rile_Dist", "AfD") %>%
  mutate(Rile_DistCorona = Corona * Rile_Dist) %>%
  na.omit
=======
  dplyr::select("Party_yes","Health", "Corona","Days_next_ele", "Rel_Seats", "Rile_Dist", "AfD","Corona_dur") %>%
  na.omit %>%
  mutate(CoronaCorona_dur = Corona_dur * Corona)
>>>>>>> Stashed changes
X <- as.matrix(cbind(1,logit_df[,2:ncol(logit_df)]))

# Testing for pairwise correlation

cor(X)


y <- logit_df[,1]

# start values
startvals <- rep(0,ncol(X))


# optimize

res <- optim(
  par = startvals,
  fn = ll_logit,
  y = y,
  X = X,
  control = list(fnscale = -1),
  hessian = TRUE,
  method = "BFGS"
)

### Format the output

format_probit_output <- function(res, X, Caption) {

    if (is.null(colnames(X))) {
    var_names <- c("Intercept", paste("Var", 1:(ncol(X) - 1)))
    colnames(X) <- var_names
  }
  # Estimates:
  probit_coef <- res$par
  # Standard Errors:
  probit_se <- sqrt(diag(solve(-res$hessian)))
  
  # t value:
  t <- probit_coef / probit_se
  # degrees of freedom:
  df <- nrow(X) - length(probit_coef)
  t.val <- qt(0.975, df) # critical value of t
  # p value:
  p <-  2 * pt(-abs(t), df)
  
  out <- cbind(probit_coef, probit_se, p)[1:ncol(X), ]
  
  colnames(out) <- c("ML Estimates", "ML SE", "Pr(>|t|)")
  
  if(is.null(colnames(X[,1]))){
    
    colnames(X) <- c("Intercept",colnames(X[,2:ncol(X)]))

  }
  
  rownames(out) <- colnames(X)
  
  
  # Headline
  cat(paste0("\n",Caption,"\n"), rep("=", 40), "\n", sep = "")
  #print(out)
  return(model = out)
}

format_probit_output(res = res, X = X, Caption = "Probit Model of Legislative Voting")


### ========================= Simulation ### ===================##

### Sim function


sim_function <-
  function(nsim = 1000,
           coefs,
           vcov,
           scenario,
           response_function,
           predicted_values = F,
           stochastic_component) {
    if (is.null(dim(scenario))) {
      stop("The scenario needs to be in a matrix.")
    }
    if (length(coefs) != ncol(scenario)) {
      stop("The scenario and the parameter vector don't fit.")
    }
    # Set up the sampling distribution
    S <- mvrnorm(nsim, coefs, vcov)
    
    # Calculate mu and p
    
    # The linear component
    mu <- S %*% t(scenario)
    
    # The response function
    p <- response_function(mu)
    
    # Calculate ev, observed value approach
    ev <- apply(p, 1, mean) 
    
    if (predicted_values) {
      pv <-
        array(stochastic_component(ndraws = prod(dim(ev)), p = ev), dim = dim(ev))
      return(list(ev = ev, pv = pv))
    }
    return(list(ev = ev))
    
  }


<<<<<<< Updated upstream
### calculate:how did Corona crisis affect opposition work? Also: 
=======
### calculate:how did Corona crisis affect opposition work? Did this effect change over the course of the pandemic?
>>>>>>> Stashed changes

# logit response function
response_function <- function(x) {
  1 / (1 + exp(-x))
}

# for predicted values: the stochastic component
stochastic_component <- function(ndraws, p) {
  rbinom(n = ndraws, size = 1, prob = p)
}

# coefficients and variance-covariance matrix of our model
coefs <- res$par[1:ncol(X)]
vcov <- solve(-res$hessian[(1:ncol(X)), (1:ncol(X))])

# Which scenarios to simulate for holding all else equal

scenarios_corona <- c(0,1)
scenarios_distance <- sort(unique(df_opposition$Rile_Dist))

<<<<<<< Updated upstream

cases <- array(NA, c(dim(X), # 1 & 2
                     length(scenarios_corona), # 3
                     length(scenarios_distance))) # 4
=======
scenarios_duration <- seq(min(logit_df$Corona_dur),max(logit_df$Corona_dur),by = 1)

cases <- array(NA, c(dim(X), # 1 & 2
                     length(scenarios_corona), # 3
                     length(scenarios_duration))) # 4
>>>>>>> Stashed changes

cases[, , ,] <- X

# scenarios
for(i in 1:length(scenarios_corona)){
<<<<<<< Updated upstream
  for(j in 1:length(scenarios_distance)){
    cases[,which(colnames(X) == "Corona"),i,] <- scenarios_corona[i]
    cases[,which(colnames(X) == "Rile_Dist"),,j] <- scenarios_distance[j]
=======
  for(j in 1:length(scenarios_duration)){
    cases[,which(colnames(X) == "Corona"),i,] <- scenarios_corona[i]
    cases[,which(colnames(X) == "Corona_dur"),,j] <- scenarios_duration[j]
>>>>>>> Stashed changes
    
    
  }
}

# interaction

for(i in 1:length(scenarios_corona)){
<<<<<<< Updated upstream
  for(j in 1:length(scenarios_distance)){
  cases[,which(colnames(X) == "Rile_DistCorona"),i,] <- cases[,which(colnames(X) == "Rile_Dist"),i,] * scenarios_corona[i]
  cases[,which(colnames(X) == "Rile_DistCorona"),,j] <- cases[,which(colnames(X) == "Corona"),,j] * scenarios_distance[j]
  }
}

df1 <- cases[,,1,1] # No Corona closest ideology
df2 <- cases[,,1,2] # No Corona second-closests ideologcy
df3 <- cases[,,1,3] # No Corona third-closest ideology
df4 <- cases[,,1,4] # no Corona furthest away ideology

df5 <- cases[,,2,1] # Corona closest ideology
df6 <- cases[,,2,2] # Corona second-closests ideologcy
df7 <- cases[,,2,3] # Corona third-closest ideology
df8 <- cases[,,2,4] # Corona furthest away ideology
=======
  for(j in 1:length(scenarios_duration)){
  cases[,which(colnames(X) == "CoronaCorona_dur"),i,] <- cases[,which(colnames(X) == "Corona_dur"),i,] * scenarios_corona[i]
  cases[,which(colnames(X) == "CoronaCorona_dur"),,j] <- cases[,which(colnames(X) == "Corona"),,j] * scenarios_health[j]
  }
}

# df1 <- cases[,,1,1] # No corona and no health
# df2 <- cases[,,1,2] # No corona but health
# df3 <- cases[,,2,1] # Corona but no Health
# df4 <- cases[,,2,2] # COrona and health

ev_corona <- matrix(NA, nrow = 1000, ncol = length(scenarios_corona)+length(scenarios_duration))
>>>>>>> Stashed changes

ev_corona <- matrix(NA, nrow = 1000, ncol = length(scenarios_corona)*length(scenarios_distance))

for(i in 1:length(scenarios_corona)){
<<<<<<< Updated upstream
  for(j in 1:length(scenarios_distance)) {
    if(i == 1){
      ev_corona[,i*j] <- sim_function(
        coefs = coefs,
        vcov = vcov,
        response_function = response_function,
        stochastic_component = stochastic_component,
        scenario = cases[,,i,j],
        predicted_values = F
      )$ev
    } else{
        ev_corona[,i+2+j] <- sim_function(
          coefs = coefs,
          vcov = vcov,
          response_function = response_function,
          stochastic_component = stochastic_component,
          scenario = cases[,,i,j],
          predicted_values = F
        )$ev
      }
=======
  ev_corona[,i] <- sim_function(
    coefs = coefs,
    vcov = vcov,
    response_function = response_function,
    stochastic_component = stochastic_component,
    scenario = cases[,,i,j],
    predicted_values = F
  )$ev
  for(j in 1:length(scenarios_duration)) {
    ev_corona[,i+j] <- sim_function(
      coefs = coefs,
      vcov = vcov,
      response_function = response_function,
      stochastic_component = stochastic_component,
      scenario = cases[,,i,j],
      predicted_values = F
    )$ev
    
    cat("Round",j,i)
>>>>>>> Stashed changes
  }
}



ev_corona_mean <- apply(ev_corona, 2, mean)
ev_corona_ci <- t(apply(ev_corona, 2, quantile, c(0.025, 0.975)))


df_corona <- data.frame(
  mean = ev_corona_mean,
  lower = ev_corona_ci[,1],
  upper = ev_corona_ci[,2],
<<<<<<< Updated upstream
  scenario = c(paste0("Pre Corona_",sort(unique(df_opposition$Rile_Dist))),
               paste0("Post Corona_",sort(unique(df_opposition$Rile_Dist))))
)

df_corona <- df_corona %>%
  separate(scenario, sep = "_", into = c("Corona","Rile_Dist")) %>%
  mutate(Rile_Dist = as.numeric(Rile_Dist)
         # Party = case_when(max(Rile_Dist) ~ "The Left",
         #                   min(Rile_Dist) ~ "FDP",
         #                   sort(Rile_Dist)[2] ~ "The Greens",
         #                   sort(Rile_Dist)[3] ~ "AfD")
         )

ggplot(data = df_corona, aes(x = Rile_Dist, color = Corona))+
  geom_line(aes(y = mean))+
  geom_line(aes(y = lower), linetype = "dashed")+
  geom_line(aes(y = upper), linetype = "dashed")+
  ylab("P(Voting with Gov.)")+
  xlab("Left-Right Distance to Coalition")+
  theme_bw()
=======
  duration = scenarios_duration)

ggplot(data = df_corona)+
  geom_errorbar(aes(x = scenarios_duration, y = mean, ymin = lower, ymax = upper))



# Do the same stuff but only corona: two scenarios



scenarios_corona <- c(0,1)

cases <- array(NA, c(dim(X), # 1 & 2
                     length(scenarios_corona))) # 4

cases[, , ] <- X

# scenarios
for(i in 1:length(scenarios_corona)){
    cases[,which(colnames(X) == "Corona"),i] <- scenarios_corona[i]
}

# interaction

for(i in 1:length(scenarios_corona)){
    cases[,which(colnames(X) == "HealthCorona"),i] <- cases[,which(colnames(X) == "Health"),i] * scenarios_corona[i]
}

df1 <- cases[,,1] # No corona
df2 <- cases[,,2] # corona


ev_corona <- matrix(NA, nrow = 1000, ncol = length(scenarios_corona))


for(i in 1:length(scenarios_corona)){
  ev_corona[,i] <- sim_function(
    coefs = coefs,
    vcov = vcov,
    response_function = response_function,
    stochastic_component = stochastic_component,
    scenario = cases[,,i],
    predicted_values = F
  )$ev
}

df_fd <- data.frame(First_diff = ev_corona[,2] - ev_corona[,1])

ggplot(data = df_fd,aes(x = First_diff))+
  geom_histogram()+
  geom_vline(xintercept = quantile(df_fd$First_diff,c(0.025, 0.975)))



fd_corona_mean <- mean(df_fd)
fd_corona_ci <- t(quantile(df_fd,c(0.025, 0.975)))

df_fd <- data.frame(
  mean = fd_corona_mean,
  lower = fd_corona_ci[,1],
  upper = fd_corona_ci[,2],
  scenario = c("Before Corona","After Corona")
)

ggplot(data = df_corona)+
  geom_errorbar(aes(x = scenario, y = mean, ymin = lower, ymax = upper))

>>>>>>> Stashed changes
