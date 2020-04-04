


SEIR_model <- function(I1_0 = 2, N = 10000, 
                       beta1 = 0.02, beta2 = 0.002, beta3 = 0.0002, alpha = 1, 
                       gamma0 = 0.5, gamma1 = 0.8, gamma2 = 0.6, gamma3 = 0.3, 
                       exposed_to_asymptomatic =  5, asymptomatic_to_onset = 0, 
                       onset_to_hosp = 5, hosp_to_ICU = 3, ICU_to_death = 3, ICU_to_recover = 3,
                       sim_time = 60, time_0 = '03/23/2020', time_from_infect_to_report = 10
                       ) {
  ###' SEIR model gets the infected data at time 0 and model the infetion spread over time (projected)
  ###' I1_0: <int> number of infected patient at time 0
  ###' N: <int> population of the community
  ###' beta: <float> rate of spread which represents the probability of transmitting disease between a susceptible and an exposed individual
  ###' alpha: <float> incubation rate: rate of latent individuals becoming infectious (symptomatic + asmpromatic). average duration of incubation = 1/alpha
  ###' gamma1: <float> recovery rate from mild  infected individuals
  ###' rho1: <float> rate of (mild & asymptomatic) infected individuals to hospitatlization
  ###' gamma2: <float> recovery rate from hospitalized cases
  ###' rho2: <float> rate of hospitalized cases ending up in ICU
  ###' gamma3: <float> recovery rate from ICU cases
  ###' mu: <float> mortality rate of ICU cases
  ###' time_from_infect_to_report: <int> time it takes from being infected to being confirmed
  rho0 <- 1 - gamma0
  rho1 <- 1 - gamma1 
  rho2 <- 1 - gamma2
  mu <- 1 - gamma3
  S <- E <- I0 <- I1 <- I2 <- I3 <- R <- D <- rep(0, sim_time)  
  # estimate the exposed and susceptible at time 0 
  I1[1] <- I1_0
  I0[1] <- I1[1] / gamma0
  E[1] <- I0[1] / alpha
  S[1] <- N - E[1] # number of susceptible at time 0
  for (t in 1:(sim_time - 1))
  {
    S[t + 1] = S[t] * beta * I0
  }
  
}