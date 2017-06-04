rm(list=ls())

# Simulation parameters
seed <- 8902438
num_subjs  <- 10
num_trials <- 360
pr_correct <- 0.8

# Set seed
set.seed(seed)

# Parameters from gng_m3 fit to example data will be used to simulate data
simul_pars <- data.frame(xi      = rnorm(num_subjs, 0.05, 0.01), 
                         ep      = rnorm(num_subjs, 0.15, 0.05), 
                         b       = rnorm(num_subjs, 0.40, 0.10), 
                         pi      = rnorm(num_subjs, 0.10, 0.02), 
                         rho_rew = rnorm(num_subjs, 4.00, 1.00), 
                         rho_pun = rnorm(num_subjs, 2.00, 1.0), 
                         subjID  = 1:10) 

# For storing simulated choice data for all subjects
all_data <- NULL

for (i in 1:num_subjs) {
  # Initializing latent model terms
  wv_g  <- rep(0,4)   # action wegith for go
  wv_ng <- rep(0,4);  # action wegith for nogo
  qv_g  <- rep(0,4);  # Q value for go
  qv_ng <- rep(0,4);  # Q value for nogo
  sv    <- rep(0,4);  # stimulus value 
  pGo   <- rep(0,4);  # pr(go) 
  
  # Individual-level (i.e. per subject) parameter values
  xi      <- simul_pars$xi[i]
  ep      <- simul_pars$ep[i]
  b       <- simul_pars$b[i]
  pi      <- simul_pars$pi[i]
  rho_rew <- simul_pars$rho_rew[i]
  rho_pun <- simul_pars$rho_pun[i]
  
  # For storing simulated data for current subject
  tmp_data = data.frame( subjID=NULL, cue=NULL, keyPressed=NULL, outcome=NULL)
  
  # Generate random cue presentation for each subject
  cue <- sample(c(rep(1, num_trials/4), rep(2, num_trials/4), 
                  rep(3, num_trials/4), rep(4, num_trials/4)), replace = F)
  
  for (t in 1:num_trials)  {
    # Update values
    wv_g[ cue[t] ]  <- qv_g[ cue[t] ] + b + pi * sv[ cue[t] ];
    wv_ng[ cue[t] ] <- qv_ng[ cue[t] ];  # qv_ng is always equal to wv_ng (regardless of action)      
    pGo[ cue[t] ]   <- boot::inv.logit( wv_g[ cue[t] ] - wv_ng[ cue[t] ] ); 
    pGo[ cue[t] ]   <- pGo[ cue[t] ] * (1 - xi) + xi/2;  # noise
    
    # Press key given cue? 
    keyPressed <- rbinom(size=1, n = 1, prob = pGo[ cue[t] ] );
    
    # Generate outcome 
    if (cue[t]==1) {
      if (keyPressed) {
        outcome <- rbinom(size=1, n = 1, prob = pr_correct );
      } else{
        outcome <- rbinom(size=1, n = 1, prob = 1-pr_correct );
      }
    } else if (cue[t]==2) {
      if (keyPressed) {
        outcome <- rbinom(size=1, n = 1, prob = pr_correct ) - 1;
      } else{
        outcome <- rbinom(size=1, n = 1, prob = 1-pr_correct ) - 1;
      }
    } else if (cue[t]==3) {
      if (keyPressed) {
        outcome <- rbinom(size=1, n = 1, prob = 1-pr_correct );
      } else{
        outcome <- rbinom(size=1, n = 1, prob = pr_correct );
      }
    } else if (cue[t]==4) {
      if (keyPressed) {
        outcome <- rbinom(size=1, n = 1, prob = 1-pr_correct ) - 1;
      } else{
        outcome <- rbinom(size=1, n = 1, prob = pr_correct ) - 1;
      }
    }
    
    # after receiving feedback, update sv[t+1]
    if (outcome >= 0) {
      sv[ cue[t] ] <- sv[ cue[t] ] + ep * ( rho_rew * outcome - sv[ cue[t] ] );
    } else {
      sv[ cue[t] ] <- sv[ cue[t] ] + ep * ( rho_pun * outcome - sv[ cue[t] ] );
    }
    
    # update action values
    if (keyPressed) { # update go value 
      if (outcome >= 0) {
        qv_g[ cue[t] ]  <- qv_g[ cue[t] ] + ep * ( rho_rew * outcome - qv_g[ cue[t] ]);
      } else {
        qv_g[ cue[t] ]  <- qv_g[ cue[t] ] + ep * ( rho_pun * outcome - qv_g[ cue[t] ]);
      }
    } else { # update no-go value  
      if (outcome >= 0) {
        qv_ng[ cue[t] ] <- qv_ng[ cue[t] ] + ep * ( rho_rew * outcome - qv_ng[ cue[t] ]);  
      } else {
        qv_ng[ cue[t] ] <- qv_ng[ cue[t] ] + ep * ( rho_pun * outcome - qv_ng[ cue[t] ]);  
      }
    }  
    
    # append simulated task/response to subject data
    tmp_data[t, "subjID"]     = i
    tmp_data[t, "cue"]        = cue[t]
    tmp_data[t, "keyPressed"] = keyPressed
    tmp_data[t, "outcome"]    = outcome
  } # end of t loop
  # Append current subject with all subjects' data
  all_data = rbind(all_data, tmp_data)
}

# Write out data
write.table(all_data, file = "gng_m4_simulated_data.txt", row.names = F, col.names = T, sep = "\t")
