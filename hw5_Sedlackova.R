# Problem 1 Part 2

p_given_theta_n <- function(y,theta,n){
  return(choose(n,y)* (theta)^y * (1-theta)^(n-y))
}

valid_y = 0:4
out = c()
for (i in valid_y){
  out = c(out, p_given_theta_n(i,3/4,4))
}

plot(valid_y, out, main="Likelihood", xlab="Number of Heads",ylab="Probability")

# Problem 1 Part 3
post_given_theta_n <- function(y,theta,n){
  return((n+1)*p_given_theta_n(y,theta,n))
}
 
vec = seq(0, 1, by=0.01)

out1 = c()
out2 = c()
out3 = c()
out4 = c()

for (i in vec){
  out1 = c(out1, post_given_theta_n(1,i,1))
  out2 = c(out2, post_given_theta_n(2,i,2))
  out3 = c(out3, post_given_theta_n(2,i,3))
  out4 = c(out4, post_given_theta_n(3,i,4))
}

plot(vec, out1, main="Posterior after 1 head and 1 flip", xlab="Theta",ylab="Probability")
plot(vec, out2, main="Posterior after 2 heads and 2 flips", xlab="Theta",ylab="Probability")
plot(vec, out3, main="Posterior after 2 heads and 3 flips", xlab="Theta",ylab="Probability")
plot(vec, out4, main="Posterior after 3 heads and 4 flips", xlab="Theta",ylab="Probability")


# Problem 2
# Deriving the posteriors given our candy pulls (nl is number of limes pulled, 
# and nc is number of cherries pulled; so nc + nl = N, where N is the total
# number of candy pulls)
p_h1_given_d <- function(nc, nl){
  denom = (0.1)*(1^nc)*(0^nl) +
    (0.2)*(0.75^nc)*(0.25^nl) +
    (0.4)*(0.5^nc)*(0.5^nl) +
    (0.2)*(0.25^nc)*(0.75^nl) +
    (0.1)*(0^nc)*(1^nl)
  numer = (0.1)*(1^nc)*(0^nl)
  return(numer/denom)
}

p_h2_given_d <- function(nc, nl){
  denom = (0.1)*(1^nc)*(0^nl) +
    (0.2)*(0.75^nc)*(0.25^nl) +
    (0.4)*(0.5^nc)*(0.5^nl) +
    (0.2)*(0.25^nc)*(0.75^nl) +
    (0.1)*(0^nc)*(1^nl)
  numer = (0.2)*(0.75^nc)*(0.25^nl)
  return(numer/denom)
}

p_h3_given_d <- function(nc, nl){
  denom = (0.1)*(1^nc)*(0^nl) +
    (0.2)*(0.75^nc)*(0.25^nl) +
    (0.4)*(0.5^nc)*(0.5^nl) +
    (0.2)*(0.25^nc)*(0.75^nl) +
    (0.1)*(0^nc)*(1^nl)
  numer = (0.4)*(0.5^nc)*(0.5^nl)
  return(numer/denom)
}

p_h4_given_d <- function(nc, nl){
  denom = (0.1)*(1^nc)*(0^nl) +
          (0.2)*(0.75^nc)*(0.25^nl) +
          (0.4)*(0.5^nc)*(0.5^nl) +
          (0.2)*(0.25^nc)*(0.75^nl) +
          (0.1)*(0^nc)*(1^nl)
  numer = (0.2)*(0.25^nc)*(0.75^nl)
  return(numer/denom)
}

p_h5_given_d <- function(nc, nl){
  denom = (0.1)*(1^nc)*(0^nl) +
    (0.2)*(0.75^nc)*(0.25^nl) +
    (0.4)*(0.5^nc)*(0.5^nl) +
    (0.2)*(0.25^nc)*(0.75^nl) +
    (0.1)*(0^nc)*(1^nl)
  numer = (0.1)*(0^nc)*(1^nl)
  return(numer/denom)
}

library(dplyr)
# Vector of number of candy pulls
N_max_pulls = 100
bag_h1_results = ifelse(runif(N_max_pulls) <= 0, "Lime", "Cherry") %>%
  data.frame(candy_pull = .) %>%
  mutate(num_limes = ifelse(candy_pull == "Lime", 1, 0),
         num_cherries = ifelse(candy_pull == "Cherry",1,0),
         cum_num_limes = cumsum(num_limes),
         cum_num_cherries = cumsum(num_cherries),
         pull_number = 1:nrow(.)) %>%
  select(pull_number, candy_pull, cum_num_limes, cum_num_cherries) %>%
  bind_rows(data.frame(pull_number=0, candy_pull=NA, cum_num_limes=0, cum_num_cherries=0)) %>%
  arrange(pull_number)

bag_h2_results = ifelse(runif(N_max_pulls) <= 0.25, "Lime", "Cherry") %>%
  data.frame(candy_pull = .) %>%
  mutate(num_limes = ifelse(candy_pull == "Lime", 1, 0),
         num_cherries = ifelse(candy_pull == "Cherry",1,0),
         cum_num_limes = cumsum(num_limes),
         cum_num_cherries = cumsum(num_cherries),
         pull_number = 1:nrow(.)) %>%
  select(pull_number, candy_pull, cum_num_limes, cum_num_cherries) %>%
  bind_rows(data.frame(pull_number=0, candy_pull=NA, cum_num_limes=0, cum_num_cherries=0)) %>%
  arrange(pull_number)

bag_h3_results = ifelse(runif(N_max_pulls) <= 0.5, "Lime", "Cherry") %>%
  data.frame(candy_pull = .) %>%
  mutate(num_limes = ifelse(candy_pull == "Lime", 1, 0),
         num_cherries = ifelse(candy_pull == "Cherry",1,0),
         cum_num_limes = cumsum(num_limes),
         cum_num_cherries = cumsum(num_cherries),
         pull_number = 1:nrow(.)) %>%
  select(pull_number, candy_pull, cum_num_limes, cum_num_cherries) %>%
  bind_rows(data.frame(pull_number=0, candy_pull=NA, cum_num_limes=0, cum_num_cherries=0)) %>%
  arrange(pull_number)

bag_h4_results = ifelse(runif(N_max_pulls) <= 0.75, "Lime", "Cherry") %>%
  data.frame(candy_pull = .) %>%
  mutate(num_limes = ifelse(candy_pull == "Lime", 1, 0),
         num_cherries = ifelse(candy_pull == "Cherry",1,0),
         cum_num_limes = cumsum(num_limes),
         cum_num_cherries = cumsum(num_cherries),
         pull_number = 1:nrow(.)) %>%
  select(pull_number, candy_pull, cum_num_limes, cum_num_cherries) %>%
  bind_rows(data.frame(pull_number=0, candy_pull=NA, cum_num_limes=0, cum_num_cherries=0)) %>%
  arrange(pull_number)

bag_h5_results = ifelse(runif(N_max_pulls) <= 1, "Lime", "Cherry") %>%
  data.frame(candy_pull = .) %>%
  mutate(num_limes = ifelse(candy_pull == "Lime", 1, 0),
         num_cherries = ifelse(candy_pull == "Cherry",1,0),
         cum_num_limes = cumsum(num_limes),
         cum_num_cherries = cumsum(num_cherries),
         pull_number = 1:nrow(.)) %>%
  select(pull_number, candy_pull, cum_num_limes, cum_num_cherries) %>%
  bind_rows(data.frame(pull_number=0, candy_pull=NA, cum_num_limes=0, cum_num_cherries=0)) %>%
  arrange(pull_number)

# Plotting posteriors after more and more candy pulls (assuming h1 candy distribution)
post_h1_bag1 = c()
post_h2_bag1 = c()
post_h3_bag1 = c()
post_h4_bag1 = c()
post_h5_bag1 = c()
for (i in 0:N_max_pulls){
  nc = bag_h1_results$cum_num_cherries[i+1]
  nl = bag_h1_results$cum_num_limes[i+1]
  post_h1_bag1 = c(post_h1_bag1,p_h1_given_d(nc,nl))
  post_h2_bag1 = c(post_h2_bag1,p_h2_given_d(nc,nl))
  post_h3_bag1 = c(post_h3_bag1,p_h3_given_d(nc,nl))
  post_h4_bag1 = c(post_h4_bag1,p_h4_given_d(nc,nl))
  post_h5_bag1 = c(post_h5_bag1,p_h5_given_d(nc,nl))
}

# Plot results (assuming h1 candy distribution)
plot(0:N_max_pulls, post_h1_bag1, pch=1, col="blue", 
     ylim=c(0.0,1.0), xlab='Number of Candy Pulls', ylab='Posterior Probability',
     main = "Posteriors Assuming Pulling from h1 Candy Distribution")
legend(85, 0.85, legend=c("p(h1 | d)", "p(h2 | d)", "p(h3 | d)", "p(h4 | d)", "p(h5 | d)"),
       col=c("blue", "red","green","yellow","orange"), pch=1:5, cex=0.8)
par(new=T)
plot(0:N_max_pulls, post_h2_bag1, pch=2, col="red", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, post_h3_bag1, pch=3, col="green", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, post_h4_bag1, pch=4, col="yellow", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, post_h5_bag1, pch=5, col="orange", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=F)

# Now let's plot the probability of the next pull being lime given previous pulls
p_dnext_given_d <- function(nc, nl){
  result = (0)*p_h1_given_d(nc,nl) +
           (0.25)*p_h2_given_d(nc,nl) + 
           (0.5)*p_h3_given_d(nc,nl) +
           (0.75)*p_h4_given_d(nc,nl) +
           (1)*p_h5_given_d(nc,nl)
  return(result)
}

pred_next_lime_bag1 = c()
for (i in 0:N_max_pulls){
  nc = bag_h1_results$cum_num_cherries[i+1]
  nl = bag_h1_results$cum_num_limes[i+1]
  pred_next_lime_bag1 = c(pred_next_lime_bag1, p_dnext_given_d(nc,nl))
}

plot(0:N_max_pulls, pred_next_lime_bag1, pch=1, col="black", 
     ylim=c(0.0,1.0), xlab='Number of Candy Pulls', ylab='Probability that next candy is lime',
     main = "Predictive Prob. That Next Candy is Lime (Assume h1 dist.)")
legend(70, 0.85, legend=c("p(d_next = lime | d)"),
       col=c("black"), pch=1, cex=0.8)

##### -----

# Plotting posteriors after more and more candy pulls (assuming h2 candy distribution)
post_h1_bag2 = c()
post_h2_bag2 = c()
post_h3_bag2 = c()
post_h4_bag2 = c()
post_h5_bag2 = c()
for (i in 0:N_max_pulls){
  nc = bag_h2_results$cum_num_cherries[i+1]
  nl = bag_h2_results$cum_num_limes[i+1]
  post_h1_bag2 = c(post_h1_bag2,p_h1_given_d(nc,nl))
  post_h2_bag2 = c(post_h2_bag2,p_h2_given_d(nc,nl))
  post_h3_bag2 = c(post_h3_bag2,p_h3_given_d(nc,nl))
  post_h4_bag2 = c(post_h4_bag2,p_h4_given_d(nc,nl))
  post_h5_bag2 = c(post_h5_bag2,p_h5_given_d(nc,nl))
}

# Plot results (assuming h2 candy distribution)
plot(0:N_max_pulls, post_h1_bag2, pch=1, col="blue", 
     ylim=c(0.0,1.0), xlab='Number of Candy Pulls', ylab='Posterior Probability',
     main = "Posteriors Assuming Pulling from h2 Candy Distribution")
legend(85, 0.85, legend=c("p(h1 | d)", "p(h2 | d)", "p(h3 | d)", "p(h4 | d)", "p(h5 | d)"),
       col=c("blue", "red","green","yellow","orange"), pch=1:5, cex=0.8)
par(new=T)
plot(0:N_max_pulls, post_h2_bag2, pch=2, col="red", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, post_h3_bag2, pch=3, col="green", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, post_h4_bag2, pch=4, col="yellow", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, post_h5_bag2, pch=5, col="orange", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=F)

# Now let's plot the probability of the next pull being lime given previous pulls
pred_next_lime_bag2 = c()
for (i in 0:N_max_pulls){
  nc = bag_h2_results$cum_num_cherries[i+1]
  nl = bag_h2_results$cum_num_limes[i+1]
  pred_next_lime_bag2 = c(pred_next_lime_bag2, p_dnext_given_d(nc,nl))
}

plot(0:N_max_pulls, pred_next_lime_bag2, pch=1, col="black", 
     ylim=c(0.0,1.0), xlab='Number of Candy Pulls', ylab='Probability that next candy is lime',
     main = "Predictive Prob. That Next Candy is Lime (Assume h2 dist.)")
legend(70, 0.85, legend=c("p(d_next = lime | d)"),
       col=c("black"), pch=1, cex=0.8)

#### ----

# Plotting posteriors after more and more candy pulls (assuming h3 candy distribution)
post_h1_bag3 = c()
post_h2_bag3 = c()
post_h3_bag3 = c()
post_h4_bag3 = c()
post_h5_bag3 = c()
for (i in 0:N_max_pulls){
  nc = bag_h3_results$cum_num_cherries[i+1]
  nl = bag_h3_results$cum_num_limes[i+1]
  post_h1_bag3 = c(post_h1_bag3,p_h1_given_d(nc,nl))
  post_h2_bag3 = c(post_h2_bag3,p_h2_given_d(nc,nl))
  post_h3_bag3 = c(post_h3_bag3,p_h3_given_d(nc,nl))
  post_h4_bag3 = c(post_h4_bag3,p_h4_given_d(nc,nl))
  post_h5_bag3 = c(post_h5_bag3,p_h5_given_d(nc,nl))
}

# Plot results (assuming h3 candy distribution)
plot(0:N_max_pulls, post_h1_bag3, pch=1, col="blue", 
     ylim=c(0.0,1.0), xlab='Number of Candy Pulls', ylab='Posterior Probability',
     main = "Posteriors Assuming Pulling from h3 Candy Distribution")
legend(85, 0.85, legend=c("p(h1 | d)", "p(h2 | d)", "p(h3 | d)", "p(h4 | d)", "p(h5 | d)"),
       col=c("blue", "red","green","yellow","orange"), pch=1:5, cex=0.8)
par(new=T)
plot(0:N_max_pulls, post_h2_bag3, pch=2, col="red", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, post_h3_bag3, pch=3, col="green", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, post_h4_bag3, pch=4, col="yellow", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, post_h5_bag3, pch=5, col="orange", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=F)

# Now let's plot the probability of the next pull being lime given previous pulls
pred_next_lime_bag3 = c()
for (i in 0:N_max_pulls){
  nc = bag_h3_results$cum_num_cherries[i+1]
  nl = bag_h3_results$cum_num_limes[i+1]
  pred_next_lime_bag3 = c(pred_next_lime_bag3, p_dnext_given_d(nc,nl))
}

plot(0:N_max_pulls, pred_next_lime_bag3, pch=1, col="black", 
     ylim=c(0.0,1.0), xlab='Number of Candy Pulls', ylab='Probability that next candy is lime',
     main = "Predictive Prob. That Next Candy is Lime (Assume h3 dist.)")
legend(70, 0.85, legend=c("p(d_next = lime | d)"),
       col=c("black"), pch=1, cex=0.8)

#### ----

# Plotting posteriors after more and more candy pulls (assuming h4 candy distribution)
post_h1_bag4 = c()
post_h2_bag4 = c()
post_h3_bag4 = c()
post_h4_bag4 = c()
post_h5_bag4 = c()
for (i in 0:N_max_pulls){
  nc = bag_h4_results$cum_num_cherries[i+1]
  nl = bag_h4_results$cum_num_limes[i+1]
  post_h1_bag4 = c(post_h1_bag4,p_h1_given_d(nc,nl))
  post_h2_bag4 = c(post_h2_bag4,p_h2_given_d(nc,nl))
  post_h3_bag4 = c(post_h3_bag4,p_h3_given_d(nc,nl))
  post_h4_bag4 = c(post_h4_bag4,p_h4_given_d(nc,nl))
  post_h5_bag4 = c(post_h5_bag4,p_h5_given_d(nc,nl))
}

# Plot results (assuming h4 candy distribution)
plot(0:N_max_pulls, post_h1_bag4, pch=1, col="blue", 
     ylim=c(0.0,1.0), xlab='Number of Candy Pulls', ylab='Posterior Probability',
     main = "Posteriors Assuming Pulling from h4 Candy Distribution")
legend(85, 0.85, legend=c("p(h1 | d)", "p(h2 | d)", "p(h3 | d)", "p(h4 | d)", "p(h5 | d)"),
       col=c("blue", "red","green","yellow","orange"), pch=1:5, cex=0.8)
par(new=T)
plot(0:N_max_pulls, post_h2_bag4, pch=2, col="red", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, post_h3_bag4, pch=3, col="green", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, post_h4_bag4, pch=4, col="yellow", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, post_h5_bag4, pch=5, col="orange", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=F)

# Now let's plot the probability of the next pull being lime given previous pulls
pred_next_lime_bag4 = c()
for (i in 0:N_max_pulls){
  nc = bag_h4_results$cum_num_cherries[i+1]
  nl = bag_h4_results$cum_num_limes[i+1]
  pred_next_lime_bag4 = c(pred_next_lime_bag4, p_dnext_given_d(nc,nl))
}

plot(0:N_max_pulls, pred_next_lime_bag4, pch=1, col="black", 
     ylim=c(0.0,1.0), xlab='Number of Candy Pulls', ylab='Probability that next candy is lime',
     main = "Predictive Prob. That Next Candy is Lime (Assume h4 dist.)")
legend(70, 0.85, legend=c("p(d_next = lime | d)"),
       col=c("black"), pch=1, cex=0.8)

#### ----

# Plotting posteriors after more and more candy pulls (assuming h5 candy distribution)
post_h1_bag5 = c()
post_h2_bag5 = c()
post_h3_bag5 = c()
post_h4_bag5 = c()
post_h5_bag5 = c()
for (i in 0:N_max_pulls){
  nc = bag_h5_results$cum_num_cherries[i+1]
  nl = bag_h5_results$cum_num_limes[i+1]
  post_h1_bag5 = c(post_h1_bag5,p_h1_given_d(nc,nl))
  post_h2_bag5 = c(post_h2_bag5,p_h2_given_d(nc,nl))
  post_h3_bag5 = c(post_h3_bag5,p_h3_given_d(nc,nl))
  post_h4_bag5 = c(post_h4_bag5,p_h4_given_d(nc,nl))
  post_h5_bag5 = c(post_h5_bag5,p_h5_given_d(nc,nl))
}

# Plot results (assuming h4 candy distribution)
plot(0:N_max_pulls, post_h1_bag5, pch=1, col="blue", 
     ylim=c(0.0,1.0), xlab='Number of Candy Pulls', ylab='Posterior Probability',
     main = "Posteriors Assuming Pulling from h5 Candy Distribution")
legend(85, 0.85, legend=c("p(h1 | d)", "p(h2 | d)", "p(h3 | d)", "p(h4 | d)", "p(h5 | d)"),
       col=c("blue", "red","green","yellow","orange"), pch=1:5, cex=0.8)
par(new=T)
plot(0:N_max_pulls, post_h2_bag5, pch=2, col="red", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, post_h3_bag5, pch=3, col="green", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, post_h4_bag5, pch=4, col="yellow", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, post_h5_bag5, pch=5, col="orange", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=F)

# Now let's plot the probability of the next pull being lime given previous pulls
pred_next_lime_bag5 = c()
for (i in 0:N_max_pulls){
  nc = bag_h5_results$cum_num_cherries[i+1]
  nl = bag_h5_results$cum_num_limes[i+1]
  pred_next_lime_bag5 = c(pred_next_lime_bag5, p_dnext_given_d(nc,nl))
}

plot(0:N_max_pulls, pred_next_lime_bag5, pch=1, col="black", 
     ylim=c(0.0,1.0), xlab='Number of Candy Pulls', ylab='Probability that next candy is lime',
     main = "Predictive Prob. That Next Candy is Lime (Assume h5 dist.)")
legend(70, 0.85, legend=c("p(d_next = lime | d)"),
       col=c("black"), pch=1, cex=0.8)

# Problem 2, Part C

# Here we will generate, say 10 random generations of 100 candy pulls from the h1 candy 
# distribution, and then average the posteriors p(h_i | d) over all 10 generations.
N_generations = 10

results_p_h1_bag1 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h2_bag1 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h3_bag1 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h4_bag1 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h5_bag1 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
for (j in 1:N_generations){
  # Pull 100 from h1 candy distribution
  rand_pulls = ifelse(runif(N_max_pulls) <= 0, "Lime", "Cherry") %>%
    data.frame(candy_pull = .) %>%
    mutate(num_limes = ifelse(candy_pull == "Lime", 1, 0),
           num_cherries = ifelse(candy_pull == "Cherry",1,0),
           cum_num_limes = cumsum(num_limes),
           cum_num_cherries = cumsum(num_cherries),
           pull_number = 1:nrow(.)) %>%
    select(pull_number, candy_pull, cum_num_limes, cum_num_cherries) %>%
    bind_rows(data.frame(pull_number=0, candy_pull=NA, cum_num_limes=0, cum_num_cherries=0)) %>%
    arrange(pull_number)
  
  # Compute posteriors for this random generation
  post_h1 = c()
  post_h2 = c()
  post_h3 = c()
  post_h4 = c()
  post_h5 = c()
  for (i in 0:N_max_pulls){
    nc = rand_pulls$cum_num_cherries[i+1]
    nl = rand_pulls$cum_num_limes[i+1]
    post_h1 = c(post_h1,p_h1_given_d(nc,nl))
    post_h2 = c(post_h2,p_h2_given_d(nc,nl))
    post_h3 = c(post_h3,p_h3_given_d(nc,nl))
    post_h4 = c(post_h4,p_h4_given_d(nc,nl))
    post_h5 = c(post_h5,p_h5_given_d(nc,nl))
  }
  
  # Append the corresponding row in the matrices of all results
  results_p_h1_bag1[j,] = post_h1
  results_p_h2_bag1[j,] = post_h2
  results_p_h3_bag1[j,] = post_h3
  results_p_h4_bag1[j,] = post_h4
  results_p_h5_bag1[j,] = post_h5
  
}

# Now we need to average over all generations for each posterior
avg_p_h1_bag1 = colMeans(results_p_h1_bag1)
avg_p_h2_bag1 = colMeans(results_p_h2_bag1)
avg_p_h3_bag1 = colMeans(results_p_h3_bag1)
avg_p_h4_bag1 = colMeans(results_p_h4_bag1)
avg_p_h5_bag1 = colMeans(results_p_h5_bag1)

# Plot averaged results (assuming h1 candy distribution)
plot(0:N_max_pulls, avg_p_h1_bag1, pch=1, col="blue", 
     ylim=c(0.0,1.0), xlab='Number of Candy Pulls', ylab='Averaged Posterior Probability',
     main = "Averaged Posteriors Over 10 Generations (h1 candy dist.)")
legend(85, 0.85, legend=c("p(h1 | d)", "p(h2 | d)", "p(h3 | d)", "p(h4 | d)", "p(h5 | d)"),
       col=c("blue", "red","green","yellow","orange"), pch=1:5, cex=0.8)
par(new=T)
plot(0:N_max_pulls, avg_p_h2_bag1, pch=2, col="red", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, avg_p_h3_bag1, pch=3, col="green", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, avg_p_h4_bag1, pch=4, col="yellow", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, avg_p_h5_bag1, pch=5, col="orange", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=F)


#### ----

# Here we will generate, say 10 random generations of 100 candy pulls from the h2 candy 
# distribution, and then average the posteriors p(h_i | d) over all 10 generations.
results_p_h1_bag2 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h2_bag2 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h3_bag2 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h4_bag2 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h5_bag2 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
for (j in 1:N_generations){
  # Pull 100 from h2 candy distribution
  rand_pulls = ifelse(runif(N_max_pulls) <= 0.25, "Lime", "Cherry") %>%
    data.frame(candy_pull = .) %>%
    mutate(num_limes = ifelse(candy_pull == "Lime", 1, 0),
           num_cherries = ifelse(candy_pull == "Cherry",1,0),
           cum_num_limes = cumsum(num_limes),
           cum_num_cherries = cumsum(num_cherries),
           pull_number = 1:nrow(.)) %>%
    select(pull_number, candy_pull, cum_num_limes, cum_num_cherries) %>%
    bind_rows(data.frame(pull_number=0, candy_pull=NA, cum_num_limes=0, cum_num_cherries=0)) %>%
    arrange(pull_number)
  
  # Compute posteriors for this random generation
  post_h1 = c()
  post_h2 = c()
  post_h3 = c()
  post_h4 = c()
  post_h5 = c()
  for (i in 0:N_max_pulls){
    nc = rand_pulls$cum_num_cherries[i+1]
    nl = rand_pulls$cum_num_limes[i+1]
    post_h1 = c(post_h1,p_h1_given_d(nc,nl))
    post_h2 = c(post_h2,p_h2_given_d(nc,nl))
    post_h3 = c(post_h3,p_h3_given_d(nc,nl))
    post_h4 = c(post_h4,p_h4_given_d(nc,nl))
    post_h5 = c(post_h5,p_h5_given_d(nc,nl))
  }
  
  # Append the corresponding row in the matrices of all results
  results_p_h1_bag2[j,] = post_h1
  results_p_h2_bag2[j,] = post_h2
  results_p_h3_bag2[j,] = post_h3
  results_p_h4_bag2[j,] = post_h4
  results_p_h5_bag2[j,] = post_h5
  
}

# Now we need to average over all generations for each posterior
avg_p_h1_bag2 = colMeans(results_p_h1_bag2)
avg_p_h2_bag2 = colMeans(results_p_h2_bag2)
avg_p_h3_bag2 = colMeans(results_p_h3_bag2)
avg_p_h4_bag2 = colMeans(results_p_h4_bag2)
avg_p_h5_bag2 = colMeans(results_p_h5_bag2)

# Plot averaged results (assuming h2 candy distribution)
plot(0:N_max_pulls, avg_p_h1_bag2, pch=1, col="blue", 
     ylim=c(0.0,1.0), xlab='Number of Candy Pulls', ylab='Averaged Posterior Probability',
     main = "Averaged Posteriors Over 10 Generations (h2 candy dist.)")
legend(85, 0.85, legend=c("p(h1 | d)", "p(h2 | d)", "p(h3 | d)", "p(h4 | d)", "p(h5 | d)"),
       col=c("blue", "red","green","yellow","orange"), pch=1:5, cex=0.8)
par(new=T)
plot(0:N_max_pulls, avg_p_h2_bag2, pch=2, col="red", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, avg_p_h3_bag2, pch=3, col="green", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, avg_p_h4_bag2, pch=4, col="yellow", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, avg_p_h5_bag2, pch=5, col="orange", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=F)

#### ----

# Here we will generate, say 10 random generations of 100 candy pulls from the h3 candy 
# distribution, and then average the posteriors p(h_i | d) over all 10 generations.
results_p_h1_bag3 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h2_bag3 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h3_bag3 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h4_bag3 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h5_bag3 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
for (j in 1:N_generations){
  # Pull 100 from h3 candy distribution
  rand_pulls = ifelse(runif(N_max_pulls) <= 0.5, "Lime", "Cherry") %>%
    data.frame(candy_pull = .) %>%
    mutate(num_limes = ifelse(candy_pull == "Lime", 1, 0),
           num_cherries = ifelse(candy_pull == "Cherry",1,0),
           cum_num_limes = cumsum(num_limes),
           cum_num_cherries = cumsum(num_cherries),
           pull_number = 1:nrow(.)) %>%
    select(pull_number, candy_pull, cum_num_limes, cum_num_cherries) %>%
    bind_rows(data.frame(pull_number=0, candy_pull=NA, cum_num_limes=0, cum_num_cherries=0)) %>%
    arrange(pull_number)
  
  # Compute posteriors for this random generation
  post_h1 = c()
  post_h2 = c()
  post_h3 = c()
  post_h4 = c()
  post_h5 = c()
  for (i in 0:N_max_pulls){
    nc = rand_pulls$cum_num_cherries[i+1]
    nl = rand_pulls$cum_num_limes[i+1]
    post_h1 = c(post_h1,p_h1_given_d(nc,nl))
    post_h2 = c(post_h2,p_h2_given_d(nc,nl))
    post_h3 = c(post_h3,p_h3_given_d(nc,nl))
    post_h4 = c(post_h4,p_h4_given_d(nc,nl))
    post_h5 = c(post_h5,p_h5_given_d(nc,nl))
  }
  
  # Append the corresponding row in the matrices of all results
  results_p_h1_bag3[j,] = post_h1
  results_p_h2_bag3[j,] = post_h2
  results_p_h3_bag3[j,] = post_h3
  results_p_h4_bag3[j,] = post_h4
  results_p_h5_bag3[j,] = post_h5
  
}

# Now we need to average over all generations for each posterior
avg_p_h1_bag3 = colMeans(results_p_h1_bag3)
avg_p_h2_bag3 = colMeans(results_p_h2_bag3)
avg_p_h3_bag3 = colMeans(results_p_h3_bag3)
avg_p_h4_bag3 = colMeans(results_p_h4_bag3)
avg_p_h5_bag3 = colMeans(results_p_h5_bag3)

# Plot averaged results (assuming h3 candy distribution)
plot(0:N_max_pulls, avg_p_h1_bag3, pch=1, col="blue", 
     ylim=c(0.0,1.0), xlab='Number of Candy Pulls', ylab='Averaged Posterior Probability',
     main = "Averaged Posteriors Over 10 Generations (h3 candy dist.)")
legend(85, 0.85, legend=c("p(h1 | d)", "p(h2 | d)", "p(h3 | d)", "p(h4 | d)", "p(h5 | d)"),
       col=c("blue", "red","green","yellow","orange"), pch=1:5, cex=0.8)
par(new=T)
plot(0:N_max_pulls, avg_p_h2_bag3, pch=2, col="red", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, avg_p_h3_bag3, pch=3, col="green", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, avg_p_h4_bag3, pch=4, col="yellow", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, avg_p_h5_bag3, pch=5, col="orange", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=F)

#### ----

# Here we will generate, say 10 random generations of 100 candy pulls from the h4 candy 
# distribution, and then average the posteriors p(h_i | d) over all 10 generations.
results_p_h1_bag4 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h2_bag4 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h3_bag4 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h4_bag4 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h5_bag4 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
for (j in 1:N_generations){
  # Pull 100 from h4 candy distribution
  rand_pulls = ifelse(runif(N_max_pulls) <= 0.75, "Lime", "Cherry") %>%
    data.frame(candy_pull = .) %>%
    mutate(num_limes = ifelse(candy_pull == "Lime", 1, 0),
           num_cherries = ifelse(candy_pull == "Cherry",1,0),
           cum_num_limes = cumsum(num_limes),
           cum_num_cherries = cumsum(num_cherries),
           pull_number = 1:nrow(.)) %>%
    select(pull_number, candy_pull, cum_num_limes, cum_num_cherries) %>%
    bind_rows(data.frame(pull_number=0, candy_pull=NA, cum_num_limes=0, cum_num_cherries=0)) %>%
    arrange(pull_number)
  
  # Compute posteriors for this random generation
  post_h1 = c()
  post_h2 = c()
  post_h3 = c()
  post_h4 = c()
  post_h5 = c()
  for (i in 0:N_max_pulls){
    nc = rand_pulls$cum_num_cherries[i+1]
    nl = rand_pulls$cum_num_limes[i+1]
    post_h1 = c(post_h1,p_h1_given_d(nc,nl))
    post_h2 = c(post_h2,p_h2_given_d(nc,nl))
    post_h3 = c(post_h3,p_h3_given_d(nc,nl))
    post_h4 = c(post_h4,p_h4_given_d(nc,nl))
    post_h5 = c(post_h5,p_h5_given_d(nc,nl))
  }
  
  # Append the corresponding row in the matrices of all results
  results_p_h1_bag4[j,] = post_h1
  results_p_h2_bag4[j,] = post_h2
  results_p_h3_bag4[j,] = post_h3
  results_p_h4_bag4[j,] = post_h4
  results_p_h5_bag4[j,] = post_h5
  
}

# Now we need to average over all generations for each posterior
avg_p_h1_bag4 = colMeans(results_p_h1_bag4)
avg_p_h2_bag4 = colMeans(results_p_h2_bag4)
avg_p_h3_bag4 = colMeans(results_p_h3_bag4)
avg_p_h4_bag4 = colMeans(results_p_h4_bag4)
avg_p_h5_bag4 = colMeans(results_p_h5_bag4)

# Plot averaged results (assuming h4 candy distribution)
plot(0:N_max_pulls, avg_p_h1_bag4, pch=1, col="blue", 
     ylim=c(0.0,1.0), xlab='Number of Candy Pulls', ylab='Averaged Posterior Probability',
     main = "Averaged Posteriors Over 10 Generations (h4 candy dist.)")
legend(85, 0.85, legend=c("p(h1 | d)", "p(h2 | d)", "p(h3 | d)", "p(h4 | d)", "p(h5 | d)"),
       col=c("blue", "red","green","yellow","orange"), pch=1:5, cex=0.8)
par(new=T)
plot(0:N_max_pulls, avg_p_h2_bag4, pch=2, col="red", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, avg_p_h3_bag4, pch=3, col="green", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, avg_p_h4_bag4, pch=4, col="yellow", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, avg_p_h5_bag4, pch=5, col="orange", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=F)

#### ----

# Here we will generate, say 10 random generations of 100 candy pulls from the h5 candy 
# distribution, and then average the posteriors p(h_i | d) over all 10 generations.
results_p_h1_bag5 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h2_bag5 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h3_bag5 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h4_bag5 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
results_p_h5_bag5 = matrix(nrow=N_generations,ncol=N_max_pulls+1,byrow = TRUE)
for (j in 1:N_generations){
  # Pull 100 from h5 candy distribution
  rand_pulls = ifelse(runif(N_max_pulls) <= 1, "Lime", "Cherry") %>%
    data.frame(candy_pull = .) %>%
    mutate(num_limes = ifelse(candy_pull == "Lime", 1, 0),
           num_cherries = ifelse(candy_pull == "Cherry",1,0),
           cum_num_limes = cumsum(num_limes),
           cum_num_cherries = cumsum(num_cherries),
           pull_number = 1:nrow(.)) %>%
    select(pull_number, candy_pull, cum_num_limes, cum_num_cherries) %>%
    bind_rows(data.frame(pull_number=0, candy_pull=NA, cum_num_limes=0, cum_num_cherries=0)) %>%
    arrange(pull_number)
  
  # Compute posteriors for this random generation
  post_h1 = c()
  post_h2 = c()
  post_h3 = c()
  post_h4 = c()
  post_h5 = c()
  for (i in 0:N_max_pulls){
    nc = rand_pulls$cum_num_cherries[i+1]
    nl = rand_pulls$cum_num_limes[i+1]
    post_h1 = c(post_h1,p_h1_given_d(nc,nl))
    post_h2 = c(post_h2,p_h2_given_d(nc,nl))
    post_h3 = c(post_h3,p_h3_given_d(nc,nl))
    post_h4 = c(post_h4,p_h4_given_d(nc,nl))
    post_h5 = c(post_h5,p_h5_given_d(nc,nl))
  }
  
  # Append the corresponding row in the matrices of all results
  results_p_h1_bag5[j,] = post_h1
  results_p_h2_bag5[j,] = post_h2
  results_p_h3_bag5[j,] = post_h3
  results_p_h4_bag5[j,] = post_h4
  results_p_h5_bag5[j,] = post_h5
  
}

# Now we need to average over all generations for each posterior
avg_p_h1_bag5 = colMeans(results_p_h1_bag5)
avg_p_h2_bag5 = colMeans(results_p_h2_bag5)
avg_p_h3_bag5 = colMeans(results_p_h3_bag5)
avg_p_h4_bag5 = colMeans(results_p_h4_bag5)
avg_p_h5_bag5 = colMeans(results_p_h5_bag5)

# Plot averaged results (assuming h5 candy distribution)
plot(0:N_max_pulls, avg_p_h1_bag5, pch=1, col="blue", 
     ylim=c(0.0,1.0), xlab='Number of Candy Pulls', ylab='Averaged Posterior Probability',
     main = "Averaged Posteriors Over 10 Generations (h5 candy dist.)")
legend(85, 0.85, legend=c("p(h1 | d)", "p(h2 | d)", "p(h3 | d)", "p(h4 | d)", "p(h5 | d)"),
       col=c("blue", "red","green","yellow","orange"), pch=1:5, cex=0.8)
par(new=T)
plot(0:N_max_pulls, avg_p_h2_bag5, pch=2, col="red", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, avg_p_h3_bag5, pch=3, col="green", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, avg_p_h4_bag5, pch=4, col="yellow", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=T)
plot(0:N_max_pulls, avg_p_h5_bag5, pch=5, col="orange", 
     ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
par(new=F)
