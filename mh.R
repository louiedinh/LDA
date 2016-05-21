# target is the target distribution function [e.g gives you unnormalized p(xprime)]
# proposal is the proposal distribution function 
#   [e.g gives you proposed next state xprime = proposal(x, proposal_args)]
# xinit - starting state
# N is the number of samples
# target_args is passed to target [::list]
# proposal_args is passed to proposal [::list]
# proposal_prob, compuates q(xprime|x) and is called as proposal_prob(x, xprime, proposal_args)
MH <- function(target, proposal, xinit, N, target_args, proposal_args, proposal_prob){
  samples <- numeric(N)
  naccept = 0
  x <- xinit
  for (t in 1:N) {
    # Get our proposed sample
    xprime <- do.call(proposal, c(list(mu=x), proposal_args))
    # Figure out acceptance probability  
    px <- do.call(target, c(list(x=x), target_args))
    pxprime <- do.call(target, c(list(x=xprime), target_args))
    qx <- do.call(proposal_prob, c(list(x=xprime, mu=x), proposal_args))
    qxprime <- do.call(proposal_prob, c(list(x=x, mu=xprime), proposal_args))
    alpha <- min(1, (pxprime * qxprime) / (px * qx))
    
    # Accept?
    # is.na is used to protect against underflow
    if(!is.na(alpha) & runif(n=1) < alpha) {
      x <- xprime
      naccept <- naccept + 1
    }
    
    # Record the current sample whether we updated or not.
    samples[t] <- x
  }
  
  samples
}

