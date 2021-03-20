# Aplicação de um Algorítimo Genético

library(GA)

f <- function(x){
  10*x[1] + x[2]
  }

c1 <- function(x){
  x[1] + x[2]
}

fitness <- function(x) 
{ 
  f <- -f(x)                         # we need to maximise -f(x)
  pen <- sqrt(.Machine$double.xmax)  # penalty term
  penalty1 <- max(c1(x),2)*pen        # penalisation for 1st inequality constraint
  penalty <- max(f(x),30)*pen        # penalisation for 1st inequality constraint
  penalty2 <- min(c1(x),1)*pen        # penalisation for 1st inequality constraint
  f - penalty1 - penalty2                        # fitness function value
}

GA <- ga("binary", fitness = fitness, nBits = 2)
summary(GA)
