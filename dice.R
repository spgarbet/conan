roll <- function(n, rerolls, trigger, pips)
{
   result <- sample(pips, replace=TRUE, size=n)

   repeat{
     if(rerolls == 0) break
     replace <- FALSE
     
     for(i in 1:length(result))
     {
       if(result[i] < trigger)
       {
         result[i] <- sample(pips, replace=TRUE, size=1)
         replace <- TRUE
         break
       }
     }
     rerolls <- rerolls - 1
     if(!replace) break
   }
   
   sum(result)
}

yellow <- function(n, rerolls=0, trigger=1)  roll(n, rerolls, trigger, c(0, 0, 0, 1, 1, 2))
orange <- function(n, rerolls=0, trigger=1)  roll(n, rerolls, trigger, c(0, 0, 1, 1, 2, 2))
red    <- function(n, rerolls=0, trigger=2)  roll(n, rerolls, trigger, c(0, 1, 1, 2, 2, 3))

expected <- function(FUN, n, rerolls, N=100000) quantile(sapply(1:N, function(x) FUN(n, rerolls)))
