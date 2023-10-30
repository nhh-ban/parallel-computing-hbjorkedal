library(tweedie) 
library(ggplot2)

simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 


# Assignment 2:  
MTweedieTests <-  
  function(N,M,sig){ 
    sum(replicate(M,simTweedieTest(N)) < sig)/M 
  } 


# Assignment 3:  
df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 


library(tictoc)
library(doParallel)


#function to store the time:
printTicTocLog <-
  function() {
    tic.log() %>%
      unlist %>%
      tibble(logvals = .) %>%
      separate(logvals,
               sep = ":",
               into = c("Function type", "log")) %>%
      mutate(log = str_trim(log)) %>%
      separate(log,
               sep = " ",
               into = c("Seconds"),
               extra = "drop")
  }

# The function detectCores finds the number of cores
# available on the machine. We update the "Cores"-value
# to the minimum of the chosen cores and the available cores.
maxcores <- 16
Cores <- min(parallel::detectCores(), maxcores)

# Instantiate the cores:
cl <- makeCluster(Cores)

# Next we register the cluster..
registerDoParallel(cl)

# Take the time as before:
tic(paste0("Parallel loop, ", Cores, " cores"))

Sys.sleep(1)

for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    MTweedieTests( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05) 
} 

# Now that we're done, we close off the clusters
stopCluster(cl)

toc(log = TRUE)
