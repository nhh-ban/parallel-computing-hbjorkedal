library(tictoc)
library(dplyr)

script_path <- "scripts/"

solutions <- c("script1.R", "script2.R", "script3.R")

for(solution in solutions) {
  tic(paste("Timing", solution))
  
  # Source the script
  source(paste0(script_path, solution))
  
  # Stop timer and print results
  toc(log = TRUE)
}

# The third method is the fastest. It is likely due to that is manages to split 
# the work better along the cores. The second solution is the worst.
# This may be due to that is loads in one task at the time, not taking advantage of the
# available cores. It may be that all cores runs the same work.