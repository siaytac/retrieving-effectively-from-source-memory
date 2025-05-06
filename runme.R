rm(list=ls())
graphics.off()

{set.seed(15)} # just allows you to 'step in the same river twice' for debugging

# set the working directory
my.dir <- ""
setwd(my.dir)

# load the functions you will need to run this simulation
source("makevectors.R") # create vectors
source("updatevectors.R") # update vectors after decision
source("recognitiontest.R") # test memory
source("sourcetest.R") # source memory

# set up the parameters
Nf <- 20          # number of features in a vector (vector length)
g <- gsys <- 0.35 # feature frequency
c <- 0.7          # probability of correctly copying a feature
u <- 0.3          # probability of storing an item feature
usource <- 0.1    # probability of storing a source/context feature
criterion <- 1    # criterion for endorsing a probe

nsubj <- 1000     # number of synthetic subjects

# set up the parameters for the experimental design
Ntargets <- 48     # number of targets
Nsources <- 4      # number of unique sources
Nfoils <- 12       # number of foils
Nsourcefoils <- 12 # number of items with a new source

condition <- 1 # you need to set the condition here: 1=Condition 3-0-0, 2=Condition 2-1-0 Strong, 3=Condition 1-1-1, 4=Condition 2-1-0 Weak

source("memory.R") # load the function for Experiment 1 in Aytac et al. (2024)

# run the simulation
data <- memory(Nf, g, gsys, u, usource, c, criterion, condition, nsubj, Ntargets, Nfoils, Nsources, Nsourcefoils)
