updatevectors <- function(Nf, gsys, studylistreordered, memorytraces, u, c, bestmatch.source, ii, trial, bestmatch) {
  
  randFeatures<-rgeom(Nf, gsys)+1 #Random features
  probe = studylistreordered[(1+Nf*(ii-1)):(Nf*ii), trial] # probe including every feature stored correctly 
  
  trace <- array(memorytraces[(1+Nf*(bestmatch.source-1)):(Nf*bestmatch.source), bestmatch]) 
  randStore <- array(runif(Nf), length(trace))
  randCorr <- array(runif(Nf), length(trace))
  storeWhich <- (randStore<=u & trace==0) # only if the feature equals to zero
  corrWhich <- (randStore<=u & randCorr<c & trace==0)
  
  trace[storeWhich] <- randFeatures[storeWhich]
  trace[corrWhich] <- probe[corrWhich]
  memorytraces[(1+Nf*(bestmatch.source-1)):(Nf*bestmatch.source), bestmatch] = trace
  
  return(memorytraces)
}
