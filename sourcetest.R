### SOURCE MEMORY TEST
testsourcememory <- function(memorytraces, testsource, c, gsys, n, bestmatch) {
  
  sourcealllikes=matrix(NA, n, ncol=1)
  
  for(counttrace in 1:n){ #go through each source
    
    trace = matrix((memorytraces[, bestmatch]), ncol=1) #best matching trace source+item
    trace = matrix(trace[(1+20*(counttrace)):(20*(counttrace+1))], ncol=1) #best matching trace source
    numzeros = sum((trace == 0))
    match = which(testsource == trace)  
    mismatch = which(testsource != trace) 
    nummismatch = (length(mismatch)-numzeros) 
    
    likehoodfortestsource = prod(c((c + (1 - c) * gsys * (1 - gsys)^(testsource[match]-1)) / (gsys * (1 - gsys)^(testsource[match]-1)), (1 - c)^nummismatch))
    
    sourcealllikes[counttrace] = likehoodfortestsource 
  }  
  
  return(sourcealllikes)
  
}
