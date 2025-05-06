### RECOGNITION TEST
testmemory <- function(memorytraces, testitem, c, gsys) { 
  
  allikes=matrix(NA, ncol(memorytraces), ncol=1) # set up vector to keep track of every lambda value
  
  for(counttrace in 1:ncol(memorytraces)){ # go through each trace stored in memory
    
    trace<-matrix((memorytraces[1:20,counttrace]), ncol=1) # identify the specific memory trace that will be compared to the test item
    numzeros<-sum((trace==0)) #count up the zeros -- features that are not stored -- because they do not contribute to the memory decision 
    match <- which(testitem == trace) #identify the feautures in the memory trace that match the test item
    mismatch <- which(testitem != trace) #identify the feautures that mismatch
    nummismatch<-(length(mismatch)-numzeros) #count number of mismatching features (note again that this only applies to features that are acutally stored, that is why we subtrace the number of zeros stored)
    
    likehoodfortestitem <- prod(c((c + (1 - c) * gsys * (1 - gsys)^(testitem[match]-1)) / (gsys * (1 - gsys)^(testitem[match]-1)), (1 - c)^nummismatch))#compute lambda value for the match between this specific memory trace and the test item
    
    allikes[counttrace]<-likehoodfortestitem #keep trace of match for every memory trace, the average of these values will be the odds
  }
  return(allikes)
}
