
memory = function(Nf, g, gsys, u, usource, c, criterion, condition, nsubj, Ntargets, Nfoils, Nsources, Nsourcefoils) {
  
  data <- list() # create an empty list to save the simulated data
  
  for (subj in 1:nsubj){
    
    # create items
    prototype <- matrix((rgeom(1*Nf, g)+1),ncol=1) 
    targets <- makevectors(Nf, g, Ntargets, g, 1, 0, prototype) # note values of u is 1 and c is 0 because we want every ...
    foils <- makevectors(Nf, g, Nfoils, g, 1, 0, prototype)     # feature to be stored and stored correctly for items
    
    # create unique sources
    source <- makevectors(Nf, g, Nsources, g, 1, 0, prototype)  
    
    # create sources for foils in case falsely recognized old - relevant to the experimental design 
    source.for.foils <- rep(source, 3)
    source.for.foils <- matrix(source.for.foils, Nf, Nsourcefoils)
    foilswithsource <- rbind(foils, source.for.foils)
    
    # repeat these sources and save in a matrix
    sources <- matrix(rep(source, Ntargets/Nsources), Nf, Ntargets)
    
    repetition.one <- rbind(targets, sources)
    
    lists.for.study <- list(repetition.one, repetition.one, repetition.one)
    
    # create other sources for Conditions 2 & 4 in Aytac et al. (Experiment 1, 2024)
    if (condition == 2 || condition == 4) {
      mixitup2 <- cbind(sources[,-1], sources[,1])
      repetition.two <- rbind(targets, mixitup2)
      
      lists.for.study <- list(repetition.one, repetition.one, repetition.two)
      
      sources <- rbind(sources, mixitup2)
    }
    
    # create one more for Condition 3 in Aytac et al.'s Exp 1
    if (condition == 3) {
      
      mixitup <- cbind(sources[,-1], sources[,1])
      repetition.two <- rbind(targets, mixitup)
      
      mixitup2 <- cbind(sources[,-1:-2], sources[,1:2])
      repetition.three <- rbind(targets, mixitup2)
      
      lists.for.study <- list(repetition.one, repetition.two, repetition.three)
      
      sources <- rbind(sources, mixitup, mixitup2)
    }
    
    # append item vectors to source vectors 
    studylist <- rbind(targets, sources)
    
    # create empty memory traces to make updating easier
    memorytraces <- matrix(NA, Nf*2, Ntargets)
    store.house <- split(memorytraces, rep(1:ncol(memorytraces), each=nrow(memorytraces))) #make it list
    
    # randomize the repetition order for each subject
    lists.for.study <- sample(lists.for.study,3)
    
    
    ##### Study Phase: Every Presentation Initiates Recognition and Source Decisions #####
    
    for (repetitions in 1:3) {
      
      # pick the list
      study.list <- lists.for.study[[repetitions]]
      
      # randomize order of items for additional study
      studyOrder <- sample.int(Ntargets)
      studylistreordered <- study.list[,studyOrder]
      
      for (trial in 1:Ntargets) {
        
        ## Step 1: Item Decisions
        itemprobe <- matrix(studylistreordered[1:Nf, trial], ncol=1) 
        allikes <- testmemory(memorytraces, itemprobe, c, gsys) 
        odds <- mean(allikes[!is.na(allikes)])
        
        if (is.na(odds) || odds<criterion) {
          
          newvectoritem <- makevectors(Nf, g, 1, gsys, u, c, studylistreordered[1:Nf,trial])
          newvectorsource <- makevectors(Nf, g, 1, gsys, usource, c, studylistreordered[(Nf+1):(Nf*2),trial])
          newvector <- rbind(newvectoritem, newvectorsource)
          
          if (repetitions == 1) {
            
            store.house[[trial]] <- newvector
            memorytraces <- sapply(store.house, "length<-", max(lengths(store.house)))
            
          } else {
            
            leng <- length(store.house)
            store.house[[leng+1]] <- newvector
            memorytraces <- sapply(store.house, "length<-", max(lengths(store.house)))
            
          }
          
          
        } else if (odds >= criterion) {
          
          # find the best matching trace
          bestmatch <- which.max(allikes)
          
          # update the item vector 
          memorytraces <- updatevectors(Nf, gsys, studylistreordered, memorytraces, u, c, 1, 1, trial, bestmatch)
          
          ## Step 2: Source Decisions
          sourceprobe <- matrix(studylistreordered[(Nf+1):(Nf*2), trial], ncol=1)
          n <- length(memorytraces[, bestmatch][(!is.na(memorytraces[, bestmatch]))])
          n <- n/Nf-1
          
          sourcealllikes <- testsourcememory(memorytraces, sourceprobe, c, gsys, n, bestmatch)
          
          sourceodds <- mean(sourcealllikes[(!is.na(sourcealllikes))])
          
          # if source odd is lower than criterion, add the probe as a new source
          if (sourceodds < criterion) {
            
            newvector <- makevectors(Nf, g, 1, gsys, usource, c, studylistreordered[(Nf+1):(Nf*2),trial])
            store.house[[bestmatch]] <- c(store.house[[bestmatch]], newvector)
            memorytraces <- sapply(store.house, "length<-", max(lengths(store.house))) 
            
            # if higher than criterion, update source trace
          } else if (sourceodds >= criterion) {
            
            bestmatch.source <- which.max(sourcealllikes)
            bestmatch.source <- bestmatch.source+1 # because source is appended to item vector
            
            # update source vector
            memorytraces <- updatevectors(Nf, gsys, studylistreordered, memorytraces, usource, c, bestmatch.source, 2, trial, bestmatch) 
            
          }  
          
        }
        
      } # trial
    } # repetitions
    
    
    ##### Test Phase: Each Test Probe Initiates Item and Source Recognition Tests #####
    
    # this sets up variables to track of the output for each simulated person
    keepanswer <- matrix(NA, Ntargets+Nfoils, ncol=2)
    keepodds <- matrix(NA, Ntargets+Nfoils, ncol=2)
    keeptesttype <- matrix(NA, Ntargets+Nfoils, ncol=2)
    
    # create a test list for item recognition
    testlist <- cbind(studylist[1:Nf,], foils)
    targ <- array('targ', Ntargets) 
    foil <- array('foil', Nfoils)
    testtype <- c(targ,foil)
    
    testOrder <- sample.int(ncol(testlist))    
    testlistreordered <- testlist[,testOrder]
    testtypereordered <- testtype[testOrder]  #reorder the labels in the same order as the stimuli
    
    # create a test list for source recognition
    sourcelist = studylist[(Nf+1):(Nf*2),] 
    
    if (condition == 1) {
      
      r <- sample(1:3, 1)
      sourcelist[,((Ntargets-Nsourcefoils+1)-r):(Ntargets-r)] <- sourcelist[,(Ntargets-Nsourcefoils+1):Ntargets]
      
    } else if (condition == 2) {
      
      r <- sample(2:3,1)  
      sourcelist[,((Ntargets-Nsourcefoils+1)-r):(Ntargets-r)] <- sourcelist[,(Ntargets-Nsourcefoils+1):Ntargets]
      
    } else if (condition == 3){
      
      r <- sample(1:3,1)
      sourcelist <- studylist[(1+Nf*(r)):(Nf*(r+1)),]
      sourcelist[,((Ntargets-Nsourcefoils+1)-(4-r)):(Ntargets-(4-r))] <- sourcelist[,(Ntargets-Nsourcefoils+1):Ntargets] #the only source left 
      
    } else  {
      
      sourcelist <- studylist[(Nf*2+1):(Nf*3),] # pick the weak list for Condition 4
      r <- sample(1:2,1)  
      sourcelist[,((Ntargets-Nsourcefoils+1)-r):(Ntargets-r)] <- sourcelist[,(Ntargets-Nsourcefoils+1):Ntargets]
      
    } 
    
    sourcetestlist <- rbind(studylist[1:Nf,], sourcelist)
    sourcetestlist <- cbind(sourcetestlist, foilswithsource)
    
    sourcetarg <- array('sourcetarg', Ntargets-(Nsourcefoils+r)) 
    sourcefoil <- array('sourcefoil', Nsourcefoils)
    sourcetargadd <- array('sourcetarg', r)
    sourcetesttype <- c(sourcetarg, sourcefoil, sourcetargadd)
    
    # now you have the source test list including foil and targets along with their in/correct sources
    sourcetestreordered <- sourcetestlist[,testOrder] 
    sourcetesttypeordered <- sourcetesttype[testOrder]
    
    ## test each probe
    for (trial in 1:ncol(testlistreordered)) {  
      
      testitem <- matrix((testlistreordered[, trial]), ncol=1)  # get the test item
      allikes <- testmemory(memorytraces, testitem, c, gsys) # call function to test memory
      
      odds <- mean(allikes[!is.na(allikes)]) # because this is recognition
      
      answer <- 0 # could do this many ways, here I say the answer is 0 ("new")
      if (odds >= criterion) {
        answer <- 1.0 } # unless the odds value exceeds the criterion in which case you say "old" 
      
      # book keeping for an individual test trial
      keepodds[trial] <- odds
      keepanswer[trial] <- answer
      keeptesttype[trial] <- testtypereordered[trial]
      
      # find the best matching trace to compare its source with the test source
      bestmatch <- which.max(allikes) 
      
      # source memory
      if (answer == 1) { # if yes for item
        
        testsource <- matrix((sourcetestreordered[,trial]), ncol=1)
        testsource <- matrix(testsource[(Nf+1):(Nf*2)], ncol=1)
        
        n <- length(memorytraces[,bestmatch][(!is.na(memorytraces[,bestmatch]))])
        n <- n/Nf-1
        
        sourcealllikes <- testsourcememory(memorytraces, testsource, c, gsys, n, bestmatch)
        
        sourcealllikes <- mean(sourcealllikes[(!is.na(sourcealllikes))]) # take the mean of all likelihood ratios
        
        answerSource <- 0  # no for source
        if (sourcealllikes >= criterion) { # yes for source
          answerSource <- 1.0 }
        
        keepodds[trial,2] <- sourcealllikes
        keepanswer[trial,2] <- answerSource
        keeptesttype[trial,2] <- sourcetesttypeordered[trial]
        
      }
      
      
    } # trial
    
    datbyS <- data.frame(oddss=keepodds, answers=keepanswer, testtypes=keeptesttype) # save as a data frame
    data[[subj]] <- datbyS # save the subject's data
    
  } # subject
  
  
  return(data)
  
} # memory
