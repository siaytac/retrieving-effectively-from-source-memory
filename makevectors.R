makevectors <- function(Nf, g, Nitemstomake, gsys, u, c, prototype) {
  
  #make items 
  studyList=matrix(prototype,Nf, Nitemstomake) # makes Nitemstomake copies of the item vector called prototype, each with Nf features
  
  #make copies (w/imperfect encoding (u)=> memory OR w/o, if u=1, to make similar study items)  
  numFeatures<-length(studyList) #How many features are there?
  randFeatures<-rgeom(numFeatures, g)+1 #make actual feature values that will serve as the item vectors
  
  traces<-array(0, dim(studyList)) #Make an empty array to hold the stored traces, note that 0 is important here - it means that a feature value is not stored and therefore does not contribute to the memory decision
  randStore<-array(runif(numFeatures),dim(studyList)) #Generate an array of uniform probabilities that we'll use to decide what should be stored
  randCorr<-array(runif(numFeatures),dim(studyList)) #Generate an array of uniform probabilities that we'll use to decide which of those store will be stored at the correct value
  storeWhich<-randStore<=u # Create an index for the features that will be stored
  corrWhich<-(randStore<=u & randCorr < c) # Create an index for the features that will be stored correctly
  traces[storeWhich] <-randFeatures[storeWhich] #copy those features to be stored into traces (this results in the errors of encoding)
  traces[corrWhich] <-studyList[corrWhich] #copy those features from the study list (this results in the correct storage of features)
  
  return(traces)
}
