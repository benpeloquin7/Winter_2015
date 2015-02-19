#Conditional query function using rejection sampling
conditional.query = function(query.var, conditioners, n.sims=1000) {
  #vector to hold our samples
  samples = c()
  
  while (length(samples) < n.sims) {
    #DAG modeled here
    Zeus.angry = flip(.3)
    Zeus.crying = flip(.2)
    thunder = (Zeus.angry && flip(.7)) || flip(.3)
    rain = (Zeus.angry && flip(.8)) || (Zeus.crying && flip(.99))
    forgetful.gardner = flip(.4)
    sprinkler = forgetful.gardner && flip(.7)
    wet.grass = (rain && flip(.8)) || (sprinkler && flip(.9)) || flip(.1)
    
    #vector of truth values for each state
    truth.values = c(Zeus.angry = Zeus.angry, Zeus.crying = Zeus.crying,
    thunder = thunder, rain = rain, sprinkler = sprinkler,
    wet.grass = wet.grass)
    
    #query.index to find index of the states we're looking for
    query.index = which(query.var == names(truth.values))
    #conditioner.indices to find indices for conditioners
    conditioner.indices = which(names(truth.values) %in% conditioners)
    #If we have an instance of all the conditioner states being true then
    #save the state of our query var into samples
    if (all(truth.values[conditioner.indices] == T)) {
      samples = c(samples, truth.values[query.index])
    }
  }
  #Get the proportion of T instances in our total sample space
  #in which the conditioners are true
  return(length(which(samples==T)) / n.sims)
}
