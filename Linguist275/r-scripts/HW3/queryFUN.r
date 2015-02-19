query = function(variable.name, n.sims=1000) {
  sims = sapply(1:n.sims, FUN=rain.sim)
  return(length(which(sims[variable.name,] == T)) / n.sims)
}
