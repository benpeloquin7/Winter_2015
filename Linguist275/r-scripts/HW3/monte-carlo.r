query = function(variable.name, n.sims = 500) {
  sims = sapply(1:n.sims, FUN=rain.sim)
  proportion = length(which(sims[variable.name,] == T)) / n.sims
  return(proportion)
}
