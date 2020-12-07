#### Eduarda Chagas
#### 04.04.2020
#### This script contains functions that calculate different types of ordinal pattern distribution.


#' Calculates all ordinal patterns with a given dimension
#' @keywords internal
#' @import gtools
.DefineSymbols <- function(dimension){
  d = c(1:dimension)
  symbol = matrix(unlist(permutations(n=dimension,r=dimension,v=d)),nrow = factorial(dimension),ncol = dimension,byrow = FALSE)
  symbol = symbol - 1
  return(symbol)
}


#' Calculates the ordinal patterns of a time series
#' @keywords internal
#' @import gtools
#' @import stats
.FormationPattern <- function(serie, dimension, delay, option){
  n.symbols = i = 1
  n = length(serie)
  p.patterns = elements = index2 = matrix(nrow = n, ncol = dimension)
  index = c(0:(dimension-1))
  while(i <= n){
    first = i
    if((i+dimension-1)<=n){
      index2[n.symbols,] = i:(i+dimension-1)
      elements[n.symbols,] = serie[i:(i+dimension-1)]
      p.patterns[n.symbols,] = index[order(elements[n.symbols,])]
      i = first + delay
      n.symbols = n.symbols + 1
    }else break
  }
  if(option == 0){
    p.patterns = na.omit(p.patterns)
    return(p.patterns[1:dim(p.patterns)[1],])
  }else if(option == 1){
    elements = na.omit(elements)
    return(elements[1:dim(elements)[1],])
  }else{
    index2 = na.omit(index2)
    return(index2[1:dim(index2)[1],])
  }
}


#' Calculates the weights required in calculating the weighted permutation entropy
#' @keywords internal
.Weights <- function(series, dimension, delay){
  groups = .FormationPattern(series,dimension,delay,1)
  weight = rep(0, dim(groups)[1])
  for(i in 1:dim(groups)[1]){
    weight[i] = (sum((groups[i,] - mean(groups[i,]))^2))/dimension
  }
  return(weight)
}


#' Calculates the Weighted Bandt-Pompe distribution of the given time series
#' @export
#' @usage WPE(series, dimension, delay)
#' @param series A numeric vector (e.g. a time series)
#' @param dimension Dimension size of ordinal patterns
#' @param delay Size of the embedding delay of ordinal patterns
#' @return A numeric vector containing the probability distribution
#' @references Fadlallah, Bilal, et al. "Weighted-permutation entropy: A complexity measure for time series incorporating amplitude information." Physical Review E 87.2 (2013): 022911.
#' @author Eduarda Chagas
#' @examples
#' set.seed(123, kind = "Mersenne-Twister")
#' x <- runif(110000)
#' d <- 3
#' del <- 1
#' WPE(series = x, dimension = d, delay = del)
WPE <- function(series, dimension, delay){
  series = unlist(series)
  w = .Weights(series,dimension,delay)
  symbols = .FormationPattern(series,dimension,delay,0)
  patterns = .DefineSymbols(dimension)
  sw = rep(0,factorial(dimension))

  for(i in 1:factorial(dimension)){
    for(j in 1:dim(symbols)[1]){
      if(all(symbols[j,] != patterns[i,]))
        sw[i] = sw[i] + w[j]
    }
  }

  pw = sw/sum(sw)
  return(pw)
}


#' Calculates the Bandt-Pompe distribution of the given time series
#' @export
#' @usage BandtPompe(series, dimension, delay)
#' @param series A numeric vector (e.g. a time series)
#' @param dimension Dimension size of ordinal patterns
#' @param delay Size of the embedding delay of ordinal patterns
#' @return A numeric vector containing the probability distribution
#' @references Bandt, Christoph, and Bernd Pompe. "Permutation entropy: a natural complexity measure for time series." Physical review letters 88.17 (2002): 174102.
#' @author Eduarda Chagas
#' @examples
#' set.seed(123, kind = "Mersenne-Twister")
#' x <- runif(110000)
#' d <- 3
#' del <- 1
#' BandtPompe(series = x, dimension = d, delay = del)
BandtPompe <- function(series, dimension, delay){
  fat = factorial(dimension)
  probability = rep(0,fat)
  p.patterns = .FormationPattern(series, dimension, delay, 0)
  n.symbols = dim(p.patterns)[1]
  symbols = .DefineSymbols(dimension)
  for(i in 1:fat){
    for(j in 1:n.symbols){
      if(all(p.patterns[j,] == symbols[i,])){
        probability[i] = probability[i] + 1
        break
      }
    }
  }
  return(probability/n.symbols)
}


#' Returns the indices of the ordinal patterns. The indices of the patterns followed are done by ordering the positions of the groups in chronological order (Permutation of Classification)
#' @keywords internal
.PatternWedding <- function(patterns){
  if(max(patterns) == 2) patterns = patterns + 1
  m = dim(patterns)[1]
  D = dim(patterns)[2]
  symbols = .DefineSymbols(D)
  wedding = rep(0, m)
  for(i in 1:m){
    e = 0
    j = 1
    stop = F
    while(j <= factorial(D) && stop == F){
      if(sum(symbols[j,] == patterns[i,]) == D){
        wedding[i] = j
        stop = T
      }
      j = j + 1
    }
  }
  return(wedding)
}

#' Calculates the transition graph of ordinal pattern for the given time series
#' @export
#' @usage TransitionGraphs(series, dimension, delay)
#' @param series A numeric vector (e.g. a time series)
#' @param dimension Dimension size of ordinal patterns
#' @param delay Size of the embedding delay of ordinal patterns
#' @return A numeric matrix containing the graph
#' @references Borges, João B., et al. "Learning and distinguishing time series dynamics via ordinal patterns transition graphs." Applied Mathematics and Computation 362 (2019): 124554.
#' @author Eduarda Chagas
#' @examples
#' set.seed(123, kind = "Mersenne-Twister")
#' x <- runif(110000)
#' d <- 3
#' del <- 1
#' TransitionGraphs(series = x, dimension = d, delay = del)
TransitionGraphs <- function(series, dimension, delay){

  graph = matrix(0, nrow = factorial(dimension), ncol = factorial(dimension))
  patterns = .FormationPattern(series, dimension, delay, 0)
  wedding = .PatternWedding(patterns)
  m = length(wedding)

  for(i in 1:(m-1)){
    graph[wedding[i], wedding[i+1]] = graph[wedding[i], wedding[i+1]] + 1
  }

  graph = graph/(m-1)
  return(graph)
}


#' Calculates the weighted transition graph of ordinal pattern for the given time series
#' @export
#' @usage WeightTransitionGraph(series, dimension, delay)
#' @param series A numeric vector (e.g. a time series)
#' @param dimension Dimension size of ordinal patterns
#' @param delay Size of the embedding delay of ordinal patterns
#' @return A numeric matrix containing the graph
#' @references Chagas, Eduarda T. C., et al. "Analysis and Classification of SAR Textures using Information Theory" (2020)
#' @author Eduarda Chagas
#' @examples
#' set.seed(123, kind = "Mersenne-Twister")
#' x <- runif(110000)
#' d <- 3
#' del <- 1
#' WeightTransitionGraph(series = x, dimension = d, delay = del)
WeightTransitionGraph <- function(series, dimension, delay){

  graph = matrix(0, nrow = factorial(dimension), ncol = factorial(dimension))
  patterns = .FormationPattern(series, dimension, delay, 0)
  elements = .FormationPattern(series, dimension, delay, 1)
  wedding = .PatternWedding(patterns)
  m = length(wedding)
  weight.total = 0

  for(i in 1:(m-1)){
    weight.i1 = (max(elements[i,]) - min(elements[i,]))
    weight.i2 = (max(elements[i+1,]) - min(elements[i+1,]))
    graph[wedding[i], wedding[i+1]] = graph[wedding[i], wedding[i+1]] + abs(weight.i1 - weight.i2)
    weight.total = weight.total + abs(weight.i1 - weight.i2)
  }

  graph = graph/weight.total
  return(graph)
}
