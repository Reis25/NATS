############################################# Packages #############################################

if(!require(gtools)) install.packages("gtools")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(RColorBrewer)) install.packages("RColorBrewer")



###################################### Fisher Plane ################################################

FS.Plane <- function(prob, dimension, xl, yl){
  
  p = NULL

  if(!is.null(prob)){
    
    prob2 = matrix(unlist(prob), ncol = dimension)
    if(is.null(dim(prob2)[1])) Series = 1
    else Series = c(1:dim(prob2)[1])
    
    fisher = sapply(prob, fisher.entropy)
    shannon = sapply(prob, shannon.entropy.normalized)
    Series = factor(Series)
    
    XMIN = 0
    XMAX = 1
    YMIN = 0
    YMAX = 1
    
    if(dim(prob2)[1] == 1)
      brbg_hcl = "#000000"
    else
      brbg_hcl = colorspace::diverging_hcl(dim(prob2)[1], palette = "Cork", h2 = 0)
    
    p = qplot(xlab = "Shannon Entropy", ylab = "Fisher Information measure") + 
      geom_point(aes(x = shannon, y = fisher, group = Series, color = Series), shape = 8, size = 4) +
      scale_colour_manual(brbg_hcl, aesthetics = "colour") +
      theme_few(base_size = 14, base_family = "serif")
    #+ scale_colour_few("Dark")
    #p = p + scale_colour_manual(values = brbg_hcl, aesthetics = "colour")
    if(!is.null(xl) && !is.null(yl))
      p = p + coord_cartesian(xlim = xl, ylim = yl, expand = FALSE)
    else
      p = p + xlim(limits=c(XMIN, XMAX)) + ylim(limits=c(YMIN, YMAX))
    
  }
    
  
  return(p)
} 

###################################### HC Plane ####################################################

HC.Plane <- function(prob, dimension, xl, yl){
  
  p = NULL
  
  if(!is.null(prob)){
    prob2 = matrix(unlist(prob), ncol = dimension)
    if(is.null(dim(prob2)[1])) Series = 1
    else Series = c(1:dim(prob2)[1])
    
    shannon = sapply(prob, shannon.entropy.normalized)
    complexity = sapply(prob, Ccomplexity)
    Series = factor(Series)
    
    if(dim(prob2)[1] == 1)
      brbg_hcl = "#000000"
    else
      brbg_hcl = colorspace::diverging_hcl(dim(prob2)[1], palette = "Cork", h2 = 0)
    
    p = cotas(dimension)
    p = p +
      geom_point(aes(x = shannon, y = complexity, group = Series, color = Series), shape = 8, size = 4) +
      scale_colour_manual(values = brbg_hcl) +
      labs(x="Shannon Entropy", y="Statistical Complexity") + 
      theme_few(base_size = 14, base_family = "serif")
      #+ scale_colour_few("Dark") 
    #p = p + scale_colour_manual(brbg_hcl, aesthetics = "colour")
    if(!is.null(xl) && !is.null(yl))
      p = p + coord_cartesian(xlim = xl, ylim = yl, expand = FALSE)
    
  }
  
  return(p)
}

###################################### Time Series Plane ################################################

time.series.plane <- function(series, xl, yl){
  p = NULL
  if(length(series) > 0){
    p = qplot(x = c(1:length(series)), y = series, geom = "line", xlab = "Values", ylab = "Series") +
      theme(plot.title = element_text(hjust=0.5)) +  
      theme_few(base_size = 14, base_family = "serif") +
      scale_colour_few("Dark")
    if(!is.null(xl) && !is.null(yl))
      p = p +  coord_cartesian(xlim = xl, ylim = yl, expand = FALSE)
  }
  return(p)
}

patterns.on.graph <- function(series, dimension, delay, number.pattern, xl, yl){
  p = NULL
  
  if(as.integer(number.pattern) <= factorial(dimension)){
    lengthW = 0
    symbols = define.symbols(dimension)
    point.time = point.value = c(1:length(series))
    p.patterns = formationPattern(series, dimension, delay, 0)
    elements = formationPattern(series, dimension, delay, 1)
    index = formationPattern(series, dimension, delay, 2)
    n.symbols = dim(p.patterns)[1]
    
    for(i in 1:n.symbols){
      if(all(p.patterns[i,] == symbols[as.integer(number.pattern),])){
        lengthW = lengthW + 1
        point.value[lengthW] = elements[i,1]
        point.time[lengthW] = index[i,1]
      }
    }
    
    if(lengthW >= 1){
      p = qplot(x = c(1:length(series)),y = series, geom="line", xlab="Values", ylab="Series") +
        geom_point(aes(x = point.time[1:lengthW], y = point.value[1:lengthW]), color = "blue")
    }
    else{
      p = qplot(x = c(1:length(series)),y = series, geom="line", xlab="Values", ylab="Series")
    }
    p = p  +  
      theme(plot.title = element_text(hjust=0.5)) +
      theme_few(base_size = 14, base_family = "serif") +
      scale_colour_few("Dark") 
    if(!is.null(xl) && !is.null(yl))
      p = p +  coord_cartesian(xlim = xl, ylim = yl, expand = FALSE)
    
  }
  return(p)
}

###################################### Histogram #########################################################

histogram <- function(series, dimension, delay){
  p = NULL
  if(length(series) > 0){
    fat = factorial(dimension)
    p.patterns = formationPattern(series, dimension, delay,0)
    n.symbols = dim(p.patterns)[1]
    symbol = define.symbols(dimension)
    index.rep = array(0, n.symbols)
    for(i in 1:n.symbols){
      for(j in 1:fat){
        if(all(p.patterns[i,]==symbol[j, ])){
          index.rep[i]=j
          break
        }
      }
    }
    index.rep = index.rep[1:n.symbols]
    index.rep = data.frame(i = index.rep)
    p = ggplot(index.rep) +
      geom_histogram(aes(x = i, y = ..density..),
                     binwidth = 1, fill = "grey", color = "black")+ 
      labs(x="Patterns", y="Probability") +
      theme_few(base_size = 14, base_family = "serif") 
    
  }
  return(p)
}


