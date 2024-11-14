# quick demonstration: https://docs.google.com/spreadsheets/d/1D3g9lWAuQZLnhTfk_nwV8CDNRBPPFW6jx4r3iPAB-lI/edit?gid=0#gid=0

get_at_least_one <- function(n, proba_n){

  # remove NAs from the vector
  proba_n <- proba_n[!is.na(proba_n)]
  
  if(length(proba_n)<n){n <- length(proba_n)}
  
  if(length(proba_n)==0){p <- NA
  } else {
    # p(X-0)
    p_0 <- 1
    for(i in 1:n){
      p_0 <- p_0*(1-proba_n[i])
    }
    # p(X>=1) = 1 - p(X=0) = 1-(1-p)^n 
    p <- 1-p_0
  }
  
  return(p)
}


get_at_least_two <- function(n, proba_n){
  
  # remove NAs from the vector
  proba_n <- proba_n[!is.na(proba_n)]
  
  if(length(proba_n)<n){n <- length(proba_n)}
  
  if(length(proba_n) %in% c(0,1)) {
    p <- NA
  
  } else {
    # p(X=0)
    p_0 <- 1
    for(i in 1:n){
      p_0 <- p_0*(1-proba_n[i])
    }
    # p(X=1)
    p_1 <- 0
    for(i in 1:n){
      p_1 <- p_1 + proba_n[i]*prod(sapply(proba_n[-i], function(x) 1-x))
    }
    # p(X>=2) = 1 - p(X=0) - p(X=1)
    p <- 1-p_0-p_1
  }
  return(p)
}


get_at_least_three <- function(n, proba_n){
  
  # remove NAs from the vector
  proba_n <- proba_n[!is.na(proba_n)]
  
  if(length(proba_n)<n){n <- length(proba_n)}
  
  if(length(proba_n) %in% c(0,1,2)) {
    p <- NA
    
  } else {
    # p(X-0)
    p_0 <- 1
    for(i in 1:n){
      p_0 <- p_0*(1-proba_n[i])
    }
    # p(X=1)
    p_1 <- 0
    for(i in 1:n){
      p_1 <- p_1 + proba_n[i]*prod(sapply(proba_n[-i], function(x) 1-x))
    }
    # p(X=2)
    p_2 <- 0
    combs <- combn(n,2)
    for(i in 1:ncol(combs)){
      names_of_n <- c(1:n)
      minus <- names_of_n[!names_of_n %in% combs[,i]]
      p_2 <- p_2 + proba_n[combs[1,i]]*proba_n[combs[2,i]]*prod(sapply(proba_n[minus], function(x) 1-x))       
    }
    # p(X>=3) = 1 - p(X=0) - p(X=1) - p(X=2)
    p <- 1-p_0-p_1-p_2
  }
  
  return(p)
}


get_at_least_half <- function(n, proba_n){
  
  # remove NAs from the vector
  proba_n <- proba_n[!is.na(proba_n)]
  if(length(proba_n)<n){n <- length(proba_n)}
  
  # calculate the probabilities depending on length(n)
  if(n %in% c(0,1)){
    # method: p(X>=50%) cannot be estimated when resources are 0 or 1
    p <- NA
    
  } else if (n==2){
    # method: p(X>=50%) is estimated with p(X>=1)
    # p(X=0)
    p_0 <- 1
    for(i in 1:n){p_0 <- p_0*(1-proba_n[i])}
    p <- 1-p_0
    rm(p_0)
    
  } else if (n %in% c(3,4)){
    # method: p(X>=50%) is estimated with p(X>=2)
    # p(X=0)
    p_0 <- 1
    for(i in 1:n){p_0 <- p_0*(1-proba_n[i])}
    # p(X=1)
    p_1 <- 0
    for(i in 1:n){p_1 <- p_1 + proba_n[i]*prod(sapply(proba_n[-i], function(x) 1-x))}
    # final p
    p <- 1-p_0-p_1
    rm(p_0, p_1)
    
  } else if (n %in% c(5,6)){
    # method: p(X>=50%) is estimated with p(X>=3)
    # p(X=0)
    p_0 <- 1
    for(i in 1:n){p_0 <- p_0*(1-proba_n[i])}
    # p(X=1)
    p_1 <- 0
    for(i in 1:n){p_1 <- p_1 + proba_n[i]*prod(sapply(proba_n[-i], function(x) 1-x))}
    # p(X=2)
    p_2 <- 0
    combs <- combn(n,2)
    for(i in 1:ncol(combs)){
      names_of_n <- c(1:n)
      minus <- names_of_n[!names_of_n %in% combs[,i]]
      p_2 <- p_2 + proba_n[combs[1,i]]*proba_n[combs[2,i]]*prod(sapply(proba_n[minus], function(x) 1-x))       
    }
    # final p
    p <- 1-p_0-p_1-p_2
    rm(p_0, p_1, p_2)
  } 
  return(p)
}


get_at_least_half_bis <- function(n, proba_n){
  
  # remove NAs from the vector
  proba_n <- proba_n[!is.na(proba_n)]
  if(length(proba_n)<n){n <- length(proba_n)}
  
  # calculate the probabilities depending on length(n)
  if(n %in% c(0,1)){
    # method: p(X>=50%) cannot be estimated when resources are 0 or 1
    p <- NA
    
  } else if (n==2){
    # method: p(X>=50%) is estimated with p(X>=1)
    # p(X=0)
    p_0 <- 1
    for(i in 1:n){p_0 <- p_0*(1-proba_n[i])}
    p <- 1-p_0
    rm(p_0)
    
  } else if (n==3){
    # method: p(X>=50%) is estimated with mean[p(X>=1),p(X>=2)]
    # p(X=0)
    p_0 <- 1
    for(i in 1:n){p_0 <- p_0*(1-proba_n[i])}
    # p(X=1)
    p_1 <- 0
    for(i in 1:n){p_1 <- p_1 + proba_n[i]*prod(sapply(proba_n[-i], function(x) 1-x))}
    # final p
    p_a <- 1-p_0
    p_b <- 1-p_0-p_1
    p <- mean(c(p_a,p_b))
    rm(p_0, p_1)
    
  } else if (n==4){
    # method: p(X>=50%) is estimated with p(X>=2)
    # p(X=0)
    p_0 <- 1
    for(i in 1:n){p_0 <- p_0*(1-proba_n[i])}
    # p(X=1)
    p_1 <- 0
    for(i in 1:n){p_1 <- p_1 + proba_n[i]*prod(sapply(proba_n[-i], function(x) 1-x))}
    # final p
    p <- 1-p_0-p_1
    rm(p_0, p_1)
    
  } else if (n==5){
    # method: p(X>=50%) is estimated with mean[p(X>=2),p(X>=3)]
    # p(X=0)
    p_0 <- 1
    for(i in 1:n){p_0 <- p_0*(1-proba_n[i])}
    # p(X=1)
    p_1 <- 0
    for(i in 1:n){p_1 <- p_1 + proba_n[i]*prod(sapply(proba_n[-i], function(x) 1-x))}
    # p(X=2)
    p_2 <- 0
    combs <- combn(n,2)
    for(i in 1:ncol(combs)){
      names_of_n <- c(1:n)
      minus <- names_of_n[!names_of_n %in% combs[,i]]
      p_2 <- p_2 + proba_n[combs[1,i]]*proba_n[combs[2,i]]*prod(sapply(proba_n[minus], function(x) 1-x))       
    }
    # final p
    p_a <- 1-p_0-p_1
    p_b <- 1-p_0-p_1-p_2
    p <- mean(c(p_a,p_b))
    rm(p_0, p_1, p_2, p_a, p_b)
    
  } else if (n==6){
    # method: p(X>=50%) is estimated with p(X>=3)
    # p(X=0)
    p_0 <- 1
    for(i in 1:n){p_0 <- p_0*(1-proba_n[i])}
    # p(X=1)
    p_1 <- 0
    for(i in 1:n){p_1 <- p_1 + proba_n[i]*prod(sapply(proba_n[-i], function(x) 1-x))}
    # p(X=2)
    p_2 <- 0
    combs <- combn(n,2)
    for(i in 1:ncol(combs)){
      names_of_n <- c(1:n)
      minus <- names_of_n[!names_of_n %in% combs[,i]]
      p_2 <- p_2 + proba_n[combs[1,i]]*proba_n[combs[2,i]]*prod(sapply(proba_n[minus], function(x) 1-x))       
    }
    # final p
    p <- 1-p_0-p_1-p_2
    rm(p_0, p_1, p_2)
  } 
  return(p)
}