library(foreach)
library(parallel)
library(doParallel)
library(lmerTest)

#####
# generate list of combinations (only with fixed effect)
generate.FE.comb <- function(
    fixed.equation, must = NULL){
  # parse fixed.equation
  vars.fixed <- all.vars(fixed.equation)
  # get length
  num.X <- length(vars.fixed)-1
  # get Y and Xs
  var.Y <- vars.fixed[1]
  var.fixed <- vars.fixed[2:length(vars.fixed)]
  count <- 1
  result <- NULL
  
  # must should be included in var.fixed
  if (!is.null(must)){
    flag <- 0
    for (i in 1:length(must)){
      # check existance 
      flag <- must[i] %in% vars.fixed
    }
    # if non-existance, error
    if (flag == 0){
      return(2)
    }
  }
  # if no random slope
  #if (is.null(slopes)){
  # simple combination
  for (i in 1:num.X){
    combs <- combn(var.fixed,i)

    for (j in 1:ncol(combs)){
      # skip overlap between fixed and must
      if (!is.null(must)){
        flag <- 0
        for (i in 1:length(must)){
          flag <- must[i] %in% combs[,j]
          if (flag){
            break
          }
        }
        if (flag){
          next
        }
      }
      
      # start current combination
      current <- sprintf('%s ~',var.Y)
      for (k in 1:nrow(combs)){
        # do combination
        if (k == 1){
          # first one without +
          current <- sprintf('%s %s',current,combs[k,j])}
        else{
          # add following terms with +
          current <- sprintf('%s + %s',current,combs[k,j])
        }
      }
      # if there are must! items?
      if (!is.null(must)){
        for (k in 1:length(must)){
          current <- sprintf('%s + %s',current,must[k])
        }
      }
      # store current one
      result[count] <- current
      count <- count + 1
    }
  }
  # null model
  if (is.null(must)){
    result[count] <- sprintf('%s ~ 1',var.Y)
  }else{
    for (k in 1:length(must)){
      if (k == 1){
        current <- sprintf('%s ~ %s',var.Y,must[k])
      }else{
        current <- sprintf('%s + %s',current,must[k])
      }
    }
    result[count] <- current
  }
  all.count <- count
  count <- count + 1

  # return result
  return(as.array(result))

}

#####
# generate list of combinations with random intercepts
generate.RI.comb <- function(
    fixed.equation,group,must=NULL){
  # get fixed effect model
  result <- generate.FE.comb(fixed.equation,must)
  
  # if any error, then return error
  if (!class(result)=='array'){
    return(result)
  }
  
  count <- length(result)
  
  all.count <- count
  count <- count + 1
  
  # add random intercepts
  for (i in 1:all.count){
    result[count] <- sprintf('%s + (1|%s)',result[i],group)
    count <- count + 1
  }
  
  # return result
  return(as.array(result))
}

#####
# generate list of all combinations with random slopes (full model)
generate.RS.comb <- function(
  fixed.equation, group, slopes = NULL, must=NULL){
  
  # create RI model
  result <- generate.RI.comb(fixed.equation, group,must)
  # if error -> return error
  if (!class(result)=='array'){
    return(result)
  }
  
  # parse fixed.equation
  vars.fixed <- all.vars(fixed.equation)
  # get length
  num.X <- length(vars.fixed)-1
  # get Y and Xs
  var.Y <- vars.fixed[1]
  var.fixed <- vars.fixed[2:length(vars.fixed)]
  #temp <- var.fixed
  
  # merge with must
  #var.fixed <- c(var.fixed,must)
  #var.fixed <- temp
  
  # random slopes
  flag <- 0
  
  if (!is.null(slopes)){
    num.random <- length(slopes)
    # check whether all random slope terms were included in the fixed effects
    exist <- 0
    for (i in 1:num.random){
      #for (j in 1:(num.X+length(must))){
      for (j in 1:(num.X)){
        # exist?
        if (slopes[i] == var.fixed[j]){
          exist <- 1
        }
      }
      # 
      # survived current one?
      if (exist == 0){
        # flag it
        flag <- 1
        break
      }
      # excessive slope?
      if (! (slopes[i] %in% var.fixed)){
        flag <- 1
      }
    }
    # check successful
    # from now on, do combination
  }
  # if flaged
  if (flag){
    return(flag)
  }
  
  # if all random slopes are included in the fixed effects, then proceed
  
  result.rs <- NULL
  count <- 1
  for (i in 1:num.X){
    combs <- combn(var.fixed,i)
    for (j in 1:ncol(combs)){
      # start current combination
      must.flag <- 0
      current <- sprintf('%s ~',var.Y)
      for (k in 1:nrow(combs)){
        # do combination
        if (k == 1){
          # first one without +
          current <- sprintf('%s %s',current,combs[k,j])}
        else{
          # add following terms with +
          current <- sprintf('%s + %s',current,combs[k,j])
        }
      }
      # check whether all musts are in
      if (!is.null(must)){
        for (l in 1:length(must)){
          # does not exist
          if ( (must[l] %in% combs[,j]) ==0){
            must.flag <-1
          }
        }
      }
      # does not exist <- stop
      if (must.flag){
        next
      }
      
      # at the end, add random slopes
      
      for (k in 1:num.random){

                # if the current slope number exceeds the fixed ef num, skip
        if (k > nrow(combs)){
          next
        }
          
        # combination for the current number of slope
        combs.rs <- combn(slopes,k)
        for (m in 1:ncol(combs.rs)){
          current.res <- current
          added.res <- 0
          
          exist.flag <- 0
          for (n in 1:nrow(combs.rs)){
          # non-existance of a certain slope in fix -> skip
            if ( !(combs.rs[n,m] %in% combs[,j])){
              exist.flag <- 1
            }
           
          }
          if (exist.flag){
            next
          }
          
          for (n in 1:nrow(combs.rs)){
 
            # add  
            # first
            if (added.res == 0){
              current.res <- sprintf('%s + (1 + %s',current.res,combs.rs[n,m])
            }else{
              # not first
              current.res <- sprintf('%s + %s',current.res,combs.rs[n,m])
            }
            added.res <- added.res + 1
          }
          #close
          if (added.res > 0){
            current.res <- sprintf('%s|%s)',current.res,group)
            result.rs[count] <- current.res
            count <- count + 1
          }
          
        }

    
   }
    }
  }
  # merge two results
  result.final <- c(result,result.rs)
  # return result 
  return(result.final)
}

explore.models <- function(data,fixed.equation, group = NULL, slopes = NULL,
                           must= NULL,
                           cores=1
){
  # create threads to distribute tasks
  cl <- parallel::makeCluster(cores,type='FORK')
  doParallel::registerDoParallel(cl)
  
  # generate combinations depending on the presence of RI/RS
  # only with fixed effects?
  if (is.null(group)){
    model <- generate.FE.comb(fixed.equation,must)
  }else{
    # only with RI?
    if (is.null(slopes)){
      model <- generate.RI.comb(fixed.equation,group,must)
    }else{
      # RS model
      model <- generate.RS.comb(fixed.equation,group,slopes,must)
    }
  }
  
  # how many models?
  num.model <- length(model)
  # create result arrays
  bics <- matrix(nrow=num.model)
  aics <- matrix(nrow=num.model)
  lrs <-matrix(nrow=num.model)
  # loop
  now <- foreach (i = seq(1,num.model)) %dopar% {
  
    # is current one with random effect?
    # no -> lm
    if (!grepl(')',model[i])){
      current.test <- lm(formula(model[i]),data)
    }else{
      # lmer
      current.test <- lmer(formula(model[i]),data)
    }
    # result extraction
    aic <- AIC(current.test)
    bic <-BIC (current.test)
    lr<-logLik(current.test)
    current.result <- data.frame(model[i],lr,aic,bic)
  }
  
  # merge results
  result <- NULL
  for (i in 1:num.model){
    result <- rbind(result,now[[i]])
  }

  colnames(result) <- c('Model','logLR','AIC','BIC')
  
  
  # stop cluster
  stopCluster(cl)
  
  return(result)
}

sort.result <- function(result, by='BIC'){
  # sort result 
  if (by == 'BIC'){
    # by BIC
    result <- result[order(result$BIC,decreasing = F),]
  }
  if (by == 'AIC'){
    result <- result[order(result$AIC,decreasing = F),]
  }
  if (by == 'LL'){
    result <- result[order(result$logLR,decreasing = T),]
  }
  #return
  return(result)
}
