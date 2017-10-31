################################################################################################################################
####################################################         Recursive Approach         ########################################
################################################################################################################################
# Optimal  time  recurring  function
F = function(N,c)
{
  # check  for  stopping  conditions  first
  if(N == 0 || c >= N) return(0)
  if(c == 0) return(N*20)
  # recursive  part
  part1 = F(N - 1, c - 1)
  part2 = F(N - 1, c)
  obj = function(x){(0.5 - x/160)*part1 + x^2/320 + (0.5 + x/160)*part2}
  # finding  the  minimum
  res = optimize(obj , c(0,80))
  print(paste('For',N,'signals and',c,'charges, Calvin decides at level',res$minimum,'and waits',res$objective))
  return(res$objective)
}

################################################################################################################################
#############################################        Dynamic Programming Approach        #######################################
################################################################################################################################
# dictionnary used as the memoizing data structure (a matrix)
d <- new.env()
d$id <- NULL
d$val <- NULL

# Optimal time recurring function
F = function(N,c,d)
{
  # check for stopping conditions first
  if(N == 0 || c >= N) return(0)
  if(c == 0) return(N*20)
  
  # check if terms we need are in the dictionary already
  if(paste0(N-1,'-',c-1) %in% d$id){
    part1 = as.numeric(d$val[which(d$id == paste0(N-1,'-',c-1))])
  } else {part1 = F(N - 1, c - 1, d)}
  
  if(paste0(N-1,'-',c) %in% d$id){
    part2 = as.numeric(d$val[which(d$id == paste0(N-1,'-',c))])
  } else {part2 = F(N - 1, c, d)}
  
  # objective function we want to minimise
  obj = function(x) { (0.5 - x/160)*part1 + x^2/320 + (0.5 + x/160)*part2 }
  
  # minimisation (assume it will always be a convex problem witha unique minimum)
  res = optimize(obj, c(0,80))
  print(paste('For',N,'signals and',c,'charges, Calvin decides at level',res$minimum,'and waits',res$objective))
  
  # add the term in the dictionary if not already in it (only useful when calling F with the same parameters in fact)
  if(!(paste0(N,'-',c) %in% d$id)) 
  {
    d$id <- c(d$id, paste0(N,'-',c))
    d$val <- c(d$val,res$objective)
  }
  
  return(res$objective)
}

F(100,50,d)
dmat <- matrix(c(d$id,as.numeric(d$val)),ncol = 2)
