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
d <<- data.frame(matrix(c(NA,NA),nrow = 1))
colnames(d) = c("id","val")

# Optimal time recurring function
F = function(N,c,d)
{
  # check for stopping conditions first
  if(N == 0 || c >= N) return(0)
  if(c == 0) return(N*20)
  
  if(paste0(N-1,'-',c-1) %in% d$id){
    part1 = as.numeric(d$val[which(d$id == paste0(N-1,'-',c-1))])
  } else {part1 = F(N - 1, c - 1, d)}
  
  if(paste0(N-1,'-',c) %in% d$id){
    part2 = as.numeric(d$val[which(d$id == paste0(N-1,'-',c))])
  } else {part2 = F(N - 1, c, d)}

  obj = function(x) 
  {
    (0.5 - x/160)*part1 + x^2/320 + (0.5 + x/160)*part2
  }
  res = optimize(obj, c(0,80))
  print(paste('For',N,'signals and',c,'charges, Calvin decides at level',res$minimum,'and waits',res$objective))
  if(!(paste0(N,'-',c) %in% d$id)) {d <<- rbind(d, c(paste0(N,'-',c),res$objective))}
  return(res$objective)
}
F(0,0,d)
d
F(1,0,d)
d
F(1,1,d)
d
F(2,0,d)
d
F(2,1,d)
d
F(2,2,d)
d
F(3,0,d)
d
F(3,1,d)
d
F(3,2,d)
d
F(3,3,d)
d
F(4,0,d)
d
F(4,1,d)
d
F(4,2,d)
d
F(4,3,d)
d
F(4,4,d)
d
F(5,3,d)
d
F(6,4,d)
d
F(10,5,d)
d
