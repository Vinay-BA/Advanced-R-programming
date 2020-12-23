library(markmyassignment)
set_assignment("https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab2.yml")

name <- "Vinay Bengaluru Ashwath Narayan Murthy"
liuid <- "vinbe289"

#1.1.1 sheldon game(player1, player2)
sheldon_game <- function(player1, player2)
{
  if (player1=='rock')
    p1 <- 1
  if (player1=='lizard')
    p1 <- 2
  if (player1=='spock')
    p1 <- 3
  if (player1=='scissors')
    p1 <- 4
  if (player1=='paper')
    p1 <- 5
  if (player2=='rock')
    p2 <- 1
  if (player2=='lizard')
    p2 <- 2
  if (player2=='spock')
    p2 <- 3
  if (player2=='scissors')
    p2 <- 4
  if (player2=='paper')
    p2 <- 5
  plyr_opts <- c('rock','lizard','spock','scissors','paper')
  stopifnot(player1 %in%  plyr_opts && player2 %in%  plyr_opts)
  if (p1==p2)
    return("Draw!")
  else if ((p1-p2)%% 2 ==0)
    return("Player 2 wins!")
  else 
    return("Player 1 wins!")
}
sheldon_game("lizard","spock")
sheldon_game("rock","paper")

#1.1.2 
my_moving_median <- function(x, n, ...)
{
  stopifnot (is.numeric(x) && is.numeric(n))
  len <- length(x)
  res_vec <- 0
  for (i in 1:(len-n))
  {
    new_vec <- x[i:(i+n)]
    res_vec[i] <- median(new_vec, ...)
  }
  return(res_vec)
}
my_moving_median(x=1:10, n=2)
my_moving_median(x = 5:15, n=4)
my_moving_median(x = c(5,1,2,NA,2,5,6,8,9,9), n=2)
my_moving_median(x = c(5,1,2,NA,2,5,6,8,9,9), n=2, na.rm=TRUE)

#1.2.2
for_mult_table <- function(from,to)
{
  if(is.numeric(from)==FALSE || is.numeric(to)==FALSE)
  {
    stop()
  }
  else
  {
    data_vec <- c(from:to)
    mul_mat <- data_vec %*% t(data_vec)
    colnames(mul_mat) <- c(from:to)
    rownames(mul_mat) <- c(from:to)
    return(mul_mat)
  }
}
for_mult_table(from=1,to=5)
for_mult_table(from=10,to=12)

#1.3.1
find_cumsum <- function(x,find_sum)
{
  stopifnot(is.numeric(x) && is.numeric(find_sum))
  y <- cumsum(x)
  i <- 1
  l <- length(x)
  while(y[i] < find_sum & i < l)
  {
    i = i+1
  }
  return(y[i])
}
find_cumsum(x=1:100,find_sum=500)
find_cumsum(x=1:10,find_sum=1000)

#1.3.2
while_mult_table <- function(from,to)
{
  if(is.numeric(from)==FALSE || is.numeric(to)==FALSE)
  {
    stop()
  }
  else
  {
    i <- 0
    while(i < from)
    {
      while(i < to)
      {
        data_vec <- c(from:to)
        mul_mat <- data_vec %*% t(data_vec)
        colnames(mul_mat) <- c(from:to)
        rownames(mul_mat) <- c(from:to)
        return(mul_mat)
      }
    }
    
  }
}
while_mult_table(from=3,to=5)
while_mult_table(from=7,to=12)

#1.4.1
repeat_find_cumsum <- function(x,find_sum)
{
    stopifnot(is.vector(x) || is.numeric(find_sum))
    i <- 1
    final_val <- 0
    repeat
    {  
      if(final_val < find_sum && length(x) >= i)
      {
        final_val <- final_val + x[i]
        i <- i + 1
      }
      else
      {
        break
      }
    }  
    return(final_val)
  }
repeat_find_cumsum(x=1:100, find_sum=500)
repeat_find_cumsum(x=1:10, find_sum=1000)

#1.4.2
repeat_my_moving_median <- function(x,n, ...)
{
  stopifnot (is.numeric(x) && is.numeric(n))
  len <- length(x)
  res_vec <- 0
  i <- 1
  repeat
  {
    new_vec <- x[i:(i+n)]
    res_vec[i] <- median(new_vec, ...)
    i <- i+1
    if(i==(len-n+1))
    {
      break
    }
  }
  return(res_vec)
}
repeat_my_moving_median(x = 1:10, n=2)
repeat_my_moving_median(x = 5:15, n=4)
repeat_my_moving_median(x = c(5,1,2,NA,2,5,6,8,9,9), n=2)

#1.5.1
in_environment <- function(env)
{
  return(ls(env))
}
env <- search()[length(search())] 
env
funs <- in_environment(env) 
funs[1:5]

#1.6.1
cov <- function(X)
{
  stopifnot(is.data.frame(X))
  res <- list()
  for(i in names(X))
  {
    res[i] <- lapply(X[i],function(i){sd(i)/mean(i)}) 
  }
  return(unlist(res))
}
data(iris)
cov(X = iris[1:4])
cov(X = iris[3:4])

#1.7.1
moment <- function(i)
{
  stopifnot(is.numeric(i))
  function(x) 
  {
    mean((x - mean(x)) ^ i) 
  }
}
m1 <- moment(i=1)
m2 <- moment(i=2)
m1(1:100)
m2(1:100)
