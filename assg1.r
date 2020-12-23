library(markmyassignment)
set_assignment("https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml")

name<-"Vinay Bengaluru Ashwath Narayan Murthy "
liuid<-"vinbe289"

#1.1.1
my_num_vector <- function()
{
  c(log10(11),cos(pi/5),exp(pi/3),(1173 %% 7)/19)
}
my_num_vector()

#1.1.2
filter_my_vector <- function(x,leq)
{
  x[x>=leq]= NA
  return(x)
}
filter_my_vector(x = c(2, 9, 2, 4, 102), leq = 4)

#1.1.3
dot_prod= function(a,b)
{
  sum(a*b)
}
dot_prod(a=c(3,1,12,2,4),b=c(1,2,3,4,5))
dot_prod(a=c(3,-1),b=c(-1,-3))

#1.1.4
approx_e<- function(N)
{
  n=0:N
  e=sum(1/factorial(n))
  return(e)
}
approx_e(4)

#1.2.1
my_magic_matrix <- function()
{
  m=matrix(c(4,9,2,3,5,7,8,1,6),nrow = 3,ncol = 3,byrow = TRUE)
  m
}
my_magic_matrix()

#1.2.2
calculate_elements <- function(A)
{
  dim1=dim(A)
  cnt1=prod(dim1)
  cnt1
}
mat<- my_magic_matrix()
calculate_elements(mat)
new_mat<-cbind(mat,mat)
calculate_elements(new_mat)

#1.2.3
row_to_zero <- function(A,i)
{
  A[i,] <- 0
  return(A)
}
mat <- my_magic_matrix()
row_to_zero(A=mat,i=1)

#1.2.4
add_elements_to_matrix<-function(A,x,i,j){
  A[i,j]<-A[i,j]+x
  return(A[])
}

mat <- my_magic_matrix()
add_elements_to_matrix(A=mat,x=10,i=2,j=3)
add_elements_to_matrix(A=mat,x=-2,i=1:3,j=2:3)

#1.3.1
my_magic_list <- function()
{
  lst=list(info="my own list",my_num_vector(),my_magic_matrix())
  return(lst)
}
my_magic_list()

#1.3.2
change_info <- function(x, text)
{
  x$info <- text
  return(x)
}
a_list <- my_magic_list()
change_info(x= a_list, text="Some new info")

#1.3.3
add_note <- function(x,note)
{
  x$note <- note
  return(x)
}
a_list <- my_magic_list()
add_note(x = a_list, note = "This is a magic list!")

#1.3.4
sum_numeric_parts <- function(x)
{
    y <- as.numeric(unlist(x))
    vb <- sum(y, na.rm = TRUE)             
    return(vb)
}
a_list <- my_magic_list()
sum_numeric_parts(x=a_list)
sum_numeric_parts(x=a_list[2])

#1.4.1
my_data.frame <- function()
{
  x=data.frame(id = 1:3, name = c("John","Lisa","Azra"),income = c(7.30,0.00,15.21), rich = c(FALSE,FALSE,TRUE))
  return(x)
}
my_data.frame()

#1.4.2
sort_head <- function(df,var.name,n)
{
  head(df[order(df[,var.name],decreasing = TRUE),],n)
}
data(iris) 
sort_head(df = iris, var.name = "Petal.Length", n = 5)

#1.4.3
add_median_variable <- function(df,j){
  compared_to_median <-NA
  xyz <- median(df[,j])
  for(k in 1:nrow(df))
    {
              if(df[k,j]<xyz)
                  compared_to_median[k] <- "SMALLER"
              else if(df[k,j]>xyz)
                  compared_to_median[k] <- "GREATER"
              else if(df[k,j]==xyz)
                  compared_to_median[k] <- "MEDIAN"
              }
  return(cbind(df,compared_to_median))
}

data(faithful)
head(add_median_variable(df = faithful, 1))
tail(add_median_variable(df = faithful, 2))

#1.4.4
analyze_columns <- function(df,j)
{
  value1 <- j[1]
  value2 <- j[2]
  first <- as.numeric(unlist(df[value1]))
  second <- as.numeric(unlist(df[value2]))
  
  final_list <- list( a <- c(mean(first),median(first),sd(first)),
                      b <- c(mean(second),median(second),sd(second)),
                      c <- cor(subset(df,select=j)))
  
  names(final_list[[1]]) <- c("mean","median","sd")
  names(final_list[[2]]) <- c("mean","median","sd")
  names(final_list)[1:3] <- c(names(df[value1]),names(df[value2]),"correlation_matrix")
  
  return(final_list)
}

data(faithful)
analyze_columns(df = faithful, 1:2)
data(iris)
analyze_columns(df = iris, c(1,3))
analyze_columns(df = iris, c(4,1))

mark_my_assignment()
