vect <- c(6 ,7 ,8 ,9)
vect [vect >7]
vect [which(vect >7)]
vect [1:2]
vect [c (1 ,2)]
vect [c( -1 , -2)]
lst <- list(a=47, b=11)
lst[[1]]
lst$b
x <-"a"
lst[which(names(lst) == x)]
lst[[which(names(lst) == x)]]

#-----------------------------------------------------

#assigning subsets

mat <- matrix(data = 1:6,2,3 )
mat [mat > 4] = 75

#------------------------------------------------------

#Functions

my_function_name <- function (x, y){
  z <- x^2 + y^2
  return (z)
}

#Unlike in many languages, return in R is a function. In other 
# languages, return is usually a reserved word (like if).By default R returns the last computed value of the function,

#--------------------------------------------------------

# help()

help('+')
