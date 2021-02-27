name <- "Hoda Fakharzadehjahromy"
liuid <-  "hodfa840"

######1.1.1#######

my_num_vector <- function(){
  
  return(c(log(11,base = 10), cos(pi/5), (1173 %% 7)/19))
  
}

#######1.1.2#######

filter_my_vector <- function(x, leq){
  
  x[x>=leq] <- NA
  return(x)
}

#######1.1.3#######

dot_prod <- function(a, b){
  
  return(sum(a*b))
  
}

#######1.1.4#######

approx_e <- function(N) {
  
  x <- 0:N
  return( sum(1/(factorial(x))))
}


########Matrices#####

########1.2.1########


my_magic_matrix <- function() {
  
  return(matrix(c(4,3,8,9,5,1,2,7,6),nrow = 3,ncol = 3))
}

########1.2.2########

calculate_elements <- function(A) {
  
  return(length(A))
}

########1.2.3########

row_to_zero <- function(A,i) {
  
  A[i,] <- 0
  return(A)
}

########1.2.4########

add_elements_to_matrix <- function(A ,x ,i ,j) {
  
  A[i,j] <- A[i,j] + x
  return(A)
}

######List###########

######1.3.1#########

my_magic_list<- function() {
  
  return(list(info="my own list",my_num_vector(),my_magic_matrix()))
}

#####1.3.2##########

change_info <- function(x, text) {
  
  x$info <- text
  return(x)
}

#####1.3.3##########


add_note <- function(x, note) {
  x$note <- note
  return(x)
}

#####1.3.4##########

sum_numeric_parts <- function(x) {
  
  #unlist(lapply(x, function(x) if(!is.character(x)) return(sum(x))))
  return(sum(as.numeric(unlist(x,recursive = TRUE)),na.rm = TRUE))
}

##########Dataframe#

#####1.4.1##########

my_data_frame <- function() {
  
  return(data.frame(id = c(1,2,3),name=c("John", "Lisa", "Azra"),
                    income=c(7.3, 0, 15.21),rich=c(FALSE, FALSE, TRUE)))
}

#####1.4.2##########

sort_head <- function(df, var.name, n){
  
  return(df[order(df[,var.name],decreasing = TRUE)[1:n],])
 
}

#######1.4.3########


add_median_variable <- function(df,j) {
  
  m <- median(df[,j])
  compared_to_median <- rep(NA,nrow(df))
  compared_to_median[which(df[,j]>m)] <- "Greater"
  compared_to_median[which(df[,j]<m)] <- "Smaller"
  compared_to_median[which(df[,j]==m)] <- "Median"
  return(data.frame(df,compared_to_median=compared_to_median))
  }


#######1.4.4########

analyze_columns <- function(df,j){
  
  new.list <- list()
  
  analyze1 <- c(mean(df[,j[1]]),median(df[,j[1]]),sd(df[,j[1]]))
  names(analyze1) <- c('mean','median','sd')
  new.list[[names(df)[j[1]]  ]] <- analyze1
  
  analyze2 <- c(mean(df[,j[2]]),median(df[,j[2]]),sd(df[,j[2]]))
  names(analyze2) <- c('mean','median','sd')
  new.list[[names(df)[j[2]]  ]] <- analyze2
  
  new.list[['correlation_matrix']] = cor(df[,j],df[,j])
  
 
  return(new.list)
  
}






