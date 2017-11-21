remove_missing <- function(a) {
  #returns the input vector without missing values
 if (class(a) == 'numeric') {return (a[!is.na(a)])}
 else {return ('non-numeric argument')}
}


get_minimum <- function(a,na.rm = FALSE) {
  #returns the minimum value in a numeric vector
      sort(remove_missing(a))[1]
}

get_maximum <- function(a,na.rm = FALSE) {
  #returns the maximum value in a numeric vector
   sort(remove_missing(a),decreasing = TRUE)[1]
}


get_range <- function(a, na.rm = FALSE) {
  #returns the range of a numeric vector
  if(class(a) != 'numeric') {return ('non-numeric argument')}
 {get_maximum(remove_missing(a)) - get_minimum(remove_missing(a))}
}


get_percentile10 <- function(a,na.rm = FALSE){
  #returns the 10th percentile of the input vector
  if(class(a) != 'numeric') {return ('non-numeric argument')}
  {return (quantile(remove_missing(a),0.1)[[1]])}
}


get_percentile90 <- function(a,na.rm = FALSE){
  #returns the 90th percentile of the input vector
  if(class(a) != 'numeric') {return ('non-numeric argument')}
  return (quantile(remove_missing(a),0.9)[[1]])
}


get_median <- function(a, na.rm = FALSE){
  #returns the median of a numeric vector
  a <- sort(remove_missing(a))
  if(length((a)) %% 2 != 0) {
    return (a[(length(a)+1)/2])
  } else{
    return ((a[length(a)/2] + a[(length(a)/2+1)])/2)
  }
}


get_average <- function(a, na.rm = FALSE){
  #returns the average of a numeric vector
  if(class(a) != 'numeric') {return ('non-numeric argument')}
  {a <- remove_missing(a)
  total <- 0
  for (i in a){total <- total + i}
  return (total/length(a))}
}


get_stdev <- function(a,na.rm = FALSE){
  #returns the standard deviation of a numeric vector
  if(class(a) != 'numeric') {return ('non-numeric argument')}
  {a <- remove_missing(a)
  total <- 0
  avg <- get_average(a)
  for (i in a){total <- total + (i-avg)^2}
  return (sqrt(total/(length(a)-1)))}
}


get_quartile1 <- function(a,na.rm = FALSE){
  #returns the first quartile of the input vector
  if(class(a) != 'numeric') {return ('non-numeric argument')}
  {a <- remove_missing(a)
  return (quantile(a)[[2]])}
}


get_quartile3 <- function(a,na.rm = FALSE){
  #returns the third quartile of the input vector
  if(class(a) != 'numeric') {return ('non-numeric argument')}
  {a <- remove_missing(a)
  return (quantile(a)[[4]])}
}


count_missing <- function(a) {
  #calculates the number of missing values NA in a input vector
  if (class(a) != "numeric") {return ('non-numeric argument')}
  sum(is.na(a))
}


summary_stats <- function(a){
  summary <- list(get_minimum(a),get_percentile10(a),get_quartile1(a),
                  get_median(a),get_average(a),get_quartile3(a),get_percentile90(a),
                  get_maximum(a),get_range(a),get_stdev(a),count_missing(a))
  names(summary) <- c("minimum","percent10","quartile1","median","mean","quartile3",
                     "percent90","maximum","range","stdev","missing")
  return (summary)
}



print_stats <- function(a) {
  name <- names(summary_stats(a))
  for (i in 1:length(name)){
    difference <- 9 - nchar(name[i])
    space <- paste(rep(" ",difference),collapse = "")
    cat(paste0(name[i],space,": ",
              format(round(summary_stats(a)[[i]],3),nsmall = 4),'\n'))}
}


rescale100 <- function(x,xmin,xmax) {
  return(100*(x-xmin)/(xmax-xmin)) 
}



drop_lowest <- function(x){
  min <- min(x)
  i <- 1
  while (i <= length(x)){
    if (x[i] == min){
      return (x[-i])
    }
    else{i <- i + 1}
  }
  return (x)
  }


score_homework <- function(hw, drop = FALSE) {
  if (drop == TRUE){
    return (get_average(drop_lowest(hw)))}
  else {return (get_average(hw))}
}



score_quiz <- function(quiz,drop = FALSE){
  if (drop == TRUE){
    return (get_average(drop_lowest(quiz)))}
  else {return (get_average(quiz))}
}


score_lab <- function(attd) {
  if (attd <= 6) {return (0)}
  if (attd == 7) {return (20)}
  if (attd == 8) {return (40)}
  if (attd == 9) {return (60)}
  if (attd == 10) {return (80)}
  else {return (100)}
}

