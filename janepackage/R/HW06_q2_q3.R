# question 2
#' side_length Function
#'
#' This function allows you to calculates the length of the third side according to the Pythagorean theorem, given the lengths of two sides of the triangle,
#' @param a: length of one side of a triangle.No default.
#' @param b: length of one side of a triangle.No default.
#' @keywords side_length
#' @export
#' @examples
#' side_length(3,4)
#'
side_length<-function(a,b){c <- sqrt((a^2+b^2))
if(a!=b){c_1 <- sqrt(abs(a^2-b^2))
print(c_1)}
print(c)}

#question3
#' trim_mean  Function
#'
#' This function allows you to calculates the mean of a numeric vector x, ignoring the s smallest and l largest values.
#' @param x: a numeric vector.
#' @param s: s smallest values.
#' @param l: l largest values.
#' @keywords trim_mean
#' @export
#' @examples
#' trim_mean(x,1,2)
#'
trim_mean <- function(x,s,l){x_sort<-x[order(x)]
if(length(x_sort)>=s+l+1){
  x_trim <- x_sort[order(x_sort)>s &order(x_sort) <= (length(x_sort)-l)]
  mean(x_trim)}
else if(length(x_sort)<s+l+1){
  stop("error for length of x ")
}}



