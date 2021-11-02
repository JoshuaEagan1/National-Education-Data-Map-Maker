#a function that extracts the last n characters from a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}