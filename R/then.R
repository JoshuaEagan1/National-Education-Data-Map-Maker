#a function to act as a 'pipe operator' for shiny validation statements, allowing
#need functions tot be evaluated sequentially instead of simultaneously 
`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}