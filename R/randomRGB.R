#' get random RGB code to get color
#'
#' @name RandomRGB
#' @author      Byungju Kim (bjk@inu.ac.kr)
#' @param x     # of random RGB codes to print
#' @param red   range of red color sampling [default=0:255]
#' @param green range of green color sampling [default=0:255]
#' @param blue  range of blue  color sampling [default=0:255]
#' @export
randomRGB <- function(x,r=0:255,g=0:255,b=0:255) {
  rgb<- function(r,g,b) {
   r  <- as.hexmode(sample(r,1))
   g  <- as.hexmode(sample(g,1))
   b  <- as.hexmode(sample(b,1))
   return(paste0("#",r,g,b,collpase=""))
  }
  out<-c()
  for(i in 1:x) {
    out<-c(out,rgb(r,g,b))
  }
  return(out)
}
