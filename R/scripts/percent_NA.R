percent_NA <- function(x, N) {
  if(missing(N)){
    result <- total_NA(x)/length(x) * 100
  } else {
    result <- total_NA(x)/N * 100
  }
  return(result)
}