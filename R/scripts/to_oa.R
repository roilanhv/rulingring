## Function to relace a "DateTime" named column to "date" named column
to_oa <- function(x){
  # x<- nee_hh
  if("date" %in% names(x)) return(x)
  if(all(c("Year", "DoY", "Hour") %in% names(x))){
     y <- select(x, sel = -(Year:Hour))
  }
  if("DateTime" %in% names(x)) {
     y <- rename(y, date = DateTime)
     return(y)
  } else {
    stop("There isn't a DateTime named column in the dataset")
  }
}# end to_oa
