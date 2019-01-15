imputation <- function(data) {
  rbind(replace(data, !is.finite(data) , 0))
}