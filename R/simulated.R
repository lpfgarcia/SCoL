#' Simulated Complexity Measures
#'
#' The complexity measures are interested to quantify the the ambiguity of the 
#' classes, the sparsity and dimensionality of the data and the complexity of
#' the boundary separating the classes.
#'
#' @family meta-features
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param features A list of features names or \code{"all"} to include all them.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#'  The details section describes the valid values for this group.
#' @param ... Further arguments passed to the summarization functions.
#' @details
#'  The following features are allowed for this method:
#'  \describe{
#'    \item{"F2"}{}
#'    \item{"F3"}{}
#'    \item{"F4"}{}
#'    \item{"N1"}{}
#'    \item{"N3"}{}
#'    \item{"N4"}{}
#'    \item{"T1"}{}
#'    \item{"LSC"}{}
#'    \item{"L2"}{}
#'    \item{"L3"}{}
#'    \item{"Density"}{}
#'    \item{"ClsCoef"}{}
#'    \item{"Hubs"}{}
#'  }
#' @return A list named by the requested meta-features.
#'
#' @references
#'  Ana C. Lorena, Luis P. F. Garcia, Jens  Lehmann, Marcilio C. P. de Souto and
#'  Tin k. Ho. How Complex is your classification problem? A survey on measuring
#'  classification complexity. arXiv:1808.03591, 2018.
#'
#' @examples
#' ## Extract all complexity measures using formula
#' simulated(Species ~ ., iris)
#'
#' ## Extract some complexity measures
#' simulated(iris[1:4], iris[5], c("F2", "F3", "F4"))
#' @export
simulated <- function(...) {
  UseMethod("simulated")
}

#' @rdname simulated
#' @export
simulated.default <- function(x, y, features="all", ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }
  y <- as.factor(y)

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }
  
  if(features[1] == "all") {
    features <- ls.simulated()
  }

  features <- match.arg(features, ls.simulated(), TRUE)
  colnames(x) <- make.names(colnames(x))

  loadNamespace("randomForest")

  measures <- imputation(mfe::metafeatures(x, y))
  unlist(sapply(features, function(f) {
    as.numeric(stats::predict(get(f), measures))
  }, simplify=FALSE))
}

#' @rdname simulated
#' @export
simulated.formula <- function(formula, data, features="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  simulated.default(modFrame[, -1], modFrame[, 1], features, ...)
}

#' List the simulated simulated measures
#'
#' @return A list of the simulated simulated measures names.
#' @export
#'
#' @examples
#' ls.simulated()
ls.simulated <- function() {
  c("F2", "F3", "F4", "N1", "N3", "N4", "T1", "LSC", "L2", "L3", "Density", 
    "ClsCoef", "Hubs")
}

ls.simulated.multiples <- function() {
  ls.simulated()
}

imputation <- function(data) {
  rbind(replace(data, !is.finite(data) , 0))
}
