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
#'  Tin K Ho and Mitra Basu. Complexity measures of supervised classification 
#'  problems. IEEE Transactions on Pattern Analysis and Machine Intelligence, 
#'  24, 3, 289--300, 2002.
#'
#'  Albert Orriols-Puig, Nuria Macia and Tin K Ho. Documentation for the data 
#'  complexity library in C++. Technical Report. La Salle - Universitat Ramon 
#'  Llull, 2010.
#'
#' @examples
#' ## Extract all complexity measures using formula
#' complexity(Species ~ ., iris)
#'
#' ## Extract some complexity measures
#' complexity(iris[1:4], iris[5], c("F2", "F3", "F4"))
#' @export
complexity <- function(...) {
  UseMethod("complexity")
}

#' @rdname complexity
#' @export
complexity.default <- function(x, y, features="all", ...) {
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
    features <- ls.complexity()
  }

  features <- match.arg(features, ls.complexity(), TRUE)
  colnames(x) <- make.names(colnames(x))

  loadNamespace("randomForest")

  measures <- imputation(mfe::metafeatures(x, y))
  sapply(features, function(f) {
    fn <- paste("m", f, sep=".")
    as.numeric(eval(call(fn, x=measures)))
  }, simplify=FALSE)
}

#' @rdname complexity
#' @export
complexity.formula <- function(formula, data, features="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  complexity.default(modFrame[, -1], modFrame[, 1], features, ...)
}

#' List the simulated complexity measures
#'
#' @return A list of the simulated complexity measures names.
#' @export
#'
#' @examples
#' ls.complexity()
ls.complexity <- function() {
  c("F2", "F3", "F4", "N1", "N3", "N4", "T1", "LSC", "L2", "L3", "Density", 
    "ClsCoef", "Hubs")
}

ls.complexity.multiples <- function() {
  ls.complexity()
}

m.F2 <- function(x, ...) {
   stats::predict(F2, x)
}

m.F3 <- function(x, ...) {
  stats::predict(F3, x)
}

m.F4 <- function(x, ...) {
   stats::predict(F4, x)
}

m.N1 <- function(x, ...) {
   stats::predict(N1, x)
}

m.N3 <- function(x, ...) {
   stats::predict(N3, x)
}

m.N4 <- function(x, ...) {
   stats::predict(N4, x)
}

m.T1 <- function(x, ...) {
   stats::predict(T1, x)
}

m.LSC <- function(x, ...) {
   stats::predict(LSC, x)
}

m.L2 <- function(x, ...) {
   stats::predict(L2, x)
}

m.L3 <- function(x, ...) {
   stats::predict(L3, x)
}

m.Density <- function(x, ...) {
   stats::predict(Density, x)
}

m.ClsCoef <- function(x, ...) {
  stats::predict(ClsCoef, x)
}

m.Hubs <- function(x, ...) {
   stats::predict(Hubs, x)
}
