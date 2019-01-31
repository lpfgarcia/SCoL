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
#'    \item{"F1"}{Maximum Fisher's Discriminant Ratio (F1) measures the overlap 
#'      between the values of the features and takes the value of the largest 
#'      discriminant ratio among all the available features.}
#'    \item{"F1v"}{Directional-vector maximum Fisher's discriminant ratio (F1v)
#'      complements F1 by searching for a vector able to separate two classes 
#'      after the training examples have been projected into it.}
#'    \item{"F2"}{Volume of the overlapping region (F2) computes the overlap of 
#'      the distributions of the features values within the classes. F2 can be 
#'      determined by finding, for each feature its minimum and maximum values 
#'      in the classes.}
#'    \item{"F3"}{The maximum individual feature efficiency (F3) of each 
#'      feature is given by the ratio between the number of examples that are 
#'      not in the overlapping region of two classes and the total number of 
#'      examples. This measure returns the maximum of the values found among 
#'      the input features.}
#'    \item{"F4"}{Collective feature efficiency (F4) get an overview on how 
#'      various features may work together in data separation. First the most 
#'      discriminative feature according to F3 is selected and all examples that
#'      can be separated by this feature are removed from the dataset. The 
#'      previous step is repeated on the remaining dataset until all the 
#'      features have been considered or no example remains. F4 returns the 
#'      ratio of examples that have been discriminated.}
#'    \item{"N1"}{Fraction of borderline points (N1) computes the percentage of 
#'      vertexes incident to edges connecting examples of opposite classes in 
#'      a Minimum Spanning Tree (MST).}
#'    \item{"N2"}{Ratio of intra/extra class nearest neighbor distance (N2)  
#'      computes the ratio of two sums: intra-class and inter-class. The former 
#'      corresponds to the sum of the distances between each example and its 
#'      closest neighbor from the same class. The later is the sum of the 
#'      distances between each example and its closest neighbor from another 
#'      class (nearest enemy).}
#'    \item{"N3"}{Error rate of the nearest neighbor (N3) classifier corresponds
#'      to the error rate of a one Nearest Neighbor (1NN) classifier, estimated 
#'      using a leave-one-out procedure in dataset.}
#'    \item{"N4"}{Non-linearity of the nearest neighbor classifier (N4) creates 
#'      a new dataset randomly interpolating pairs of training examples of the 
#'      same class and then induce a the 1NN classifier on the original data and
#'      measure the error rate in the new data points.}
#'    \item{"T1"}{Fraction of hyperspheres covering data (T1) builds 
#'      hyperspheres centered at each one of the training examples, which have 
#'      their radios growth until the hypersphere reaches an example of another 
#'      class. Afterwards, smaller hyperspheres contained in larger hyperspheres 
#'      are eliminated. T1 is finally defined as the ratio between the number of 
#'      the remaining hyperspheres and the total number of examples in the 
#'      dataset.}
#'    \item{"LSC"}{Local Set Average Cardinality (LSC) is based on Local Set 
#'      (LS) and defined as the set of points from the dataset whose distance of
#'      each example is smaller than the distance from the exemples of the 
#'      different class. LSC is the average of the LS.}
#'    \item{"L1"}{Sum of the error distance by linear programming (L1) computes 
#'      the sum of the distances of incorrectly classified examples to a linear 
#'      boundary used in their classification.}
#'    \item{"L2"}{Error rate of linear classifier (L2) computes the error rate 
#'      of the linear SVM classifier induced from dataset.}
#'    \item{"L3"}{Non-linearity of a linear classifier (L3) creates a new 
#'      dataset randomly interpolating pairs of training examples of the same 
#'      class and then induce a linear SVM on the original data and measure 
#'      the error rate in the new data points.}
#'    \item{"Density"}{Average Density of the network (Density) represents the 
#'      number of edges in the graph, divided by the maximum number of edges 
#'      between pairs of data points.}
#'    \item{"ClsCoef"}{Clustering coefficient (ClsCoef) averages the clustering 
#'      tendency of the vertexes by the ratio of existent edges between its 
#'      neighbors and the total number of edges that could possibly exist 
#'      between them.}
#'    \item{"Hubs"}{Hubs score (Hubs) is given by the number of connections it  
#'      has to other nodes, weighted by the number of connections these 
#'      neighbors have.}
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
  c("F1", "F1v", "F2", "F3", "F4", "N1", "N2", "N3", "N4", "T1", "LSC", "L1", 
    "L2", "L3", "Density", "ClsCoef", "Hubs")
}

ls.simulated.multiples <- function() {
  ls.simulated()
}

imputation <- function(data) {
  rbind(replace(data, !is.finite(data) , 0))
}
